unit TBXThemes;

// TBX Package
// Copyright 2001-2004 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// Id: TBXThemes.pas 16 2004-05-26 02:02:55Z Alex@ZEISS

interface

{$I TB2Ver.inc}

uses
  Windows, Messages, Classes, Forms, Graphics, ImgList, Controls, TB2Item;

{ TBX_SYSCOMMAND message }
const
  TBX_SYSCOMMAND = WM_USER + 312;
  TSC_BEFOREVIEWCHANGE = 1;
  TSC_VIEWCHANGE       = 2;
  TSC_AFTERVIEWCHANGE  = 3;
  TSC_APPACTIVATE      = 4;
  TSC_APPDEACTIVATE    = 5;

{ Integer Metric IDs }
const
  TMI_SPLITBTN_ARROWWIDTH    = 10;
  TMI_DROPDOWN_ARROWWIDTH    = 20;
  TMI_DROPDOWN_ARROWMARGIN   = 21;
  TMI_MENU_IMGTEXTSPACE      = 32;
  TMI_MENU_LCAPTIONMARGIN    = 33;
  TMI_MENU_RCAPTIONMARGIN    = 34;
  TMI_MENU_SEPARATORSIZE     = 35;
  TMI_TLBR_SEPARATORSIZE     = 50;
  TMI_EDIT_FRAMEWIDTH        = 60;
  TMI_EDIT_TEXTMARGINHORZ    = 61;
  TMI_EDIT_TEXTMARGINVERT    = 62;
  TMI_EDIT_BTNWIDTH          = 65;
  TMI_EDIT_MENURIGHTINDENT   = 66;

{ Boolean Metric IDs }
const
  TMB_OFFICEXPPOPUPALIGNMENT = 1;
  TMB_EDITMENUFULLSELECT     = 3; // highlights the full edit item
  TMB_EDITHEIGHTEVEN         = 4; // forces the height of the edit item to be even number (otherwise it will be odd)
  TMB_PAINTDOCKBACKGROUND    = 5; // docks are painted by the theme instead of having a uniform color
  TMB_SOLIDTOOLBARNCAREA     = 6; // no transparency in NC area of toolbars

{ Margins Metric IDs}
const
  MID_TOOLBARITEM            = 1;
  MID_MENUITEM               = 2;
  MID_STATUSPANE             = 3;

{ View types }
const
  VT_UNKNOWN                 = $0;
  VT_TOOLBAR                 = $01000;
  VT_POPUP                   = $02000;
  VT_DOCKPANEL               = $04000;
  VT_DOCKWINDOW              = $08000;
  VT_STATUSBAR               = $10000; // technicaly, this is not a view
  VT_SECTIONHEADER           = $20000; // for retrieving colors only

  { Toolbar view types }
  TVT_FLOATING               = $800;
  TVT_RESIZABLE              = $400;  // valid only when floating
  TVT_EMBEDDED               = $200;  // when the toolbar is not floating or docked
  TVT_NORMALTOOLBAR          = VT_TOOLBAR or $01;
  TVT_MENUBAR                = VT_TOOLBAR or $02;
  TVT_TOOLWINDOW             = VT_TOOLBAR or $04;

  { Popup view types }
  PVT_POPUPMENU              = VT_POPUP or $01;
  PVT_LISTBOX                = VT_POPUP or $02;
  PVT_TOOLBOX                = VT_POPUP or $04;
  PVT_CHEVRONMENU            = VT_POPUP or $08;

  { Dockable panel view types }
  DPVT_FLOATING              = $800;
  DPVT_RESIZABLE             = $400;
  DPVT_NORMAL                = VT_DOCKPANEL or $01;

  { Dockable window view types }
  DWVT_FLOATING              = $800;
  DWVT_RESIZABLE             = $400;
  DWVT_NORMAL                = VT_DOCKWINDOW or $01;

{ Item types }
const
  IT_TOOLBARBUTTON           = 1;
  IT_MENUITEM                = 2;

{ Item options (bit flags) }
const
  IO_TOOLBARSTYLE            = $01;
  IO_SUBMENUITEM             = $04;
  IO_COMBO                   = $08;
  IO_DESIGNING               = $10;
  IO_APPACTIVE               = $20;  // True when Application.Active = True
  IO_RADIO                   = $40;

{ Drag handle styles }
const
  DHS_DOUBLE                 = 0;
  DHS_NONE                   = 1;
  DHS_SINGLE                 = 2;

{ Caption/drag handle button states (bit flags) }
const
  CDBS_VISIBLE               = $1;
  CDBS_HOT                   = $2;
  CDBS_PRESSED               = $4;

{ Window info's RedrawPart (bit flags) }
const
  WRP_BORDER                 = $1;
  WRP_CAPTION                = $2;
  WRP_CLOSEBTN               = $4;

{ Popup shadow types }
const
  PST_NONE                   = 0;     // no popup shadows
  PST_WINDOWSXP              = 1;
  PST_OFFICEXP               = 2;
  PST_WINDOWS2K              = 3;

{ Edit (ComboBox) button types }
const
  EBT_DROPDOWN               = 1;
  EBT_SPIN                   = 2;

  { Edit (ComboBox) button states for EBT_DROPDOWN type (bit flags) }
  EBDS_DISABLED              = $1;
  EBDS_HOT                   = $2;
  EBDS_PRESSED               = $4;

  { Edit (ComboBox) button states for EBT_SPIN type (bit flags) }
  EBSS_DISABLED              = $1;
  EBSS_HOT                   = $2;
  EBSS_UP                    = $4;
  EBSS_DOWN                  = $8;

{ Page scroll button types }
const
  PSBT_UP                    = 1;
  PSBT_DOWN                  = 2;
  PSBT_LEFT                  = 3;
  PSBT_RIGHT                 = 4;

{ Item state flags }
const
  ISF_DISABLED               = $001;
  ISF_HOT                    = $002;
  ISF_PUSHED                 = $004;
  ISF_SELECTED               = $008;

  ISF_LOCATIONMASK           = $F00;
  ISF_TOOLBARCOLOR           = $000;  // for text and images painted in toolbars
  ISF_MENUCOLOR              = $100;  // for text and images painted in popups and DkPanels
  ISF_STATUSCOLOR            = $200;  // for text and images painted in status bars

{ StatusBar parts }
const
  SBP_BODY                   = 0;
  SBP_PANE                   = 1;
  SBP_LASTPANE               = 2;
  SBP_GRIPPER                = 3;

{ Dock positions }
const
  DP_TOP                     = 1;
  DP_BOTTOM                  = 2;
  DP_LEFT                    = 3;
  DP_RIGHT                   = 4;

type
  TTBXItemLayout = (tbxlAuto, tbxlGlyphLeft, tbxlGlyphTop);
  TTBXMargins = record
    LeftWidth: Integer;
    RightWidth: Integer;
    TopHeight: Integer;
    BottomHeight: Integer;
  end;

  TTBXHoverKind = (hkNone, hkKeyboardHover, hkMouseHover);
  TTBXComboPart = (cpNone, cpCombo, cpSplitLeft, cpSplitRight);
  TTBXItemInfo = record
    Control: TControl;
    ViewType: Integer;          // VT_*, TVT_*, PVT_*, or DPVT_* constant
    ItemOptions: Integer;       // IO_* flags
    Enabled: Boolean;
    Pushed: Boolean;
    HoverKind: TTBXHoverKind;
    Selected: Boolean;
    ImageShown: Boolean;
    ImageWidth: Integer;
    ImageHeight: Integer;
    IsVertical: Boolean;
    ComboPart: TTBXComboPart;
    IsPopupParent: Boolean;     // used in officexp theme
    PopupMargin: Integer;
    AppFlags: Integer;          // reserved for extensions
    AppData: Integer;
  end;

  TTBXWindowInfo = record
    ParentControl: TControl;
    ParentHandle: HWND;         // handle of a parent floating window
    WindowHandle: HWND;         // handle of a toolbar or dockable panel
    ViewType: Integer;          // TVT_* or DPVT_* view types (loating)
    ClientWidth: Integer;
    ClientHeight: Integer;
    ShowCaption: Boolean;
    FloatingBorderSize: TPoint;
    CloseButtonState: Integer;  // CDBS_* state flags
    RedrawPart: Integer;        // WRP_ constants
    Caption: PChar;
    EffectiveColor: TColor;
    Active: Boolean;
    AppFlags: Integer;          // reserved for extensions
    AppData: Integer;
  end;

  TTBXPopupInfo = record
    WindowHandle: HWND;
    ViewType: Integer;
    ParentRect: TRect;
    BorderSize: TPoint;
    AppFlags: Integer;          // reserved for extensions
    AppData: Integer;
  end;

  TTBXToolbarInfo = record
    WindowHandle: HWND;
    ViewType: Integer;          // TVT_* view types (docked)
    IsVertical: Boolean;
    AllowDrag: Boolean;
    BorderStyle: TBorderStyle;
    BorderSize: TPoint;
    ClientWidth: Integer;
    ClientHeight: Integer;
    DragHandleStyle: Integer;
    CloseButtonState: Integer;  // CDBS_* state flags
    Caption: PChar;
    EffectiveColor: TColor;
    AppFlags: Integer;          // reserved for extensions
    AppData: Integer;
  end;

  TTBXDockPanelInfo = record
    WindowHandle: HWND;
    ViewType: Integer;         // DPVT_* view types (docked)
    IsVertical: Boolean;
    AllowDrag: Boolean;
    BorderStyle: TBorderStyle;
    BorderSize: TPoint;
    ClientWidth: Integer;
    ClientHeight: Integer;
    ShowCaption: Boolean;
    CloseButtonState: Integer;
    Caption: PChar;
    EffectiveColor: TColor;
    AppFlags: Integer;           // reserved for extensions
    AppData: Integer;
  end;

  TTBXEditBtnInfo = record
    ButtonType: Integer;         // EBT_* button type
    ButtonState: Integer;
  end;

  TTBXEditInfo = record
    LeftBtnWidth: Integer;
    RightBtnWidth: Integer;
    LeftBtnInfo: TTBXEditBtnInfo;   // valid only if LeftBtnWidth > 0
    RightBtnInfo: TTBXEditBtnInfo;  // valid only if RightBtnWidth > 0
  end;

  TTBXTheme = class
  private
    FName: string;
    FTag: Integer;
  public
    constructor Create(const AName: string); virtual;

    { Margins, color, etc. }
    function  GetImageOffset(Canvas: TCanvas; const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList): TPoint; virtual; abstract;
    function  GetItemColor(const ItemInfo: TTBXItemInfo): TColor; virtual; abstract;
    function  GetItemTextColor(const ItemInfo: TTBXItemInfo): TColor; virtual; abstract;
    function  GetItemImageBackground(const ItemInfo: TTBXItemInfo): TColor; virtual; abstract;
    procedure GetMargins(MarginID: Integer; out Margins: TTBXMargins); virtual; abstract;
    function  GetPopupShadowType: Integer; virtual; abstract; // returns one of the PST_ constants
    procedure GetViewBorder(Control: TControl; ViewType: Integer; out Border: TPoint); virtual; abstract;
    function  GetViewColor(ViewType: Integer): TColor; virtual; abstract;
    procedure GetViewMargins(ViewType: Integer; out Margins: TTBXMargins); virtual; abstract;
    function GetSysColor(nIndex: Integer): DWORD; virtual; abstract;

    { General painting routines }
    procedure PaintBackgnd(Canvas: TCanvas; const ADockRect, ARect, AClipRect: TRect; AColor: TColor; Transparent: Boolean; AViewType: Integer); virtual; abstract;
    procedure PaintButton(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); virtual; abstract;
    procedure PaintCaption(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo; const ACaption: string; AFormat: Cardinal; Rotated: Boolean); virtual; abstract;
    procedure PaintCheckMark(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo); virtual; abstract;
    procedure PaintChevron(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo); virtual; abstract;
    procedure PaintEditFrame(Monitor: TMonitor; Canvas: TCanvas; const ARect: TRect; var ItemInfo: TTBXItemInfo; const EditInfo: TTBXEditInfo); virtual; abstract;
    procedure PaintEditButton(Canvas: TCanvas; const ARect: TRect; var ItemInfo: TTBXItemInfo; ButtonInfo: TTBXEditBtnInfo); virtual; abstract;
    procedure PaintDock(Canvas: TCanvas; const ClientRect, DockRect: TRect; DockPosition: Integer); virtual; abstract;
    procedure PaintDropDownArrow(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); virtual; abstract;
    procedure PaintFloatingBorder(Canvas: TCanvas; const ARect: TRect; const WindowInfo: TTBXWindowInfo); virtual; abstract;
    procedure PaintFrame(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); virtual; abstract;
    procedure PaintImage(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer); virtual; abstract;
    procedure PaintMenuItem(Canvas: TCanvas; const ARect: TRect; var ItemInfo: TTBXItemInfo); virtual; abstract;
    procedure PaintMenuItemFrame(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); virtual; abstract;
    procedure PaintPopupNCArea(Canvas: TCanvas; R: TRect; const PopupInfo: TTBXPopupInfo); virtual; abstract;
    procedure PaintSeparator(Canvas: TCanvas; ARect: TRect; ItemInfo: TTBXItemInfo; Horizontal, LineSeparator: Boolean); virtual; abstract;
    procedure PaintToolbarNCArea(Monitor: TMonitor; Canvas: TCanvas; R: TRect; const WindowInfo: TTBXToolbarInfo); virtual; abstract;
    procedure PaintStatusBar(Control: TWinControl; Canvas: TCanvas; R: TRect; Part: Integer); virtual; abstract;

    function GetIntegerMetrics(Viewer: TTBItemViewer; Index: Integer): Integer; overload;
    function GetIntegerMetrics(Monitor: TMonitor; Index: Integer): Integer; overload; virtual; abstract;

    { Boolean metrics access }
    function GetBooleanMetrics(Index: Integer): Boolean; virtual; abstract;
    property OfficeXPPopupAlignment: Boolean index TMB_OFFICEXPPOPUPALIGNMENT read GetBooleanMetrics;
    property EditMenuFullSelect: Boolean index TMB_EDITMENUFULLSELECT read GetBooleanMetrics;
    property EditHeightEven: Boolean index TMB_EDITHEIGHTEVEN read GetBooleanMetrics;
    property PaintDockBackground: Boolean index TMB_PAINTDOCKBACKGROUND read GetBooleanMetrics;
    property SolidToolbarNCArea: Boolean index TMB_SOLIDTOOLBARNCAREA read GetBooleanMetrics;

    property Name: string read FName;
    property Tag: Integer read FTag write FTag;
  end;

  TTBXThemeClass = class of TTBXTheme;

const
{ TBXSysParam Params }
  TSP_FLATMENUSTYLE    = 1;
  TSP_XPVISUALSTYLE    = 2;

{ Flat menu styles }
  FMS_AUTOMATIC = 0;
  FMS_DISABLED  = 1;
  FMS_ENABLED   = 2;

{ XP visual styles}
  XPVS_AUTOMATIC = 0;
  XPVS_DISABLED  = 2;

procedure SetTBXSysParam(Param: Integer; Value: Integer);
function  GetTBXSysParam(Param: Integer): Integer;

procedure AddTBXSysChangeNotification(AObject: TObject);
procedure RemoveTBXSysChangeNotification(AObject: TObject);

procedure RegisterTBXTheme(const AName: string; AThemeClass: TTBXThemeClass);
procedure UnregisterTBXTheme(const AName: string);
function  IsTBXThemeAvailable(const AName: string): Boolean;
procedure GetAvailableTBXThemes(Strings: TStrings);

function  GetTBXTheme(const AName: string): TTBXTheme;
procedure ReleaseTBXTheme(var ATheme: TTBXTheme);

{ Additional "system" variables - do not change }

var
  { Additional colors }
  clHotLight: TColor;
  clPopup: TColor;
  clPopupText: TColor;
  clToolbar: TColor;
  clToolbarText: TColor;

  { Auxiliary flags corresponding to the system color scheme }
  TBXLoColor: Boolean;

var
  USE_FLATMENUS: Boolean;
  USE_THEMES: Boolean;

{ Misc. Functions }
{ Warning: These functions may be changed or relocated in future versions }

function GetTBXCaptionRect(const WindowInfo: TTBXWindowInfo; AdjustForBorder, MinusCloseButton: Boolean): TRect;
function GetTBXCloseButtonRect(const WindowInfo: TTBXWindowInfo; AdjustForBorder: Boolean): TRect;
function GetTBXDockedCloseButtonRect(const ToolbarInfo: TTBXToolbarInfo): TRect;
function GetTBXDragHandleSize(const ToolbarInfo: TTBXToolbarInfo): Integer;

implementation

uses
  SysUtils, TBXUtils, UxTheme, Types, PasTools;

const
  SPI_GETFLATMENU = $1022;

type
  TThemeInfo = record
    Name: {MP}string;
    ThemeClass: TTBXThemeClass;
    ThemeInstance: TTBXTheme;
    RefCount: Integer;
  end;

var
  Themes: array of TThemeInfo;
  InitedThemeLibrary: Boolean;

{ TTBXThemeManager }

type
  TTBXThemeManager = class
  private
    FEnableVisualStyles: Boolean;
    FFlatMenuStyle: Integer;
    FNotifies: TList;
    FWindowHandle: HWND;
    procedure SetEnableVisualStyles(Value: Boolean);
    procedure SetFlatMenuStyle(Value: Integer);
  protected
    procedure VisualStylesClose;
    procedure VisualStylesOpen;
    procedure UpdateVariables;
    procedure WndProc(var Message: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    function  Broadcast(Msg: Cardinal; Param1, Param2: Integer): Integer;
    procedure Notify;
    procedure AddNotification(AObject: TObject);
    procedure RemoveNotification(AObject: TObject);
    property EnableVisualStyles: Boolean read FEnableVisualStyles write SetEnableVisualStyles;
    property FlatMenuStyle: Integer read FFlatMenuStyle write SetFlatMenuStyle;
  end;

var
  ThemeManager: TTBXThemeManager;

procedure SetTBXSysParam(Param: Integer; Value: Integer);
begin
  case Param of
    TSP_FLATMENUSTYLE: ThemeManager.FlatMenuStyle := Value;
    TSP_XPVISUALSTYLE: ThemeManager.EnableVisualStyles := (Value = XPVS_AUTOMATIC);
  end;
end;

function  GetTBXSysParam(Param: Integer): Integer;
const
  CXPVStyles: array [Boolean] of Integer = (XPVS_DISABLED, XPVS_AUTOMATIC);
begin
  Assert(ThemeManager <> nil);
  case Param of
    TSP_FLATMENUSTYLE: Result := ThemeManager.FlatMenuStyle;
    TSP_XPVISUALSTYLE: Result := CXPVStyles[ThemeManager.EnableVisualStyles];
  else
    Result := -1;
  end;
end;

procedure AddTBXSysChangeNotification(AObject: TObject);
begin
  ThemeManager.AddNotification(AObject);
end;

procedure RemoveTBXSysChangeNotification(AObject: TObject);
begin
  ThemeManager.RemoveNotification(AObject);
end;

function FindTBXTheme(const AName: string): Integer;
begin
  for Result := 0 to Length(Themes) - 1 do
    if CompareText(Themes[Result].Name, AName) = 0 then Exit;
  Result := -1;
end;

procedure RegisterTBXTheme(const AName: string; AThemeClass: TTBXThemeClass);
var
  Index: Integer;
begin
  if (Length(AName) = 0) or (AThemeClass = nil) then
    raise Exception.Create('Cannot register theme');
  Index := FindTBXTheme(AName);
  if Index >= 0 then raise Exception.CreateFmt('Theme %s is already registered', [AName]);
  Index := Length(Themes);
  SetLength(Themes, Index + 1);
  with Themes[Index] do
  begin
    Name := AName;
    ThemeClass := AThemeClass;
    ThemeInstance := nil;
    RefCount := 0;
  end;
end;

procedure UnregisterTBXTheme(const AName: string);
var
  Index, L: Integer;
begin
  Index := FindTBXTheme(AName);
  if Index < 0 then raise Exception.CreateFmt('Cannot unregister unknown theme %s', [AName]);
  L := Length(Themes);
  if Index < L - 1 then
    Move(Themes[Index + 1], Themes[Index], SizeOf(TThemeInfo) * (L - Index - 1));
  SetLength(Themes, L - 1);
end;

function IsTBXThemeAvailable(const AName: string): Boolean;
begin
  Result := FindTBXTheme(AName) >= 0;
end;

procedure GetAvailableTBXThemes(Strings: TStrings);
var
  I: Integer;
begin
  Assert(Strings <> nil);
  for I := 0 to Length(Themes) - 1 do
    Strings.Add(Themes[I].Name);
end;

function GetTBXTheme(const AName: string): TTBXTheme;
var
  Index: Integer;
  M: TMessage;
begin
  Index := FindTBXTheme(AName);
  if Index < 0 then raise Exception.Create('Unknown theme ' + AName);
  with Themes[Index] do
  begin
    if RefCount = 0 then
    begin
      { Create a new instance and increase reference count }
      Assert(ThemeInstance = nil);
      ThemeInstance := ThemeClass.Create(Name);
      M.Msg := TBX_SYSCOMMAND;
      M.WParam := Integer(Application.Active);
      M.LParam := 0;
      M.Result := 0;
      ThemeInstance.Dispatch(M);
    end;
    Inc(RefCount);
    Result := ThemeInstance;
  end;
end;

procedure ReleaseTBXTheme(var ATheme: TTBXTheme);
var
  Index: Integer;
begin
  for Index := 0 to Length(Themes) - 1 do
    with Themes[Index] do
      if ThemeInstance = ATheme then
      begin
        if RefCount < 1 then raise Exception.Create('Cannot release theme ' + Themes[Index].Name);
        Dec(RefCount);
        if RefCount = 0 then
        begin
          ThemeInstance.Free;
          ThemeInstance := nil;
          ATheme := nil;
        end;
        Exit;
      end;
  raise Exception.Create('Cannot release theme');
end;

{ TTBXTheme }

constructor TTBXTheme.Create(const AName: string);
begin
  FName := AName;
end;

function TTBXTheme.GetIntegerMetrics(Viewer: TTBItemViewer; Index: Integer): Integer;
begin
  Result := GetIntegerMetrics(Viewer.View.GetMonitor, Index);
end;


{ Misc. Functions }

function GetTBXCaptionRect(const WindowInfo: TTBXWindowInfo;
  AdjustForBorder, MinusCloseButton: Boolean): TRect;
begin
  Result := Rect(0, 0, WindowInfo.ClientWidth, GetSystemMetricsForControl(WindowInfo.ParentControl, SM_CYSMCAPTION) - 1);
  if MinusCloseButton then Dec(Result.Right, Result.Bottom);
  if AdjustForBorder then
    with WindowInfo.FloatingBorderSize do OffsetRect(Result, X, Y);
end;

function GetTBXCloseButtonRect(const WindowInfo: TTBXWindowInfo;
  AdjustForBorder: Boolean): TRect;
begin
  Result := GetTBXCaptionRect(WindowInfo, AdjustForBorder, False);
  Result.Left := Result.Right - Result.Bottom + Result.Top;
end;

function GetTBXDockedCloseButtonRect(const ToolbarInfo: TTBXToolbarInfo): TRect;
var
  X, Y, Z: Integer;
begin
  with ToolbarInfo do
  begin
    Z := GetTBXDragHandleSize(ToolbarInfo) - 1;
    if not IsVertical then
    begin
      X := BorderSize.X;
      Y := BorderSize.Y;
    end
    else
    begin
      X := (ClientWidth + BorderSize.X) - Z;
      Y := BorderSize.Y;
    end;
    Result := Bounds(X, Y, Z, Z);
  end;
end;

function GetTBXDragHandleSize(const ToolbarInfo: TTBXToolbarInfo): Integer;
const
  DragHandleSizes: array [Boolean, 0..2] of Integer = ((9, 0, 6), (14, 14, 14));
begin
  with ToolbarInfo do
  begin
    if AllowDrag then
      Result :=
        ScaleByPixelsPerInch(
          DragHandleSizes[(CloseButtonState and CDBS_VISIBLE) <> 0, DragHandleStyle],
          Screen.MonitorFromWindow(ToolbarInfo.WindowHandle, mdNearest))
    else
      Result := 0;
  end;
end;

{ TTBXThemeManager }

procedure TTBXThemeManager.AddNotification(AObject: TObject);
begin
  FNotifies.Add(AObject);
end;

function TTBXThemeManager.Broadcast(Msg: Cardinal; Param1, Param2: Integer): Integer;
var
  I: Integer;
  M: TMessage;
begin
  if FNotifies.Count > 0 then
  begin
    M.Msg := Msg;
    M.WParam := Param1;
    M.LParam := Param2;
    M.Result := 0;
    for I := 0 to FNotifies.Count - 1 do TObject(FNotifies[I]).Dispatch(M);
    Result := M.Result;
  end
  else Result := 0;
end;

constructor TTBXThemeManager.Create;
begin
  FEnableVisualStyles := True;
  FFlatMenuStyle := FMS_AUTOMATIC;
  FNotifies := TList.Create;
  FWindowHandle := Classes.AllocateHWnd(WndProc);
  UpdateVariables;
end;

destructor TTBXThemeManager.Destroy;
begin
  VisualStylesClose;
  Classes.DeallocateHWnd(FWindowHandle);
  FNotifies.Free;
  VisualStylesClose;
  inherited;
end;

procedure TTBXThemeManager.Notify;
begin
  if FNotifies.Count > 0 then
  begin
    Broadcast(TBX_SYSCOMMAND, TSC_BEFOREVIEWCHANGE, 0);
    Broadcast(TBX_SYSCOMMAND, TSC_VIEWCHANGE, 0);
    Broadcast(TBX_SYSCOMMAND, TSC_AFTERVIEWCHANGE, 0);
  end;
end;

procedure TTBXThemeManager.RemoveNotification(AObject: TObject);
begin
  FNotifies.Remove(AObject);
end;

procedure TTBXThemeManager.SetEnableVisualStyles(Value: Boolean);
begin
  if Value <> FEnableVisualStyles then
  begin
    FEnableVisualStyles := Value;
    UpdateVariables;
    Notify;
  end;
end;

procedure TTBXThemeManager.SetFlatMenuStyle(Value: Integer);
begin
  if Value <> FFlatMenuStyle then
  begin
    FFlatMenuStyle := Value;
    UpdateVariables;
    Notify;
  end;
end;

procedure TTBXThemeManager.UpdateVariables;
var
  DC: HDC;
  SysFlatMenus: Boolean;
  ToolbarTheme: THandle;
begin
  TBXUtils.RecreateStock;

  DC := GetDC(0);
  try
    TBXLoColor := GetDeviceCaps(DC, BITSPIXEL) * GetDeviceCaps(DC, PLANES) < 12;
  finally
    ReleaseDC(0, DC);
  end;

  VisualStylesClose;
  VisualStylesOpen;

  clToolbar := clBtnFace;
  clToolbarText := clBtnText;
  if USE_THEMES then
  begin
    ToolbarTheme := OpenThemeData(FWindowHandle, 'TOOLBAR');
    GetThemeColor(ToolbarTheme, 0, 0, TMT_FILLCOLOR, Cardinal(clToolbar));
    GetThemeColor(ToolbarTheme, 0, 0, TMT_TEXTCOLOR, Cardinal(clToolbarText));
    CloseThemeData(ToolbarTheme);
  end;

  SysFlatMenus := False;
  SystemParametersInfo(SPI_GETFLATMENU, 0, @SysFlatMenus, 0);


  if SysFlatMenus then // System indicates support for flat menus
  begin
    if FlatMenuStyle in [FMS_AUTOMATIC, FMS_ENABLED] then
    begin
      USE_FLATMENUS := True;
      clPopup := clMenu;
      clPopupText := clMenuText;
    end
    else
    begin
      USE_FLATMENUS := False;
      clPopup := clToolbar;
      clPopupText := clToolbarText;
    end;
  end
  else
  begin
    if FlatMenuStyle = FMS_ENABLED then
    begin
      USE_FLATMENUS := True;
      clPopup := clWindow;
      clPopupText := clWindowText;
    end
    else
    begin
      USE_FLATMENUS := False;
      clPopup := clMenu;
      clPopupText := clMenuText;
    end;
  end;
end;

procedure TTBXThemeManager.VisualStylesClose;
begin
  if InitedThemeLibrary then
  begin
    InitedThemeLibrary := False;
    FreeThemeLibrary;
  end;
end;

procedure TTBXThemeManager.VisualStylesOpen;
begin
  USE_THEMES := False;
  if EnableVisualStyles then
  begin
    InitedThemeLibrary := InitThemeLibrary;
    USE_THEMES := UseThemes;
  end;
end;

procedure TTBXThemeManager.WndProc(var Message: TMessage);
const
  ActiveFlags: array [Boolean] of Integer = (TSC_APPDEACTIVATE, TSC_APPACTIVATE);
begin
  case Message.Msg of
    WM_DISPLAYCHANGE, WM_SYSCOLORCHANGE, WM_THEMECHANGED:
      begin
        UpdateVariables;
        Notify;
      end;
    WM_ACTIVATEAPP:
      Broadcast(TBX_SYSCOMMAND, ActiveFlags[Boolean(Message.WParam)], 0);
  end;
  with Message do Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
end;

initialization
if GetSysColorBrush(COLOR_HOTLIGHT) = 0 then clHotLight := clHighlight
else clHotLight := TColor($80000000 or 26);
Themes := nil;
ThemeManager := TTBXThemeManager.Create;

finalization
ThemeManager.Free;
SetLength(Themes, 0);

end.
