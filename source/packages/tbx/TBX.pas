unit TBX;

// TBX Package
// Copyright 2001-2004 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// Id: TBX.pas 21 2004-05-29 22:16:01Z Alex@ZEISS

interface

{$I TB2Ver.inc}

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, ImgList, Forms,
  TB2Item, TB2Dock, TB2Toolbar, TB2Anim, TBXUtils, TBXThemes, PasTools;

const
  TBXVersion = 2.1;
  TBXVersionString = '2.1';
  TBXVersionText = 'TBX version ' + TBXVersionString;

{ TBX Messages }
const
  TBM_THEMECHANGE = WM_USER + 314;
  TBM_GETVIEWTYPE = WM_USER + 237;
  TBM_GETEFFECTIVECOLOR = WM_USER + 238;

function GetViewType(View: TTBView): Integer;
function GetWinViewType(Window: TControl): Integer;
function IsFloating(ViewType: Integer): Boolean;

type
  TTextWrapping = (twNone, twEndEllipsis, twPathEllipsis, twWrap);
  TTextTruncation = twNone..twPathEllipsis;
  TTriState = (tsDefault, tsTrue, tsFalse);
  TFontSize = 25..1000;

  TFontSettings = class(TPersistent)
  private
    FBold: TTriState;
    FItalic: TTriState;
    FUnderline: TTriState;
    FStrikeOut: TTriState;
    FSize: TFontSize;
    FColor: TColor;
    FName: TFontName;
    FOnChange: TNotifyEvent;
    procedure SetBold(Value: TTriState);
    procedure SetColor(Value: TColor);
    procedure SetItalic(Value: TTriState);
    procedure SetName(const Value: TFontName);
    procedure SetSize(Value: TFontSize);
    procedure SetStrikeOut(Value: TTriState);
    procedure SetUnderline(Value: TTriState);
  protected
    procedure Modified;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    procedure Apply(Font: TFont); overload;
    procedure Apply(var LF: TLogFont; var FontColor: TColor); overload;
    procedure Assign(Src: TPersistent); override;
    function  CreateTransformedFont(Src: HFont; var FontColor: TColor): HFont;
  published
    property Bold: TTriState read FBold write SetBold default tsDefault;
    property Italic: TTriState read FItalic write SetItalic default tsDefault;
    property Underline: TTriState read FUnderline write SetUnderline default tsDefault;
    property StrikeOut: TTriState read FStrikeOut write SetStrikeOut default tsDefault;
    property Size: TFontSize read FSize write SetSize default 100;  // percent
    property Color: TColor read FColor write SetColor default clNone;
    property Name: TFontName read FName write SetName;   // default ''
  end;

  TTBXPopupPositionInfo = record
    Item: TTBCustomItem;         // this is a tentative type, it will be changed
    ParentView: TTBView;         // or removed in future versions
    ParentViewer: TTBItemViewer;
    PositionAsSubmenu: Boolean;
    APopupPoint: TPoint;
    Alignment: TTBPopupAlignment;
    PopupWindow: TTBPopupWindow;
    X, Y: Integer;
    ParentItemRect: TRect;
    AppFlags: Integer;           // reserved for extensions
    AppData: Integer;
  end;

  TTBXThemeClass = class of TTBXTheme;

function GetStateFlags(const ItemInfo: TTBXItemInfo): Integer;
function GetTBXTextColor(StateFlags: Integer): TColor;
procedure DrawTBXCaption(Canvas: TCanvas; Rect: TRect; const Text: string;
  Format: Cardinal; StateFlags: Integer);
procedure DrawTBXImage(Canvas: TCanvas; Rect: TRect; ImageList: TCustomImageList;
  ImageIndex: Integer; StateFlags: Integer);


type
  { TTBXItem }

  TAdjustFontEvent = procedure(Item: TTBCustomItem; Viewer: TTBItemViewer;
    Font: TFont; StateFlags: Integer) of object; // state flags are the combination of ISF_* constants
  TDrawImageEvent = procedure(Item: TTBCustomItem; Viewer: TTBItemViewer;
    Canvas: TCanvas; ImageRect: TRect; ImageOffset: TPoint; StateFlags: Integer) of object;

  TTBXCustomItem = class(TTBCustomItem)
  private
    FAlwaysSelectFirst: Boolean;
    FFontSettings: TFontSettings;
    FLayout: TTBXItemLayout;
    FMinHeight: Integer;
    FMinWidth: Integer;
    FToolBoxPopup: Boolean;
    FOnAdjustFont: TAdjustFontEvent;
    FOnDrawImage: TDrawImageEvent;
    procedure FontSettingsChanged(Sender: TObject);
    function  GetStretch: Boolean;
    procedure SetFontSettings(Value: TFontSettings);
    procedure SetLayout(Value: TTBXItemLayout);
    procedure SetMinHeight(Value: Integer);
    procedure SetMinWidth(Value: Integer);
    procedure SetStretch(Value: Boolean);
  protected
    function CreatePopup(const ParentView: TTBView; const ParentViewer: TTBItemViewer;
      const PositionAsSubmenu, SelectFirstItem, Customizing: Boolean;
      const APopupPoint: TPoint; const Alignment: TTBPopupAlignment): TTBPopupWindow; override;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    procedure GetPopupPosition(ParentView: TTBView; PopupWindow: TTBPopupWindow;
      var PopupPositionRec: TTBPopupPositionRec); override;
    function GetPopupWindowClass: TTBPopupWindowClass; override;
    property ToolBoxPopup: Boolean read FToolBoxPopup write FToolBoxPopup default False;
    property OnAdjustFont: TAdjustFontEvent read FOnAdjustFont write FOnAdjustFont;
    property OnDrawImage: TDrawImageEvent read FOnDrawImage write FOnDrawImage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate;
    property AlwaysSelectFirst: Boolean read FAlwaysSelectFirst write FAlwaysSelectFirst default False;
    property FontSettings: TFontSettings read FFontSettings write SetFontSettings;
    property Layout: TTBXItemLayout read FLayout write SetLayout default tbxlAuto;
    property MinHeight: Integer read FMinHeight write SetMinHeight default 0;
    property MinWidth: Integer read FMinWidth write SetMinWidth default 0;
    property Stretch: Boolean read GetStretch write SetStretch default False;
  end;

  TTBXItem = class (TTBXCustomItem)
    property Action;
    property AutoCheck;
    property Caption;
    property Checked;
    property DisplayMode;
    property Enabled;
    property FontSettings;
    property GroupIndex;
    property HelpContext;
    { MP }
    property HelpKeyword;
    property Hint;
    property ImageIndex;
    property Images;
    property InheritOptions;
    property Layout;
    property MaskOptions;
    property MinHeight;
    property MinWidth;
    property Options;
    property RadioItem;
    property ShortCut;
    property Stretch;
    property Visible;
    property OnAdjustFont;
    property OnDrawImage;
    property OnClick;
    property OnSelect;
  end;

  TTBXItemViewer = class(TTBItemViewer)
  private
    FWide: Boolean;
  protected
    procedure DoPaintCaption(Canvas: TCanvas; const ClientAreaRect: TRect;
      var CaptionRect: TRect; IsTextRotated: Boolean; var PaintDefault: Boolean); virtual;
    function  GetAccRole: Integer; override;
    function  GetImageSize: TSize; dynamic;
    function  GetItemType: Integer; virtual;
    function  GetTextFlags: Cardinal; dynamic;
    function  GetTextSize(Canvas: TCanvas; const Text: string; TextFlags: Cardinal; Rotated: Boolean; StateFlags: Integer): TSize; dynamic;
    function  IsToolbarSize: Boolean; override;
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    procedure DrawItemImage(Canvas: TCanvas; ARect: TRect; ItemInfo: TTBXItemInfo); virtual;
    procedure DoAdjustFont(AFont: TFont; StateFlags: Integer); virtual;
    function  GetImageShown: Boolean; virtual;
    function  IsPtInButtonPart(X, Y: Integer): Boolean; override;
    procedure MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean); override;
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect; IsHoverItem, IsPushed, UseDisabledShadow: Boolean); override;
    property Wide: Boolean read FWide write FWide default True;
  public
    constructor Create(AView: TTBView; AItem: TTBCustomItem; AGroupLevel: Integer); override;
    function  IsToolbarStyle: Boolean; override;
  end;

  { TTBXSubmenuItem }

  TTBXSubmenuItem = class(TTBXCustomItem)
  private
    function GetDropdownCombo: Boolean;
    procedure SetDropdownCombo(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Action;
    property AlwaysSelectFirst;
    property AutoCheck;
    property Caption;
    property Checked;
    property DisplayMode;
    property DropdownCombo: Boolean read GetDropdownCombo write SetDropdownCombo default False;
    property Enabled;
    property FontSettings;
    property GroupIndex;
    property HelpContext;
    { MP }
    property HelpKeyword;
    property Hint;
    property ImageIndex;
    property Images;
    property InheritOptions;
    property Layout;
    property LinkSubitems;
    property MaskOptions;
    property MinHeight;
    property MinWidth;
    property Options;
    property RadioItem;
    property ShortCut;
    property Stretch;
    property SubMenuImages;
    property ToolBoxPopup;
    property Visible;
    property OnAdjustFont;
    property OnDrawImage;
    property OnClick;
    property OnPopup;
    property OnSelect;
  end;

  { TTBXSeparatorItem }

  TTBXSeparatorItem = class(TTBSeparatorItem)
  private
    FSize: Integer;
    procedure SetSize(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  published
    property Size: Integer read FSize write SetSize default -1;
    property MaskOptions;
    property Options;
  end;

  TTBXSeparatorItemViewer = class(TTBSeparatorItemViewer)
  protected
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect;
      IsHoverItem, IsPushed, UseDisabledShadow: Boolean); override;
    function  IsToolbarSize: Boolean; override;
  public
    function  IsToolbarStyle: Boolean; override;
  end;

  { TTBXPopupWindow }

  TTBXPopupWindow = class(TTBPopupWindow)
  private
    FControlRect: TRect;
    FShadows: TShadows;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure TBMGetViewType(var Message: TMessage); message TBM_GETVIEWTYPE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMPrint(var Message: TMessage); message WM_PRINT;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMTB2kPopupShowing(var Message: TMessage); message WM_TB2K_POPUPSHOWING;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateShadow; virtual;
    procedure DestroyShadow; virtual;
    function  GetNCSize: TPoint; override;
    function  GetShowShadow: Boolean; virtual;
    function  GetViewClass: TTBViewClass; override;
    procedure PaintScrollArrows; override;
  public
    destructor Destroy; override;
    function  GetFillColor: TColor;
  end;

  TTBXPopupView = class(TTBPopupView);

  { TTBXToolbarView }

  TTBXToolbarView = class(TTBToolbarView)
  protected
    procedure GetMargins(AOrientation: TTBViewOrientation; var Margins: TRect); override;
  end;

  { TTBXToolbar }
  TTBXItemTransparency = (itAuto, itEnable, itDisable);

  TTBXToolbar = class(TTBCustomToolbar)
  private
    FEffectiveColor: TColor;
    FItemTransparency: TTBXItemTransparency;
    FSnapDistance: Integer;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure SetItemTransparency(const Value: TTBXItemTransparency);
    procedure SetSnapDistance(Value: Integer);
    procedure TBMGetViewType(var Message: TMessage); message TBM_GETVIEWTYPE;
    procedure TBMGetEffectiveColor(var Message: TMessage); message TBM_GETEFFECTIVECOLOR;
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMDpiChangedBeforeParent(var Message: TMessage); message WM_DPICHANGED_BEFOREPARENT;
    procedure WMDpiChangedAfterParent(var Message: TMessage); message WM_DPICHANGED_AFTERPARENT;
  protected
    procedure DrawNCArea(const DrawToDC: Boolean; const ADC: HDC; const Clip: HRGN); override;
    function  GetChevronItemClass: TTBChevronItemClass; override;
    function  GetFloatingWindowParentClass: TTBFloatingWindowParentClass; override;
    procedure GetToolbarInfo(out ToolbarInfo: TTBXToolbarInfo); virtual;
    function  GetViewClass: TTBToolbarViewClass; override;
    procedure Loaded; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure UpdateEffectiveColor;
    procedure Rebuild;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  Embedded: Boolean;
    function  GetFloatingBorderSize: TPoint; override;
    procedure UpdateChildColors;
    property EffectiveColor: TColor read FEffectiveColor;
  published
    property ActivateParent;
    property Align;
    property AutoResize;
    property BorderStyle;
    property Caption;
    property ChevronHint;
    property ChevronMoveItems;
    property ChevronPriorityForNewItems;
    property CloseButton;
    property CloseButtonWhenDocked;
    property CurrentDock;
    property DblClickUndock default False;
    property DefaultDock;
    property DockableTo;
    property DockMode;
    property DockPos;
    property DockRow;
    property DragHandleStyle;
    property FloatingMode;
    property Font;
    property FullSize;
    property HideWhenInactive;
    property Images;
    property Items;
    property ItemTransparency: TTBXItemTransparency read FItemTransparency write SetItemTransparency default itAuto;
    property LastDock;
    property LinkSubitems;
    property MenuBar;
    property Options;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ProcessShortCuts;
    property Resizable;
    property ShowCaption;
    property ShowHint;
    property ShrinkMode;
    property SmoothDrag;
    property SnapDistance: Integer read FSnapDistance write SetSnapDistance default 0;
    property Stretch;
    property SystemFont;
    property TabOrder;
    property TabStop;
    property UpdateActions;
    property UseLastDock;
    property Visible;
    property Color default clNone;
    property OnClose;
    property OnCloseQuery;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMove;
    property OnRecreated;
    property OnRecreating;
    property OnDockChanged;
    property OnDockChanging;
    property OnDockChangingHidden;
    property OnResize;
    property OnShortCut;
    property OnVisibleChanged;
    { MP }
    property OnGetBaseSize;
    property OnEndModal;
  end;

  { TTBXChevronItem }

  TTBXChevronItem = class(TTBChevronItem)
  public
    procedure GetPopupPosition(ParentView: TTBView;
      PopupWindow: TTBPopupWindow; var PopupPositionRec: TTBPopupPositionRec); override;
    function GetPopupWindowClass: TTBPopupWindowClass; override;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  end;

  TTBXChevronItemViewer = class(TTBItemViewer)
  protected
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect;
      IsHoverItem, IsPushed, UseDisabledShadow: Boolean); override;
    function CaptionShown: Boolean; override;
  end;

  TTBXChevronPopupWindow = class(TTBXPopupWindow);

  { TTBXPopupMenu }

  TTBXRootItem = class(TTBRootItem)
  private
    FPopupControlRect: TRect;
  protected
    function CreatePopupEx(SelectFirstItem: Boolean; const AControlRect: TRect;
      Alignment: TTBPopupAlignment): TTBPopupWindow; virtual;
    function GetPopupWindowClass: TTBPopupWindowClass; override;
    procedure GetPopupPosition(ParentView: TTBView; PopupWindow: TTBPopupWindow;
      var PopupPositionRec: TTBPopupPositionRec); override;
    function OpenPopupEx(const SelectFirstItem, TrackRightButton: Boolean;
      const ControlRect: TRect; const Alignment: TTBPopupAlignment;
      const ReturnClickedItemOnly: Boolean): TTBCustomItem;
    function PopupEx(const ControlRect: TRect; TrackRightButton: Boolean;
      Alignment: TTBPopupAlignment = tbpaLeft;
      ReturnClickedItemOnly: Boolean = False): TTBCustomItem;
  end;

  TTBXPopupMenu = class(TTBPopupMenu)
  private
    FToolBoxPopup: Boolean;
    procedure TBMGetViewType(var Message: TMessage); message TBM_GETVIEWTYPE;
  protected
    function GetRootItemClass: TTBRootItemClass; override;
  public
    function PopupEx(const ControlRect: TRect;
      ReturnClickedItemOnly: Boolean = False): TTBCustomItem;
    property ToolBoxPopup: Boolean read FToolBoxPopup write FToolBoxPopup default False;
  end;

  TTBXFloatingWindowParent = class(TTBFloatingWindowParent)
  private
    FCloseButtonHover: Boolean;
    FSnapDistance: Integer;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMNCMouseLeave(var Message: TMessage); message $2A2 {WM_NCMOUSELEAVE};
    procedure WMNCMouseMove(var Message: TWMNCMouseMove); message WM_NCMOUSEMOVE;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
  protected
    procedure CancelNCHover;
    procedure DrawNCArea(const DrawToDC: Boolean; const ADC: HDC;
      const Clip: HRGN; RedrawWhat: TTBToolWindowNCRedrawWhat); override;
    property CloseButtonHover: Boolean read FCloseButtonHover;
  public
    property SnapDistance: Integer read FSnapDistance write FSnapDistance default 0;
  end;

  TTBXDock = class(TTBDock)
  private
    FMoving: Boolean;
    FResizing: Boolean;
    FUseParentBackground: Boolean;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure TBMGetEffectiveColor(var Message: TMessage); message TBM_GETEFFECTIVECOLOR;
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    function  ThemedBackground: Boolean; virtual;
    procedure DrawBackground(DC: HDC; const DrawRect: TRect); override;
    procedure Resize; override;
    procedure SetUseParentBackground(Value: Boolean);
    function  UsingBackground: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color default clNone;
    property UseParentBackground: Boolean read FUseParentBackground write SetUseParentBackground default False;
  end;

  { TTBXMenuAnimation }

  TMenuAnimation = (maNone, maUnfold, maSlide, maFade);
  TAnimationMode = (amNone, amSysDefault, amRandom, amUnfold, amSlide, amFade);
  TAnimationModes = set of TAnimationMode;

  TTBXMenuAnimation = class
  private
    FAnimationMode: TAnimationMode;
    function SysParamEnabled(Param: Cardinal): Boolean;
    function GetAvailableModes: TAnimationModes;
    function GetMenuAnimation: TMenuAnimation;
    procedure SetAnimationMode(Value: TAnimationMode);
    property MenuAnimation: TMenuAnimation read GetMenuAnimation;
  public
    constructor Create(AAnimationMode: TAnimationMode = amSysDefault);
    property AnimationMode: TAnimationMode read FAnimationMode write SetAnimationMode;
    property AvailableModes: TAnimationModes read GetAvailableModes;
  end;

var
  CurrentTheme: TTBXTheme;
  TBXMenuAnimation: TTBXMenuAnimation;

procedure TBXSetTheme(const AThemeName: string);
function TBXCurrentTheme: string;

procedure AddThemeNotification(AObject: TObject);
procedure RemoveThemeNotification(AObject: TObject);

{ Additional system colors }
procedure AddTBXColor(var AColor: TColor; const AName: string);
function  TBXIdentToColor(const Ident: string; var Color: Longint): Boolean;
function  TBXColorToString(Color: TColor): string;
function  TBXStringToColor(S: string): TColor;
procedure TBXGetColorValues(Proc: TGetStrProc);

{ Internal routines - do not use }
function GetPopupMargin(ItemViewer: TTBItemViewer): Integer;
function GetEffectiveColor(C: TControl): TColor;
procedure DrawParentBackground(Control: TControl; DC: HDC; R: TRect);
procedure AddToList(var List: TList; Item: Pointer);
procedure RemoveFromList(var List: TList; Item: Pointer);

function CreateTBXPopupMenu(Owner: TComponent): TTBXPopupMenu;

implementation

{$R tbx_glyphs.res}

uses
  TBXExtItems, TBXLists, TB2Common, UxTheme, MultiMon, TBXOfficeXPTheme,
  ComCtrls, Menus, MMSystem, Types, UITypes;

type
  TTBItemAccess = class(TTBCustomItem);
  TTBViewAccess = class(TTBView);
  TTBItemViewerAccess = class(TTBItemViewer);
  TTBFloatingWindowParentAccess = class(TTBFloatingWindowParent);
  TTBCustomDockableWindowAccess = class(TTBCustomDockableWindow);
  TTBXToolbarAccess = class(TTBXToolbar);
  TControlAccess = class(TControl);
  TTBXThemeAccess = class(TTBXTheme);
  TDockAccess = class(TTBDock);
  TTBPopupWindowAccess = class(TTBPopupWindow);

  { TTBNexus }
  TTBXNexus = class
  private
    FNotifies: TList;
    procedure TBXSysCommand(var Message: TMessage); message TBX_SYSCOMMAND;
  protected
    procedure Broadcast(Msg: Cardinal; WParam, LParam: Integer);
  public
    constructor Create(const DefaultTheme: string);
    destructor Destroy; override;
    procedure SetTheme(const AThemeName: string);
    function  GetTheme: string;
    procedure AddNotifie(AObject: TObject);
    procedure RemoveNotifie(AObject: TObject);
  end;

var
  TBXNexus: TTBXNexus;

procedure AddThemeNotification(AObject: TObject);
begin
  TBXNexus.AddNotifie(AObject);
end;

procedure RemoveThemeNotification(AObject: TObject);
begin
  TBXNexus.RemoveNotifie(AObject);
end;

function GetEffectiveColor(C: TControl): TColor;
var
  Message: TMessage;
begin
  if C = nil then Result := clBtnFace
  else
  begin
    Message.Msg := TBM_GETEFFECTIVECOLOR;
    Message.WParam := 0;
    Message.LParam := 0;
    Message.Result := 0;
    C.Dispatch(Message);
    if Message.Result <> 0 then Result := Message.WParam
    else if (C is TForm) and (TForm(C).FormStyle = fsMDIForm) then
      Result := clBtnFace
    else
      Result := TControlAccess(C).Color;
  end;
end;

procedure DrawParentBackground(Control: TControl; DC: HDC; R: TRect);
var
  Parent: TWinControl;
  Theme: HTHEME;
  R2: TRect;
  Shift: TPoint;
  UsingThemes: Boolean;
  Msg: TMessage;
begin
  Parent := Control.Parent;
  if Parent = nil then FillRectEx(DC, R, clBtnFace)
  else
  begin
    Shift.X := 0; Shift.Y := 0;
    Shift := Parent.ScreenToClient(Control.ClientToScreen(Shift));
    SaveDC(DC);
    try
      SetWindowOrgEx(DC, Shift.X, Shift.Y, nil);
      Msg.Msg := WM_ERASEBKGND;
      Msg.WParam := Integer(DC);
      Msg.LParam := Integer(DC);
      Msg.Result := 0;
      Parent.Dispatch(Msg);
    finally
      RestoreDC(DC, -1);
    end;

    if Msg.Result <> 0 then Exit;

    UsingThemes := USE_THEMES and not (csDesigning in Control.ComponentState);
    if Parent is TTBDock then
    begin
      SaveDC(DC);
      SetWindowOrgEx(DC, Control.Left, Control.Top, nil);
      TDockAccess(Parent).DrawBackground(DC, R);
      RestoreDC(DC, -1);
    end
    else if not UsingThemes then
      FillRectEx(DC, R, GetEffectiveColor(Parent))
    else
    begin
      { Unfortunately, DrawThemeParentBackground does seem to have some problems
        with the back buffer. Therefore some sort of workaround is used which
        will work for tab sheets }
      //  if Control is TWinControl then
      //    DrawThemeParentBackground(TWinControl(Control).Handle, DC, @R);

      if Parent is TTabSheet then
      begin
        Theme := OpenThemeData(Parent.Handle, 'TAB');
        R2 := Parent.ClientRect;
        R2.TopLeft := Control.ScreenToClient(Parent.ClientToScreen(R2.TopLeft));
        R2.BottomRight := Control.ScreenToClient(Parent.ClientToScreen(R2.BottomRight));
        DrawThemeBackground(Theme, DC, TABP_BODY, 0, R2, @R);
        CloseThemeData(Theme);
      end
      else FillRectEx(DC, R, GetEffectiveColor(Parent));
    end;
  end;
end;

function GetViewType(View: TTBView): Integer;
var
  Message: TMessage;
begin
  Result := VT_UNKNOWN;
  if (View <> nil) and (View.Owner <> nil) then
  begin
    Message.Msg := TBM_GETVIEWTYPE;
    Message.WParam := 0;
    Message.LParam := 0;
    Message.Result := VT_UNKNOWN;
    View.Window.Dispatch(Message);
    Result := Message.Result;
  end;
end;

function GetWinViewType(Window: TControl): Integer;
var
  Message: TMessage;
begin
  Result := VT_UNKNOWN;
  if Window <> nil then
  begin
    Message.Msg := TBM_GETVIEWTYPE;
    Message.WParam := 0;
    Message.LParam := 0;
    Message.Result := VT_UNKNOWN;
    Window.Dispatch(Message);
    Result := Message.Result;
  end;
end;

function IsFloating(ViewType: Integer): Boolean;
begin
  Result := ViewType and TVT_FLOATING <> 0;
end;

procedure UpdateNCArea(Control: TWinControl; ViewType: Integer);
begin
  with Control do
  begin
    ClientWidth := ClientWidth;
    ClientHeight := ClientHeight;
  end;
  SetWindowPos(Control.Handle, 0, 0, 0, 0, 0,
    SWP_FRAMECHANGED or SWP_NOACTIVATE or SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE);
  Control.Invalidate;
end;

function GetPopupMargin(ItemViewer: TTBItemViewer): Integer;
var
  ImgList: TCustomImageList;
  Sz: TSize;
  TextMetric: TTextMetric;
  H, M2: Integer;
  Margins: TTBXMargins;
begin
  Sz.Cx := 0;
  Sz.Cy := 0;
  if ItemViewer is TTBXItemViewer then
    Sz := TTBXItemViewer(ItemViewer).GetImageSize;
  if (Sz.Cx = 0) or (Sz.Cy = 0) then
  begin
    ImgList := TTBItemViewerAccess(ItemViewer).GetImageList;
    if ImgList <> nil then
    begin
      Sz.Cx := ImgList.Width;
      Sz.Cy := ImgList.Height;
    end;
    if (Sz.Cx = 0) or (Sz.Cy = 0) then
    begin
      Sz.Cx := 16;
      Sz.Cy := 16;
    end;
  end;
  StockBitmap1.Canvas.Font := TTBViewAccess(ItemViewer.View).GetFont;
  GetTextMetrics(StockBitmap1.Canvas.Handle, TextMetric);

  CurrentTheme.GetMargins(MID_MENUITEM, Margins);
  M2 := Margins.TopHeight + Margins.BottomHeight;
  Result := TextMetric.tmHeight + TextMetric.tmExternalLeading + M2;
  H := Sz.CY + M2;
  if H > Result then Result := H;
  Result := (Sz.Cx + M2) * Result div H;
end;

procedure GetOfficeXPPopupPosition1(var PopupPositionRec: TTBPopupPositionRec);
begin
  with PopupPositionRec do
  begin
    if not PositionAsSubmenu then
    begin
      NCSizeX := 0;
      NCSizeY := 0;
      Dec(ParentItemRect.Right);
      if X = ParentItemRect.Right + 1 then Dec(X);
      if X + W <= ParentItemRect.Left then Inc(X);
      Dec(ParentItemRect.Bottom);
      if Y = ParentItemRect.Bottom + 1 then Dec(Y);
      if Y + H <= ParentItemRect.Top then Inc(Y);
      Dec(W);
      Dec(H);
    end
    else
    begin
      Inc(X, NCSizeX);
      Inc(Y, NCSizeY);
      NCSizeX := 0;
      NCSizeY := 0;
    end;
  end;
end;

procedure GetOfficeXPPopupPosition2(var PopupPositionRec: TTBPopupPositionRec);
begin
  with PopupPositionRec do if not PositionAsSubmenu then
  begin
    Inc(W);
    Inc(H);
  end;
end;

procedure AddToList(var List: TList; Item: Pointer);
begin
  if List = nil then List := TList.Create;
  List.Add(Item)
end;


procedure RemoveFromList(var List: TList; Item: Pointer);
begin
  if List <> nil then
  begin
    List.Remove(Item);
    if List.Count = 0 then
    begin
      List.Free;
      List := nil;
    end;
  end;
end;

//============================================================================//

{ Misc. Routines }

procedure InvalidateAll(const Ctl: TWinControl);
begin
  if Ctl.HandleAllocated then
    RedrawWindow(Ctl.Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or
      RDW_ERASE);
end;

function GetStateFlags(const ItemInfo: TTBXItemInfo): Integer;
const
  CEnabledStates: array [Boolean] of Integer = (ISF_DISABLED, 0);
  CHotStates: array [Boolean] of Integer = (0, ISF_HOT);
  CPushedStates: array [Boolean] of Integer = (0, ISF_PUSHED);
  CSelectedStates: array [Boolean] of Integer = (0, ISF_SELECTED);
begin
  with ItemInfo do
    Result := CEnabledStates[ItemInfo.Enabled] or CPushedStates[ItemInfo.Pushed] or
      CHotStates[ItemInfo.HoverKind = hkMouseHover] or CSelectedStates[ItemInfo.Selected];
end;

function GetTBXTextColor(StateFlags: Integer): TColor;
const
  HoverKinds: array [Boolean] of TTBXHoverKind = (hkNone, hkMouseHover);
var
  ItemInfo: TTBXItemInfo;
begin
  FillChar(ItemInfo, SizeOf(ItemInfo), 0);
  case StateFlags and ISF_LOCATIONMASK of
    ISF_TOOLBARCOLOR:
      begin
        ItemInfo.ViewType := VT_TOOLBAR;
        ItemInfo.ItemOptions := IO_TOOLBARSTYLE;
      end;
    ISF_MENUCOLOR:
      begin
        ItemInfo.ViewType := VT_DOCKPANEL;
      end;
    ISF_STATUSCOLOR:
      begin
        ItemInfo.ViewType := VT_STATUSBAR;
      end;
  else
    ItemInfo.ViewType := VT_UNKNOWN;
  end;
  ItemInfo.Enabled := StateFlags and ISF_DISABLED = 0;
  ItemInfo.Pushed := StateFlags and ISF_PUSHED <> 0;
  ItemInfo.HoverKind := HoverKinds[StateFlags and ISF_HOT <> 0];
  ItemInfo.Selected := StateFlags and ISF_SELECTED <> 0;
  Result := CurrentTheme.GetItemTextColor(ItemInfo);
end;

procedure DrawTBXCaption(Canvas: TCanvas; Rect: TRect; const Text: string; Format: Cardinal; StateFlags: Integer);
const
  HoverKinds: array [Boolean] of TTBXHoverKind = (hkNone, hkMouseHover);
var
  ItemInfo: TTBXItemInfo;
begin
  FillChar(ItemInfo, SizeOf(ItemInfo), 0);
  case StateFlags and ISF_LOCATIONMASK of
    ISF_TOOLBARCOLOR:
      begin
        ItemInfo.ViewType := VT_TOOLBAR;
        ItemInfo.ItemOptions := IO_TOOLBARSTYLE;
      end;
    ISF_MENUCOLOR:
      begin
        ItemInfo.ViewType := VT_DOCKPANEL;
      end;
    ISF_STATUSCOLOR:
      begin
        ItemInfo.ViewType := VT_STATUSBAR;
      end;
  end;
  ItemInfo.Enabled := StateFlags and ISF_DISABLED = 0;
  ItemInfo.Pushed := StateFlags and ISF_PUSHED <> 0;
  ItemInfo.HoverKind := HoverKinds[StateFlags and ISF_HOT <> 0];
  ItemInfo.Selected := StateFlags and ISF_SELECTED <> 0;
  CurrentTheme.PaintCaption(Canvas, Rect, ItemInfo, Text, Format, False);
end;

procedure DrawTBXImage(Canvas: TCanvas; Rect: TRect; ImageList: TCustomImageList;
  ImageIndex: Integer; StateFlags: Integer);
const
  HoverKinds: array [Boolean] of TTBXHoverKind = (hkNone, hkMouseHover);
var
  ItemInfo: TTBXItemInfo;
begin
  FillChar(ItemInfo, SizeOf(ItemInfo), 0);
  case StateFlags and ISF_LOCATIONMASK of
    ISF_TOOLBARCOLOR:
      begin
        ItemInfo.ViewType := VT_TOOLBAR;
        ItemInfo.ItemOptions := IO_TOOLBARSTYLE;
      end;
    ISF_MENUCOLOR:
      begin
        ItemInfo.ViewType := VT_DOCKPANEL;
      end;
    ISF_STATUSCOLOR:
      begin
        ItemInfo.ViewType := VT_STATUSBAR;
      end;
  end;
  ItemInfo.Enabled := not Boolean(StateFlags and ISF_DISABLED);
  ItemInfo.Pushed := Boolean(StateFlags and ISF_PUSHED);
  ItemInfo.HoverKind := HoverKinds[Boolean(StateFlags and ISF_HOT)];
  ItemInfo.Selected := Boolean(StateFlags and ISF_SELECTED);
  CurrentTheme.PaintImage(Canvas, Rect, ItemInfo, ImageList, ImageIndex);
end;

//============================================================================//

{ TFontSettings }

procedure TFontSettings.Apply(Font: TFont);
var
  FS: TFontStyles;
begin
  if Size <> 100 then Font.Size := (Font.Size * FSize + 50) div 100;
  if Color <> clNone then Font.Color := Color;
  if Name <> '' then Font.Name := Name;
  FS := Font.Style;
  if Bold = tsTrue then Include(FS, fsBold)
  else if Bold = tsFalse then Exclude(FS, fsBold);
  if Italic = tsTrue then Include(FS, fsItalic)
  else if Italic = tsFalse then Exclude(FS, fsItalic);
  if Underline = tsTrue then Include(FS, fsUnderline)
  else if Underline = tsFalse then Exclude(FS, fsUnderline);
  if StrikeOut = tsTrue then Include(FS, fsStrikeOut)
  else if StrikeOut = tsFalse then Exclude(FS, fsStrikeOut);
  Font.Style := FS;
end;

procedure TFontSettings.Apply(var LF: TLogFont; var FontColor: TColor);
begin
  if Size <> 100 then LF.lfHeight := (LF.lfHeight * Size + 50) div 100;
  if Color <> clNone then FontColor := Color;
  if Name <> '' then StrPLCopy(LF.lfFaceName, Name, 31);
  if Bold = tsTrue then LF.lfWeight := FW_BOLD
  else if Bold = tsFalse then LF.lfWeight := FW_NORMAL;
  if Italic = tsTrue then LF.lfItalic := 1
  else if Italic = tsFalse then LF.lfItalic := 0;
  if Underline = tsTrue then LF.lfUnderline := 1
  else if Underline = tsFalse then LF.lfUnderline := 0;
  if StrikeOut = tsTrue then LF.lfStrikeOut := 1
  else if StrikeOut = tsFalse then LF.lfStrikeOut := 0;
end;

procedure TFontSettings.Assign(Src: TPersistent);
var
  F: TFontSettings;
begin
  if Src is TPersistent then
  begin
    F := TFontSettings(Src);
    if (FBold <> F.Bold) or (FItalic <> F.Italic) or (FUnderline <> F.Underline) or
      (FStrikeOut <> F.StrikeOut) or (FSize <> F.Size) or (FColor <> F.Color) or
      (FName <> F.Name) then
    begin
      FBold := F.Bold;
      FItalic := F.Italic;
      FUnderline := F.Underline;
      FStrikeOut := F.StrikeOut;
      FSize := F.Size;
      FColor := F.Color;
      FName := F.Name;
      Modified;
    end;
  end
  else inherited;
end;

constructor TFontSettings.Create;
begin
  FSize := 100;
  FColor := clNone;
end;

function TFontSettings.CreateTransformedFont(Src: HFont; var FontColor: TColor): HFont;
var
  LF: TLogFont;
begin
  GetObject(Src, SizeOf(LF), @LF);
  Apply(LF, FontColor);
  Result := CreateFontIndirect(LF);
end;

procedure TFontSettings.Modified;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TFontSettings.SetBold(Value: TTriState);
begin
  if FBold <> Value then
  begin
    FBold := Value;
    Modified;
  end;
end;

procedure TFontSettings.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Modified;
  end;
end;

procedure TFontSettings.SetItalic(Value: TTriState);
begin
  if FItalic <> Value then
  begin
    FItalic := Value;
    Modified;
  end;
end;

procedure TFontSettings.SetName(const Value: TFontName);
begin
  if FName <> Value then
  begin
    FName := Value;
    Modified;
  end;
end;

procedure TFontSettings.SetSize(Value: TFontSize);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    Modified;
  end;
end;

procedure TFontSettings.SetStrikeOut(Value: TTriState);
begin
  if FStrikeOut <> Value then
  begin
    FStrikeOut := Value;
    Modified;
  end;
end;

procedure TFontSettings.SetUnderline(Value: TTriState);
begin
  if FUnderline <> Value then
  begin
    FUnderline := Value;
    Modified;
  end;
end;

//============================================================================//

{ TTBXCustomItem }

constructor TTBXCustomItem.Create(AOwner: TComponent);
begin
  inherited;
  FFontSettings := TFontSettings.Create;
  FFontSettings.OnChange := FontSettingsChanged;
end;

function TTBXCustomItem.CreatePopup(const ParentView: TTBView;
  const ParentViewer: TTBItemViewer; const PositionAsSubmenu,
  SelectFirstItem, Customizing: Boolean; const APopupPoint: TPoint;
  const Alignment: TTBPopupAlignment): TTBPopupWindow;
var
  DoSelectFirstItem: Boolean;
begin
  if AlwaysSelectFirst then DoSelectFirstItem := True
  else DoSelectFirstItem := SelectFirstItem;
  Result := inherited CreatePopup(ParentView, ParentViewer, PositionAsSubmenu,
    DoSelectFirstItem, Customizing, APopupPoint, Alignment);
end;

destructor TTBXCustomItem.Destroy;
begin
  FFontSettings.Free;
  inherited;
end;

procedure TTBXCustomItem.FontSettingsChanged(Sender: TObject);
begin
  Change(True);
end;

function TTBXCustomItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TTBXItemViewer;
end;

procedure TTBXCustomItem.GetPopupPosition(ParentView: TTBView;
  PopupWindow: TTBPopupWindow; var PopupPositionRec: TTBPopupPositionRec);
var
  VT: Integer;
begin
  if CurrentTheme.OfficeXPPopupAlignment then with PopupPositionRec do
  begin
    GetOfficeXPPopupPosition1(PopupPositionRec);
    inherited GetPopupPosition(ParentView, PopupWindow, PopupPositionRec);
    GetOfficeXPPopupPosition2(PopupPositionRec);
    VT := GetWinViewType(PopupWindow);
    PopupPositionRec.PlaySound := not (VT and PVT_LISTBOX = PVT_LISTBOX);
  end
  else inherited;
end;

function TTBXCustomItem.GetPopupWindowClass: TTBPopupWindowClass;
begin
  Result := TTBXPopupWindow;
end;

function TTBXCustomItem.GetStretch: Boolean;
begin
  Result := tbisStretch in ItemStyle;
end;

procedure TTBXCustomItem.Invalidate;
begin
  Change(False);
end;

procedure TTBXCustomItem.SetFontSettings(Value: TFontSettings);
begin
  FFontSettings.Assign(Value);
end;

procedure TTBXCustomItem.SetLayout(Value: TTBXItemLayout);
begin
  if Value <> FLayout then
  begin
    FLayout := Value;
    Change(True);
  end;
end;

procedure TTBXCustomItem.SetMinHeight(Value: Integer);
begin
  if Value <> FMinHeight then
  begin
    FMinHeight := Value;
    Change(True);
  end;
end;

procedure TTBXCustomItem.SetMinWidth(Value: Integer);
begin
  if Value <> FMinWidth then
  begin
    FMinWidth := Value;
    Change(True);
  end;
end;

procedure TTBXCustomItem.SetStretch(Value: Boolean);
begin
  if Value xor (tbisStretch in ItemStyle) then
  begin
    if Value then ItemStyle := ItemStyle + [tbisStretch]
    else ItemStyle := ItemStyle - [tbisStretch];
    Change(True);
  end;
end;


//============================================================================//

{ TTBXItemViewer }

procedure TTBXItemViewer.CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer);
const
  CMarginIDs: array [Boolean] of Integer = (MID_MENUITEM, MID_TOOLBARITEM);
  CStartSize: array [Boolean] of Integer = (0, 6);
var
  W, H: Integer;
  ImgSize: TSize;
  Item: TTBCustomItem;
  ItemLayout, SaveLayout: TTBXItemLayout;
  IsCaptionShown: Boolean;
  IsTextRotated: Boolean;
  ToolbarStyle: Boolean;
  S: string;
  TextFlags: Cardinal;
  TextMetric: TTextMetric;
  TextSize: TSize;
  Margins: TTBXMargins;
  Two, Three: Integer;
begin
  Item := TTBCustomItem(Self.Item);
  ToolbarStyle := IsToolbarStyle;
  ImgSize := GetImageSize;
  if (ImgSize.CX <= 0) or (ImgSize.CY <= 0) then
  begin
    ImgSize.CX := 0;
    ImgSize.CY := 0;
  end;

  if Item is TTBXCustomItem then ItemLayout := TTBXCustomItem(Item).Layout
  else ItemLayout := tbxlAuto;
  SaveLayout := ItemLayout;
  if ItemLayout = tbxlAuto then
  begin
    if tboImageAboveCaption in Item.EffectiveOptions then ItemLayout := tbxlGlyphTop
    else
    begin
      if View.Orientation <> tbvoVertical then ItemLayout := tbxlGlyphLeft
      else ItemLayout := tbxlGlyphTop;
    end;
  end;

  { Setup font }
  TextFlags := 0;
  IsCaptionShown := CaptionShown;
  IsTextRotated := (View.Orientation = tbvoVertical) and ToolbarStyle;
  if IsCaptionShown then
  begin
    S := GetCaptionText;
    if not (SaveLayout = tbxlAuto) or (tboImageAboveCaption in Item.EffectiveOptions) then IsTextRotated := False;
    if IsTextRotated or not ToolbarStyle then TextFlags := DT_SINGLELINE;
    TextSize := GetTextSize(Canvas, S, TextFlags, IsTextRotated, 0);
  end
  else
  begin
    SetLength(S, 0);
    TextSize.CX := 0;
    TextSize.CY := 0;
    IsTextRotated := False;
  end;

  { Measure size }
  if ToolbarStyle then
  begin
    if not IsTextRotated then
    begin
      AWidth := 3;
      AHeight := 6;
    end
      else
    begin
      AWidth := 6;
      AHeight := 3;
    end;

    if CaptionShown then
    begin
      Three := ScaleByPixelsPerInch(3, View.GetMonitor);
      Inc(AWidth, TextSize.CX);
      Inc(AHeight, TextSize.CY);
      if not IsTextRotated then Inc(AWidth, 2 * Three)
      else Inc(AHeight, 2 * Three);
    end;

    if GetImageShown and (ImgSize.CX > 0) and (ImgSize.CY > 0) then
    begin
      if ItemLayout = tbxlGlyphLeft then
      begin
        Inc(AWidth, ImgSize.CX + 3);
        if Wide then Inc(AWidth);
        if AHeight < ImgSize.CY + 6 then AHeight := ImgSize.CY + 6;
      end
      else
      begin
        Inc(AHeight, ImgSize.CY + 3);
        if AWidth < ImgSize.CX + 7 then AWidth := ImgSize.CX + 7;
      end;
    end;

    if tbisSubmenu in TTBItemAccess(Item).ItemStyle then with CurrentTheme do
    begin
      if tbisCombo in TTBItemAccess(Item).ItemStyle then Inc(AWidth, GetIntegerMetrics(Self, TMI_SPLITBTN_ARROWWIDTH))
      else if tboDropdownArrow in Item.EffectiveOptions then
      begin
        if (ItemLayout <> tbxlGlyphTop) or (ImgSize.CX = 0) or IsTextRotated then
        begin
          Two := ScaleByPixelsPerInch(2, View.GetMonitor);
          if View.Orientation <> tbvoVertical then
          begin
            Inc(AWidth, Two + GetIntegerMetrics(Self, TMI_DROPDOWN_ARROWWIDTH))
          end
            else
          begin
            Inc(AHeight,
                GetIntegerMetrics(Self, TMI_DROPDOWN_ARROWWIDTH) + Two +
                GetIntegerMetrics(Self, TMI_DROPDOWN_ARROWMARGIN));
          end;
        end
        else
        begin
          if (ItemLayout = tbxlGlyphTop) and (IsTextRotated xor (View.Orientation <> tbvoVertical)) then
          begin
            W := ImgSize.CX + GetIntegerMetrics(Self, TMI_DROPDOWN_ARROWWIDTH) + 2;
            if W > AWidth - 7 then AWidth := W + 7;
          end
          else
          begin
            H := ImgSize.CY + GetIntegerMetrics(Self, TMI_DROPDOWN_ARROWWIDTH) + 2;
            if H > AHeight - 7 then AHeight := H + 7;
          end;
        end
      end;
    end;
  end
  else // Not a ToolbarStyle
    with CurrentTheme do
    begin
      GetTextMetrics(Canvas.Handle, TextMetric);
      Inc(TextSize.CY, TextMetric.tmExternalLeading);

      AWidth := TextSize.CX;
      AHeight := TextSize.CY;

      if ImgSize.CY = 0 then ImgSize.CY := 16;
      if AHeight < ImgSize.CY then AHeight := ImgSize.CY;

      GetMargins(MID_MENUITEM, Margins);
      Inc(AWidth, Margins.LeftWidth + Margins.RightWidth);
      Inc(AHeight, Margins.TopHeight + Margins.BottomHeight);

      Inc(AWidth,
          GetPopupMargin(Self) + GetIntegerMetrics(Self, TMI_MENU_IMGTEXTSPACE) +
            GetIntegerMetrics(Self, TMI_MENU_LCAPTIONMARGIN) + GetIntegerMetrics(Self, TMI_MENU_RCAPTIONMARGIN));
      S := Item.GetShortCutText;
      if Length(S) > 0 then Inc(AWidth, (AHeight - 6) + GetTextWidth(Canvas.Handle, S, True));
      Inc(AWidth, AHeight); { Note: maybe this should be controlled by the theme }
    end;

  if Item is TTBXCustomItem then with TTBXCustomItem(Item) do
  begin
    if AWidth < MinWidth then AWidth := MinWidth;
    if AHeight < MinHeight then AHeight := MinHeight;
  end;
end;

constructor TTBXItemViewer.Create(AView: TTBView; AItem: TTBCustomItem; AGroupLevel: Integer);
begin
  inherited;
  FWide := True;
end;

procedure TTBXItemViewer.DoAdjustFont(AFont: TFont; StateFlags: Integer);
begin
  if tboDefault in Item.EffectiveOptions then with AFont do Style := Style + [fsBold];
  if Item is TTBXCustomItem then
    with TTBXCustomItem(Item) do
    begin
      FontSettings.Apply(AFont);
      if Assigned(FOnAdjustFont) then FOnAdjustFont(Item, Self, AFont, StateFlags);
    end
  else if Item is TTBXEditItem then
    with TTBXEditItem(Item) do
    begin
      FontSettings.Apply(AFont);
    end;
end;

procedure TTBXItemViewer.DoPaintCaption(Canvas: TCanvas; const ClientAreaRect: TRect;
  var CaptionRect: TRect; IsTextRotated: Boolean; var PaintDefault: Boolean);
begin
  // do nothing
end;

procedure TTBXItemViewer.DrawItemImage(Canvas: TCanvas; ARect: TRect; ItemInfo: TTBXItemInfo);
var
  ImgList: TCustomImageList;
  I: TTBXCustomItem;
begin
  ImgList := GetImageList;
  if (ImgList <> nil) and (Item.ImageIndex >= 0) and (Item.ImageIndex < ImgList.Count) then
  begin
    CurrentTheme.PaintImage(Canvas, ARect, ItemInfo, ImgList, Item.ImageIndex);
    if Item is TTBXCustomItem then
    begin
      I := TTBXCustomItem(Item);
      if Assigned(I.FOnDrawImage) then
        I.FOnDrawImage(I, Self, Canvas, ARect,
          CurrentTheme.GetImageOffset(Canvas, ItemInfo, ImgList),
          GetStateFlags(ItemInfo));
    end;
  end;
end;

function TTBXItemViewer.GetAccRole: Integer;
{ Returns the MSAA "role" of the viewer. }
const
  { Constants from OleAcc.h }
  ROLE_SYSTEM_BUTTONDROPDOWNGRID = $3A;
begin
  Result := inherited GetAccRole;
  if (Item is TTBXCustomItem) and TTBXCustomItem(Item).ToolBoxPopup and
    (tbisSubmenu in TTBXCustomItem(Item).ItemStyle) then
    Result := ROLE_SYSTEM_BUTTONDROPDOWNGRID;
end;

function TTBXItemViewer.GetImageShown: Boolean;
begin
  Result := (Item.ImageIndex >= 0) and
    ((Item.DisplayMode in [nbdmDefault, nbdmImageAndText]) or
    (IsToolbarStyle and (Item.DisplayMode = nbdmTextOnlyInMenus)));
end;

function TTBXItemViewer.GetImageSize: TSize;
var
  ImgList: TCustomImageList;
begin
  ImgList := GetImageList;
  with Result do if ImgList <> nil then
  begin
    CX := ImgList.Width;
    CY := ImgList.Height;
  end
  else
  begin
    CX := 0;
    CY := 0;
  end;
end;

function TTBXItemViewer.GetItemType: Integer;
begin
  if IsToolbarStyle then Result := IT_TOOLBARBUTTON
  else Result := IT_MENUITEM;
end;

function TTBXItemViewer.GetTextFlags: Cardinal;
begin
  Result := 0;
  if not AreKeyboardCuesEnabled and (vsUseHiddenAccels in View.Style) and
    not (vsShowAccels in View.State) then Result := DT_HIDEPREFIX;
end;

function TTBXItemViewer.GetTextSize(Canvas: TCanvas; const Text: string;
  TextFlags: Cardinal; Rotated: Boolean; StateFlags: Integer): TSize;
var
  DC: HDC;
  R: TRect;
  RotatedFont, SaveFont: HFONT;
  TextMetric: TTextMetric;
begin
  { note: rotated font size is consistent only for single-line captions! }
  if Length(Text) = 0 then with Result do
  begin
    CX := 0;
    CY := 0;
    Exit;
  end;

  { Select proper font }
  Canvas.Font := TTBViewAccess(View).GetFont;
  DoAdjustFont(Canvas.Font, StateFlags);

  if not Rotated then with R, Result do
  begin
    Left := 0; Right := 1;
    Top := 0; Bottom := 0;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), R, TextFlags or DT_CALCRECT);
    CX := Right;
    CY := Bottom;
  end
  else
  begin
    DC := Canvas.Handle;
    RotatedFont := CreateRotatedFont(DC);
    SaveFont := SelectObject(DC, RotatedFont);
    GetTextMetrics(DC, TextMetric);
    Result.CX := TextMetric.tmHeight;
    Result.CY := GetTextWidth(DC, Text, True);
    SelectObject(DC, SaveFont);
    DeleteObject(RotatedFont);
  end;
end;

function TTBXItemViewer.IsPtInButtonPart(X, Y: Integer): Boolean;
var
  W: Integer;
begin
  Result := not (tbisSubmenu in TTBItemAccess(Item).ItemStyle);
  if (tbisCombo in TTBItemAccess(Item).ItemStyle) then
  begin
    if IsToolbarStyle then W := CurrentTheme.GetIntegerMetrics(Self, TMI_SPLITBTN_ARROWWIDTH)
    else W := GetSystemMetricsForControl(View.Window, SM_CXMENUCHECK);
    Result := X < (BoundsRect.Right - BoundsRect.Left) - W;
  end;
end;

function TTBXItemViewer.IsToolbarSize: Boolean;
begin
  Result := inherited IsToolbarSize;
  Result := Result or ((GetViewType(View) and PVT_TOOLBOX) = PVT_TOOLBOX);
end;

function TTBXItemViewer.IsToolbarStyle: Boolean;
begin
  Result := inherited IsToolbarStyle;
  Result := Result or ((GetViewType(View) and PVT_TOOLBOX) = PVT_TOOLBOX);
end;

procedure TTBXItemViewer.MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean);
begin
  inherited;
  { Force the item to behave as a 'normal' menu item
    That is make it respond to mouse as an item with IsToolbarStyle = False }
  if Item.Enabled and not ((tbisSubmenu in TTBItemAccess(Item).ItemStyle) and
    not IsPtInButtonPart(X, Y)) then
  begin
    if View.MouseOverSelected then
    begin
      Execute(True);
    end;
  end;
end;

procedure TTBXItemViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsHoverItem, IsPushed, UseDisabledShadow: Boolean);
const
  CToolbarStyle: array [Boolean] of Integer = (0, IO_TOOLBARSTYLE);
  CCombo: array [Boolean] of Integer = (0, IO_COMBO);
  CSubmenuItem: array [Boolean] of Integer = (0, IO_SUBMENUITEM);
  CDesigning: array [Boolean] of Integer = (0, IO_DESIGNING);
  CAppActive: array [Boolean] of Integer = (0, IO_APPACTIVE);
var
  Item: TTBXCustomItem;
  View: TTBViewAccess;
  ItemInfo: TTBXItemInfo;

  R: TRect;
  ComboRect: TRect;
  CaptionRect: TRect;
  ImageRect: TRect;
  C: TColor;

  ToolbarStyle: Boolean;
  HasArrow: Boolean;
  IsSplit: Boolean;
  ImageIsShown: Boolean;
  ImageOrCheckShown: Boolean;
  ImgAndArrowWidth: Integer;
  ImgSize: TSize;
  IsComboPushed: Boolean;
  IsCaptionShown: Boolean;
  IsTextRotated: Boolean;
  ItemLayout: TTBXItemLayout;
  PaintDefault: Boolean;
  S: string;
  StateFlags: Integer;
  IsSpecialDropDown: Boolean;
  TextFlags: Cardinal;
  TextMetrics: TTextMetric;
  TextSize: TSize;
  Margins: TTBXMargins;
  Three: Integer;
begin
  Item := TTBXCustomItem(Self.Item);
  View := TTBViewAccess(Self.View);

  ToolbarStyle := IsToolbarStyle;
  IsSplit := tbisCombo in Item.ItemStyle;
  IsComboPushed := IsSplit and IsPushed and not View.Capture;
  if IsComboPushed then IsPushed := False;

  if GetImageShown then
  begin
    ImgSize := GetImageSize;
    with ImgSize do if (CX <= 0) or (CY <= 0) then
    begin
      CX := 0;
      CY := 0;
      ImageIsShown := False;
    end
    else ImageIsShown := True;
  end
  else
  begin
    ImgSize.CX := 0;
    ImgSize.CY := 0;
    ImageIsShown := False;
  end;
  IsSplit := tbisCombo in Item.ItemStyle;

  FillChar(ItemInfo, SizeOf(ItemInfo), 0);
  ItemInfo.Control := View.Window;
  ItemInfo.ViewType := GetViewType(View);
  ItemInfo.ItemOptions := CToolbarStyle[ToolbarStyle] or CCombo[IsSplit] or
    CDesigning[csDesigning in Item.ComponentState] or CSubmenuItem[tbisSubmenu in Item.ItemStyle] or
    CAppActive[Application.Active];
  ItemInfo.Enabled := Item.Enabled or View.Customizing;
  ItemInfo.Pushed := IsPushed;
  ItemInfo.Selected := Item.Checked;
  ItemInfo.ImageShown := ImageIsShown;
  ItemInfo.ImageWidth := ImgSize.CX;
  ItemInfo.ImageHeight := ImgSize.CY;
  if IsHoverItem then
  begin
    if not ItemInfo.Enabled and not View.MouseOverSelected then ItemInfo.HoverKind := hkKeyboardHover
    else if ItemInfo.Enabled then ItemInfo.HoverKind := hkMouseHover;
  end
  else ItemInfo.HoverKind := hkNone;
  ItemInfo.IsPopupParent := ToolbarStyle and
    (((vsModal in View.State) and Assigned(View.OpenViewer)) or (tbisSubmenu in Item.ItemStyle)) and
    ((IsSplit and IsComboPushed) or (not IsSplit and IsPushed));
  ItemInfo.IsVertical := (View.Orientation = tbvoVertical) and not IsSplit;
  ItemInfo.PopupMargin := GetPopupMargin(Self);

  ItemLayout := Item.Layout;
  if ItemLayout = tbxlAuto then
  begin
    if tboImageAboveCaption in Item.EffectiveOptions then ItemLayout := tbxlGlyphTop
    else if View.Orientation <> tbvoVertical then ItemLayout := tbxlGlyphLeft
    else ItemLayout := tbxlGlyphTop;
  end;

  HasArrow := (tbisSubmenu in Item.ItemStyle) and
    ((tbisCombo in Item.ItemStyle) or (tboDropdownArrow in Item.EffectiveOptions));

  if GetImageShown then
  begin
    ImgSize := GetImageSize;
    with ImgSize do if (CX <= 0) or (CY <= 0) then
    begin
      CX := 0;
      CY := 0;
      ImageIsShown := False;
    end
    else ImageIsShown := True;
  end
  else
  begin
    ImgSize.CX := 0;
    ImgSize.CY := 0;
    ImageIsShown := False;
  end;
  ImageOrCheckShown := ImageIsShown or (not ToolbarStyle and Item.Checked);

  StateFlags := GetStateFlags(ItemInfo);

  Canvas.Font := TTBViewAccess(View).GetFont;
  Canvas.Font.Color := CurrentTheme.GetItemTextColor(ItemInfo);
  DoAdjustFont(Canvas.Font, StateFlags);
  C := Canvas.Font.Color;

  { Setup font }
  TextFlags := GetTextFlags;
  IsCaptionShown := CaptionShown;
  IsTextRotated := (View.Orientation = tbvoVertical) and ToolbarStyle;
  if IsCaptionShown then
  begin
    S := GetCaptionText;
    if (Item.Layout <> tbxlAuto) or (tboImageAboveCaption in Item.EffectiveOptions) then
      IsTextRotated := False;
    if IsTextRotated or not ToolbarStyle then TextFlags := TextFlags or DT_SINGLELINE;
    TextSize := GetTextSize(Canvas, S, TextFlags, IsTextRotated, StateFlags);
  end
  else
  begin
    StateFlags := 0;
    SetLength(S, 0);
    IsTextRotated := False;
    TextSize.CX := 0;
    TextSize.CY := 0;
  end;

  IsSpecialDropDown := HasArrow and not IsSplit and ToolbarStyle and
    ((Item.Layout = tbxlGlyphTop) or (Item.Layout = tbxlAuto) and (tboImageAboveCaption in Item.EffectiveOptions)) and
    (ImgSize.CX > 0) and not (IsTextRotated) and (TextSize.CX > 0);

  { Border & Arrows }
  R := ClientAreaRect;
  with CurrentTheme do if ToolbarStyle then
  begin
    GetMargins(MID_TOOLBARITEM, Margins);
    if HasArrow then with R do
    begin
      ItemInfo.ComboPart := cpCombo;
      if IsSplit then
      begin
        ItemInfo.ComboPart := cpSplitLeft;
        ComboRect := R;
        Dec(Right, GetIntegerMetrics(Self, TMI_SPLITBTN_ARROWWIDTH));
        ComboRect.Left := Right;
      end
      else if not IsSpecialDropDown then
      begin
        if View.Orientation <> tbvoVertical then
        begin
          ComboRect :=
            Rect(
              Right - GetIntegerMetrics(Self, TMI_DROPDOWN_ARROWWIDTH) -
                GetIntegerMetrics(Self, TMI_DROPDOWN_ARROWMARGIN), 0,
              Right - GetIntegerMetrics(Self, TMI_DROPDOWN_ARROWMARGIN), Bottom);
        end
          else
        begin
          ComboRect :=
            Rect(0,
              Bottom - GetIntegerMetrics(Self, TMI_DROPDOWN_ARROWWIDTH) -
                GetIntegerMetrics(Self, TMI_DROPDOWN_ARROWMARGIN),
              Right, Bottom - GetIntegerMetrics(Self, TMI_DROPDOWN_ARROWMARGIN));
        end;
      end
      else
      begin
        ImgAndArrowWidth := ImgSize.CX + GetIntegerMetrics(Self, TMI_DROPDOWN_ARROWWIDTH) + 2;
        ComboRect.Right := (R.Left + R.Right + ImgAndArrowWidth + 2) div 2;
        ComboRect.Left := ComboRect.Right - GetIntegerMetrics(Self, TMI_DROPDOWN_ARROWWIDTH);
        ComboRect.Top := (R.Top + R.Bottom - ImgSize.cy - 2 - TextSize.CY) div 2;
        ComboRect.Bottom := ComboRect.Top + ImgSize.CY;
      end;
    end
    else SetRectEmpty(ComboRect);

    if not IsSplit then
    begin
      PaintButton(Canvas, R, ItemInfo);
      if HasArrow then
      begin
        PaintDropDownArrow(Canvas, ComboRect, ItemInfo);
        if not IsSpecialDropDown then
        begin
          if View.Orientation <> tbvoVertical then Dec(R.Right, GetIntegerMetrics(Self, TMI_DROPDOWN_ARROWWIDTH))
          else Dec(R.Bottom, GetIntegerMetrics(Self, TMI_DROPDOWN_ARROWWIDTH));
        end;
      end;
    end
    else // IsSplit
    begin
      CurrentTheme.PaintButton(Canvas, R, ItemInfo);
      ItemInfo.Pushed := IsComboPushed;
      ItemInfo.Selected := False;
      ItemInfo.ComboPart := cpSplitRight;
      CurrentTheme.PaintButton(Canvas, ComboRect, ItemInfo);
      ItemInfo.ComboPart := cpSplitLeft;
      ItemInfo.Pushed := IsPushed;
      ItemInfo.Selected := Item.Checked;
    end;
    InflateRect(R, -2, -2);
  end
  else // not toolbar style
  begin
    GetMargins(MID_MENUITEM, Margins);
    PaintMenuItem(Canvas, R, ItemInfo);
    Inc(R.Left, Margins.LeftWidth);
    Dec(R.Right, Margins.RightWidth);
    Inc(R.Top, Margins.TopHeight);
    Dec(R.Bottom, Margins.BottomHeight);
  end;

  { Caption }
  if IsCaptionShown then
  begin
    if ToolbarStyle then
    begin
      CaptionRect := R;
      TextFlags := TextFlags or DT_CENTER or DT_VCENTER;
      Three := ScaleByPixelsPerInch(3, View.GetMonitor);
      if ImageIsShown then with CaptionRect do
        case ItemLayout of
        tbxlGlyphLeft:
          begin
            Inc(Left, ImgSize.CX + Three + 1);
            Top := (Top + Bottom  - TextSize.CY) div 2;
            Bottom := Top + TextSize.CY;
            Left := (Left + Right - TextSize.CX) div 2;
            Right := Left + TextSize.CX;
            TextFlags := TextFlags and not DT_CENTER;
          end;
        tbxlGlyphTop:
          begin
            Inc(Top, ImgSize.CY + 1);
            if IsTextRotated then Inc(CaptionRect.Top, 3);
            Top := (Top + Bottom  - TextSize.CY) div 2;
            Bottom := Top + TextSize.CY;
            Left := (Left + Right - TextSize.CX) div 2;
            Right := Left + TextSize.CX;
            TextFlags := TextFlags and not DT_VCENTER;
          end;
        end
      else
      begin
        with CaptionRect, TextSize do
        begin
          Left := (Left + R.Right - CX) div 2;
          Top := (Top + R.Bottom - CY) div 2;
          Right := Left + CX;
          Bottom := Top + CY;
        end;
      end;

      Canvas.Font.Color := C;
      PaintDefault := True;
      DoPaintCaption(Canvas, ClientAreaRect, CaptionRect, IsTextRotated, PaintDefault);
      if PaintDefault then
        CurrentTheme.PaintCaption(Canvas, CaptionRect, ItemInfo, S, TextFlags, IsTextRotated);
    end
    else with CurrentTheme do
    begin
      TextFlags := DT_LEFT or DT_VCENTER or TextFlags;
      TextSize := GetTextSize(Canvas, S, TextFlags, False, StateFlags); { TODO : Check if this line is required }
      GetTextMetrics(Canvas.Handle, TextMetrics);

      CaptionRect := R;
      Inc(CaptionRect.Left,
          ItemInfo.PopupMargin + GetIntegerMetrics(Self, TMI_MENU_IMGTEXTSPACE) +
            GetIntegerMetrics(Self, TMI_MENU_LCAPTIONMARGIN));
      with TextMetrics, CaptionRect do
        if (Bottom - Top) - (tmHeight + tmExternalLeading) = Margins.BottomHeight then Dec(Bottom);
      Inc(CaptionRect.Top, TextMetrics.tmExternalLeading);

      CaptionRect.Right := CaptionRect.Left + TextSize.CX;

      Canvas.Font.Color := C;
      PaintDefault := True;
      DoPaintCaption(Canvas, ClientAreaRect, CaptionRect, IsTextRotated, PaintDefault);
      if PaintDefault then
        CurrentTheme.PaintCaption(Canvas, CaptionRect, ItemInfo, S, TextFlags, IsTextRotated);
    end;
  end;

  { Shortcut and/or submenu arrow (menus only) }
  if not ToolbarStyle then
  begin
    S := Item.GetShortCutText;
    if Length(S) > 0 then
    begin
      CaptionRect := R;
      with CaptionRect, TextMetrics do
      begin
        Left := Right - (Bottom - Top) - GetTextWidth(Canvas.Handle, S, True);
        if (Bottom - Top) - (tmHeight + tmExternalLeading) = Margins.BottomHeight then Dec(Bottom);
        Inc(Top, TextMetrics.tmExternalLeading);
      end;
      Canvas.Font.Color := C;
      PaintDefault := True;
      DoPaintCaption(Canvas, ClientAreaRect, CaptionRect, IsTextRotated, PaintDefault);
      if PaintDefault then
        CurrentTheme.PaintCaption(Canvas, CaptionRect, ItemInfo, S, TextFlags, False);
    end;
  end;

  { Image, or check box }
  if ImageOrCheckShown then
  begin
    ImageRect := R;

    if ToolBarStyle then
    begin
      if IsSpecialDropDown then
        OffsetRect(ImageRect, (-CurrentTheme.GetIntegerMetrics(Self, TMI_DROPDOWN_ARROWWIDTH) + 1) div 2, 0);

      if ItemLayout = tbxlGlyphLeft then ImageRect.Right := ImageRect.Left + ImgSize.CX + 2
      else
      begin
        ImageRect.Top := (ImageRect.Top + ImageRect.Bottom - ImgSize.cy - 2 - TextSize.cy) div 2;
        ImageRect.Bottom := ImageRect.Top + ImgSize.CY;
      end;
    end
    else ImageRect.Right := ImageRect.Left + ClientAreaRect.Bottom - ClientAreaRect.Top;

    if ImageIsShown then with ImageRect, ImgSize do
    begin
      Left := Left + ((Right - Left) - CX) div 2;
      ImageRect.Top := Top + ((Bottom - Top) - CY) div 2;
      Right := Left + CX;
      Bottom := Top + CY;
      DrawItemImage(Canvas, ImageRect, ItemInfo);
    end
    else
      if not ToolbarStyle and Item.Checked then
      begin
        if Item.RadioItem then
          with ItemInfo do ItemOptions := ItemOptions or IO_RADIO;
        CurrentTheme.PaintCheckMark(Canvas, ImageRect, ItemInfo);
      end;
  end;
end;


//============================================================================//

{ TTBXSubmenuItem }

constructor TTBXSubmenuItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle + [tbisSubMenu, tbisSubitemsEditable];
end;

function TTBXSubmenuItem.GetDropdownCombo: Boolean;
begin
  Result := tbisCombo in ItemStyle;
end;

procedure TTBXSubmenuItem.SetDropdownCombo(Value: Boolean);
begin
  if (tbisCombo in ItemStyle) <> Value then begin
    if Value then ItemStyle := ItemStyle + [tbisCombo]
    else ItemStyle := ItemStyle - [tbisCombo];
    Change (True);
  end;
end;



//============================================================================//

{ TTBXSeparatorItem }

constructor TTBXSeparatorItem.Create(AOwner: TComponent);
begin
  inherited;
  FSize := -1;  // use default from as in TTBSeparatorItem
end;

function TTBXSeparatorItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TTBXSeparatorItemViewer;
end;

procedure TTBXSeparatorItem.SetSize(Value: Integer);
begin
  if Value < -1 then Value := -1;
  if Value <> FSize then
  begin
    FSize := Value;
    Change(True);
  end;
end;


//============================================================================//

{ TTBXSeparatorItemViewer }

procedure TTBXSeparatorItemViewer.CalcSize(const Canvas: TCanvas;
  var AWidth, AHeight: Integer);
var
  SZ: Integer;
begin
  SZ := TTBXSeparatorItem(Item).Size;
  if SZ < 0 then
  begin
    if not IsToolbarStyle then SZ := CurrentTheme.GetIntegerMetrics(Self, TMI_MENU_SEPARATORSIZE)
    else SZ := CurrentTheme.GetIntegerMetrics(Self, TMI_TLBR_SEPARATORSIZE);
    if SZ < 0 then inherited CalcSize(Canvas, AWidth, AHeight)
    else
    begin
      AWidth := SZ;
      AHeight := SZ;
    end;
  end
  else if not IsToolbarStyle then
  begin
    AHeight := SZ;
    AWidth := 0;
  end
  else
  begin
    AWidth := SZ;
    AHeight := SZ;
  end;
end;

function TTBXSeparatorItemViewer.IsToolbarSize: Boolean;
begin
  Result := inherited IsToolbarSize;
  Result := Result or ((GetViewType(View) and PVT_TOOLBOX) = PVT_TOOLBOX);
end;

function TTBXSeparatorItemViewer.IsToolbarStyle: Boolean;
begin
  Result := inherited IsToolbarStyle;
  Result := Result or ((GetViewType(View) and PVT_TOOLBOX) = PVT_TOOLBOX);
end;

procedure TTBXSeparatorItemViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsHoverItem, IsPushed, UseDisabledShadow: Boolean);
const
  CToolbarStyle: array [Boolean] of Integer = (0, IO_TOOLBARSTYLE);
  CDesigning: array [Boolean] of Integer = (0, IO_DESIGNING);
var
  Item: TTBXSeparatorItem;
  ItemInfo: TTBXItemInfo;
  R: TRect;
  LineSep, HorzLine: Boolean;
begin
  Item := TTBXSeparatorItem(Self.Item);
  if Item.Size = 0 then Exit;

  FillChar(ItemInfo, SizeOf(TTBXItemInfo), 0);
  ItemInfo.ViewType := GetViewType(View);
  ItemInfo.ItemOptions := CToolbarStyle[IsToolbarStyle] or
    CDesigning[csDesigning in Item.ComponentState];
  ItemInfo.Enabled := not Item.Blank;
  ItemInfo.Pushed := IsPushed;
  ItemInfo.Selected := False;
  ItemInfo.ImageShown := False;
  ItemInfo.ImageWidth := 0;
  ItemInfo.ImageHeight := 0;
  ItemInfo.IsVertical := View.Orientation = tbvoVertical;
  if not IsToolbarStyle then ItemInfo.PopupMargin := GetPopupMargin(Self);

  R := ClientAreaRect;
  LineSep := tbisLineSep in State;
  with ItemInfo do
  begin
    HorzLine := (IsVertical xor LineSep) or View.IsPopup;
    if (((ViewType and VT_POPUP) = VT_POPUP) and
      ((ViewType and PVT_CHEVRONMENU) = PVT_CHEVRONMENU)) then
      HorzLine := (HorzLine and LineSep);
  end;
  CurrentTheme.PaintSeparator(Canvas, R, ItemInfo, HorzLine, LineSep);
end;

//============================================================================//

//============================================================================//

{ TTBXPopupWindow }

procedure TTBXPopupWindow.CMHintShow(var Message: TCMHintShow);
begin
  with Message.HintInfo^ do
  begin
    HintStr := '';
    if Assigned(View.Selected) then
    begin
      CursorRect := View.Selected.BoundsRect;
      HintStr := View.Selected.GetHintText;
      View.Selected.Dispatch(Message);
    end;
  end;
end;

procedure TTBXPopupWindow.CMShowingChanged(var Message: TMessage);
const
  ShowFlags: array[Boolean] of UINT = (
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_HIDEWINDOW,
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_SHOWWINDOW);
var
  MenuAni: TMenuAnimation;
  AniDir: TTBAnimationDirection;
begin
  { Must override TCustomForm/TForm's CM_SHOWINGCHANGED handler so that the
    form doesn't get activated when Visible is set to True. }

  { Handle animation. NOTE: I do not recommend trying to enable animation on
    Windows 95 and NT 4.0 because there's a difference in the way the
    SetWindowPos works on those versions. See the comment in the
    TBStartAnimation function of TB2Anim.pas. }
  if ((View.ParentView = nil) or not(vsNoAnimation in View.ParentView.State)) and
     Showing and (View.Selected = nil) and not IsWindowVisible(WindowHandle) and
     (TBXMenuAnimation.AnimationMode <> amNone) then
  begin
    { Start animation only if WM_TB2K_POPUPSHOWING returns zero (or not handled) }
    if SendMessage(WindowHandle, WM_TB2K_POPUPSHOWING, TPS_ANIMSTART, 0) = 0 then
    begin
      MenuAni := TBXMenuAnimation.MenuAnimation;
      { MP (do not animate if disallowed by system-wide config) }
      if MenuAni <> maNone then
      begin
        AniDir := TTBPopupWindowAccess(Self).AnimationDirection;
        if MenuAni = maUnfold then
          if [tbadDown, tbadUp] * AniDir <> []
            then Include(AniDir, tbadRight)
            else Include(AniDir, tbadDown);
        TBStartAnimation(WindowHandle, MenuAni = maFade, AniDir);
        Exit;
      end;
    end;
  end;

  { No animation... }
  if not Showing then begin
    { Call TBEndAnimation to ensure WS_EX_LAYERED style is removed before
      hiding, otherwise windows under the popup window aren't repainted
      properly. }
    TBEndAnimation(WindowHandle);
  end;
  SetWindowPos(WindowHandle, 0, 0, 0, 0, 0, ShowFlags[Showing]);
  if Showing then SendNotifyMessage(WindowHandle, WM_TB2K_POPUPSHOWING, TPS_NOANIM, 0);
end;

procedure TTBXPopupWindow.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.Style := WindowClass.Style and not (CS_DROPSHADOW or CS_DBLCLKS);
    if GetShowShadow and (CurrentTheme.GetPopupShadowType = PST_WINDOWSXP) then
    begin
      WindowClass.Style := WindowClass.Style or CS_DROPSHADOW;
      StrPCopy(WinClassName, ClassName + 'S');
    end;
  end;
end;

procedure TTBXPopupWindow.CreateShadow;
var
  PR: TRect;
  ParentViewer: TTBItemViewer;
  VT: Integer;
  ChevronParent: Boolean;
begin
  PR := Rect(0, 0, 0, 0);
  if CurrentTheme.GetPopupShadowType = PST_OFFICEXP then
  begin
    if (View <> nil) and (View.ParentView <> nil) then
    begin
      ParentViewer := TTBViewAccess(View.ParentView).OpenViewer;
      ChevronParent := Self is TTBXChevronPopupWindow;
      if ((ParentViewer is TTBXItemViewer) or ChevronParent) then
      begin
        VT := GetViewType(ParentViewer.View);
        if ((VT and PVT_POPUPMENU) <> PVT_POPUPMENU) or ChevronParent then
        begin
          PR := ParentViewer.BoundsRect;
          PR.TopLeft := View.ParentView.Window.ClientToScreen(PR.TopLeft);
          PR.BottomRight := View.ParentView.Window.ClientToScreen(PR.BottomRight);
        end;
      end;
    end
    else if not IsRectEmpty(FControlRect) then
    begin
      PR := FControlRect;
    end;
  end;
  FShadows := TShadows.Create(PR, BoundsRect, 4, 61, TBXLoColor);
  FShadows.Show(Handle);
end;

destructor TTBXPopupWindow.Destroy;
begin
  DestroyShadow;
  inherited;
end;

procedure TTBXPopupWindow.DestroyShadow;
var
  SaveShadows: TObject;
begin
  SaveShadows := FShadows;
  FShadows := nil;
  SaveShadows.Free;
end;

function TTBXPopupWindow.GetFillColor: TColor;
begin
  Result := CurrentTheme.GetViewColor(GetViewType(View));
  View.BackgroundColor := Result;
end;

function TTBXPopupWindow.GetNCSize: TPoint;
begin
  Result := inherited GetNCSize;
  CurrentTheme.GetViewBorder(Self, GetViewType(View), Result);
end;

function TTBXPopupWindow.GetShowShadow: Boolean;
begin
  Result := ((GetViewType(View) and PVT_LISTBOX) <> PVT_LISTBOX );
end;

function TTBXPopupWindow.GetViewClass: TTBViewClass;
begin
  Result := TTBXPopupView;
end;

procedure TTBXPopupWindow.PaintScrollArrows;

  function _GetPopupMargin: Integer;
  begin
    if View.ParentView <> nil then
      Result := GetPopupMargin(TTBViewAccess(View.ParentView).OpenViewer)
    else if View.ViewerCount > 0 then
      Result := GetPopupMargin(View.Viewers[0])
    else Result := -1;
  end;

  procedure DrawArrows;
  var
    ItemInfo: TTBXItemInfo;
    Index: Integer;
  begin
    FillChar(ItemInfo, SizeOf(ItemInfo), 0);
    ItemInfo.ViewType := PVT_POPUPMENU;
    ItemInfo.Enabled := True;
    ItemInfo.PopupMargin := _GetPopupMargin;
    if ItemInfo.PopupMargin > 0 then
    begin
      if TTBViewAccess(View).ShowUpArrow then
        for Index := 0 to View.ViewerCount- 1 do
          if View.Viewers[Index].Show then
          begin
            CurrentTheme.PaintMenuItemFrame(Canvas, Rect(0, 0, ClientWidth,
              View.Viewers[Index].BoundsRect.Top), ItemInfo);
            Break;
          end;
      if TTBViewAccess(View).ShowDownArrow then
        for Index := View.ViewerCount- 1 downto 0 do
          if View.Viewers[Index].Show then
          begin
            CurrentTheme.PaintMenuItemFrame(Canvas, Rect(0,
              View.Viewers[Index].BoundsRect.Bottom, ClientWidth,
                ClientHeight), ItemInfo);
            Break;
          end;
    end;
  end;

begin
  with TTBViewAccess(View) do
    if ShowUpArrow or ShowDownArrow then
      DrawArrows;
  inherited;
end;

procedure TTBXPopupWindow.TBMGetViewType(var Message: TMessage);
var
  PI: TTBCustomItem;
begin
  Message.Result := PVT_POPUPMENU;
  if View <> nil then
    if Self is TTBXChevronPopupWindow then
      Message.Result := PVT_CHEVRONMENU
    else
    begin
      PI := View.ParentItem;
      if PI <> nil then
      begin
        if (PI.Count = 1) and (PI.Items[0] is TTBXCustomList) then
          Message.Result := PVT_LISTBOX
        else if PI is TTBXEditItem then
        begin
          Message.Result := PVT_TOOLBOX;
        end
        else if (PI is TTBXCustomItem) and (TTBXCustomItem(PI).ToolBoxPopup) then
          Message.Result := PVT_TOOLBOX
      end;
    end;
end;

procedure TTBXPopupWindow.WMEraseBkgnd(var Message: TWmEraseBkgnd);
var
  Canvas: TCanvas;
  R: TRect;
begin
  TBEndAnimation(WindowHandle);
  Canvas := TCanvas.Create;
  Canvas.Handle := Message.DC;
  R := ClientRect;
  CurrentTheme.PaintBackgnd(Canvas, R, R, R, GetFillColor, False, GetViewType(View));
  Canvas.Handle := 0;
  Canvas.Free;
  Message.Result := 1;
end;

procedure TTBXPopupWindow.WMNCCalcSize(var Message: TWMNCCalcSize);
var
  Sz: TPoint;
begin
  CurrentTheme.GetViewBorder(Self, GetViewType(View), Sz);
  with Message.CalcSize_Params^.rgrc[0], Sz do
  begin
    Inc(Left, X);
    Inc(Top, Y);
    Dec(Right, X);
    Dec(Bottom, Y);
  end;
  Message.Result := 1;
end;

procedure TBXPopupNCPaintProc(Control: TControl; Wnd: HWND; DC: HDC; AppData: Longint);
var
  R, R2: TRect;
  Canvas: TCanvas;
  View: TTBPopupView;
  PopupInfo: TTBXPopupInfo;
  ParentViewer: TTBItemViewer;
begin
  Assert(DC <> 0, 'TBXPopupNCPaintProc');
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := DC;
    FillChar(PopupInfo, SizeOf(PopupInfo), 0);
    View := TTBPopupView(AppData);
    PopupInfo.WindowHandle := View.Window.Handle;
    PopupInfo.ViewType := GetViewType(View);
    if View.ParentView <> nil then
    begin
      ParentViewer := TTBViewAccess(View.ParentView).OpenViewer;
      if ((ParentViewer is TTBXItemViewer) or (View.Window is TTBXChevronPopupWindow))
        and TTBItemViewerAccess(ParentViewer).IsToolbarStyle then
      begin
        R := ParentViewer.BoundsRect;
        R.TopLeft := View.ParentView.Window.ClientToScreen(R.TopLeft);
        R.BottomRight := View.ParentView.Window.ClientToScreen(R.BottomRight);
        GetWindowRect(Wnd, R2);
        OffsetRect(R, -R2.Left, -R2.Top);
        PopupInfo.ParentRect := R;
      end;
    end
    else if View.ParentItem is TTBXRootItem then
    begin
      R := TTBXRootItem(View.ParentItem).FPopupControlRect;
      if not IsRectEmpty(R) then
      begin
        GetWindowRect(Wnd, R2);
        OffsetRect(R, -R2.Left, -R2.Top);
        PopupInfo.ParentRect := R;
      end;
    end;
    GetWindowRect(Wnd, R);
    OffsetRect(R, -R.Left, -R.Top);
    CurrentTheme.GetViewBorder(Control, PopupInfo.ViewType, PopupInfo.BorderSize);
    R2 := R;
    with PopupInfo.BorderSize do InflateRect(R2, -X, -Y);
    with R2 do ExcludeClipRect(Canvas.Handle, Left, Top, Right, Bottom);
    CurrentTheme.PaintPopupNCArea(Canvas, R, PopupInfo);
  finally
    Canvas.Handle := 0;
    Canvas.Free;
  end;
end;

procedure TTBXPopupWindow.WMNCPaint(var Message: TMessage);
var
  DC: HDC;
begin
  DC := GetWindowDC(Handle);
  try
    Assert(DC <> 0, 'TTBXPopupWindow.WMNCPaint');
    SelectNCUpdateRgn(Handle, DC, HRGN(Message.WParam));
    TBXPopupNCPaintProc(Self, Handle, DC, LongInt(Self.View));
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TTBXPopupWindow.WMPrint(var Message: TMessage);
begin
  HandleWMPrint(Self, Handle, Message, TBXPopupNCPaintProc, LongInt(Self.View));
end;

procedure TTBXPopupWindow.WMTB2kPopupShowing(var Message: TMessage);
begin
  if Message.WParam in [TPS_ANIMFINISHED, TPS_NOANIM] then
  begin
    if not (csDestroying in ComponentState) and GetShowShadow and
      (CurrentTheme.GetPopupShadowType in [PST_OFFICEXP, PST_WINDOWS2K]) then CreateShadow;
  end;
end;

procedure TTBXPopupWindow.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  with Message.WindowPos^ do
    if ((flags and SWP_SHOWWINDOW) = 0) and ((flags and SWP_HIDEWINDOW) = 0) then
    begin
      if FShadows <> nil then
      begin
        DestroyShadow;
        CreateShadow;
      end;
    end;
end;


//============================================================================//

{ TTBXToolbarView }

procedure TTBXToolbarView.GetMargins(AOrientation: TTBViewOrientation; var Margins: TRect);
var
  VT: Integer;
  M: TTBXMargins;
begin
  VT := GetWinViewType(TTBXToolbar(Owner));
  if (VT and VT_TOOLBAR) = VT_TOOLBAR then
  begin
    if AOrientation = tbvoFloating then VT := VT or TVT_FLOATING
    else VT := VT and not TVT_FLOATING
  end
  else if (VT and VT_DOCKPANEL) = VT_DOCKPANEL then
  begin
    if AOrientation = tbvoFloating then VT := VT or DPVT_FLOATING
    else VT := VT and not DPVT_FLOATING
  end;
  CurrentTheme.GetViewMargins(VT, M);
  Margins.Left := M.LeftWidth;
  Margins.Top := M.TopHeight;
  Margins.Right := M.RightWidth;
  Margins.Bottom := M.BottomHeight;
end;


//============================================================================//

{ TTBXToolbar }

procedure TTBXToolbar.CMColorChanged(var Message: TMessage);
begin
  UpdateEffectiveColor;
  if Docked and HandleAllocated then
  begin
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or
      RDW_ERASE or RDW_UPDATENOW or RDW_ALLCHILDREN);
  end;
  UpdateChildColors;
  Invalidate;
end;

procedure TTBXToolbar.CMControlChange(var Message: TCMControlChange);
begin
  inherited;
  if Message.Inserting and (Color = clNone) then
    Message.Control.Perform(CM_PARENTCOLORCHANGED, 1, EffectiveColor);
end;

procedure TTBXToolbar.CMParentColorChanged(var Message: TMessage);
begin
  if Embedded and (Color = clNone) then
  begin
    UpdateEffectiveColor;
    if (Message.WParam = 0) then
    begin
      Message.WParam := 1;
      Message.LParam := EffectiveColor;
    end;
  end;
  inherited;
  Invalidate;
end;

constructor TTBXToolbar.Create(AOwner: TComponent);
begin
  inherited;
  AddThemeNotification(Self);
  FEffectiveColor := Color;
  Color := clNone;
  ControlStyle := ControlStyle - [csOpaque];
  DblClickUndock := False;
end;

destructor TTBXToolbar.Destroy;
begin
  RemoveThemeNotification(Self);
  inherited;
end;

procedure TTBXToolbar.DrawNCArea(const DrawToDC: Boolean; const ADC: HDC; const Clip: HRGN);
var
  DC: HDC;
  R, CR, R2: TRect;
  ACanvas: TCanvas;
  ToolbarInfo: TTBXToolbarInfo;
  UsingBackground: Boolean;
begin
  if not Docked or not HandleAllocated then Exit;

  if not DrawToDC then DC := GetWindowDC(Handle)
  else DC := ADC;

  UsingBackground := TDockAccess(CurrentDock).UsingBackground;

  try
    GetToolbarInfo(ToolbarInfo);
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    if not DrawToDC then
    begin
      SelectNCUpdateRgn(Handle, DC, Clip);
      CR := R;
      with ToolbarInfo.BorderSize, CR do
      begin
        InflateRect(CR, -X, -Y);
        if ToolbarInfo.IsVertical then Inc(Top, GetTBXDragHandleSize(ToolbarInfo))
        else Inc(Left, GetTBXDragHandleSize(ToolbarInfo));
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
      end;
    end;

    ACanvas := TCanvas.Create;
    try
      ACanvas.Handle := DC;
      if CurrentTheme.SolidToolbarNCArea then
      begin
        ACanvas.Brush.Color := EffectiveColor;
        ACanvas.Brush.Style := bsSolid;
      end
      else if UsingBackground then
      begin
        ACanvas.Brush.Color := EffectiveColor;
        R2 := CurrentDock.ClientRect;
        OffsetRect(R2, -Left, -Top);
        TDockAccess(CurrentDock).DrawBackground(DC, R2);
      end
      else
      begin
        ACanvas.Brush.Color := GetEffectiveColor(CurrentDock);
        ACanvas.FillRect(R);
        ACanvas.Brush.Color := EffectiveColor;
        ACanvas.Brush.Style := bsSolid;
      end;

      CurrentTheme.PaintToolbarNCArea(GetMonitorFromControl(Self), ACanvas, R, ToolbarInfo);
    finally
      ACanvas.Handle := 0;
      ACanvas.Free;
    end;
  finally
    if not DrawToDC then ReleaseDC(Handle, DC);
  end;
end;

function TTBXToolbar.Embedded: Boolean;
begin
  Result := not (Floating or Docked);
end;

function TTBXToolbar.GetChevronItemClass: TTBChevronItemClass;
begin
  Result := TTBXChevronItem;
end;

function TTBXToolbar.GetFloatingBorderSize: TPoint;
begin
  CurrentTheme.GetViewBorder(Self, GetViewType(View) or TVT_FLOATING, Result);
end;

function TTBXToolbar.GetFloatingWindowParentClass: TTBFloatingWindowParentClass;
begin
  Result := TTBXFloatingWindowParent;
end;

procedure TTBXToolbar.GetToolbarInfo(out ToolbarInfo: TTBXToolbarInfo);
begin
  FillChar(ToolbarInfo, SizeOf(ToolbarInfo), 0);
  ToolbarInfo.WindowHandle := Handle;
  ToolbarInfo.ViewType := GetWinViewType(Self);
  if CurrentDock <> nil then
    ToolbarInfo.IsVertical := CurrentDock.Position in [dpLeft,dpRight];
  ToolbarInfo.AllowDrag := CurrentDock.AllowDrag;
  ToolbarInfo.DragHandleStyle := Ord(DragHandleStyle);
  ToolbarInfo.ClientWidth := ClientWidth;
  ToolbarInfo.ClientHeight := ClientHeight;
  if ToolbarInfo.AllowDrag and CloseButtonWhenDocked then
  begin
    ToolbarInfo.CloseButtonState := CDBS_VISIBLE;
    if CloseButtonDown then ToolbarInfo.CloseButtonState := ToolbarInfo.CloseButtonState or CDBS_PRESSED
    else if CloseButtonHover then ToolbarInfo.CloseButtonState := ToolbarInfo.CloseButtonState or CDBS_HOT;
  end;
  ToolbarInfo.BorderStyle := BorderStyle;
  CurrentTheme.GetViewBorder(Self, ToolbarInfo.ViewType, ToolbarInfo.BorderSize);
  ToolbarInfo.EffectiveColor := EffectiveColor;
end;

function TTBXToolbar.GetViewClass: TTBToolbarViewClass;
begin
  Result := TTBXToolbarView;
end;

procedure TTBXToolbar.SetItemTransparency(const Value: TTBXItemTransparency);
begin
  FItemTransparency := Value;
  Invalidate;
end;

procedure TTBXToolbar.Loaded;
begin
  inherited;
  UpdateEffectiveColor;
end;

procedure TTBXToolbar.SetParent(AParent: TWinControl);
begin
  inherited;
  if AParent is TTBXFloatingWindowParent then
  begin
    TTBXFloatingWindowParent(AParent).SnapDistance := SnapDistance;
    // See the comment in TTBXToolbar.Rebuild
    Rebuild;
  end;
end;

procedure TTBXToolbar.SetSnapDistance(Value: Integer);
begin
  if Value < 0 then Value := 0;
  FSnapDistance := Value;
  if (Parent <> nil) and (Parent is TTBXFloatingWindowParent) then
    TTBXFloatingWindowParent(Parent).SnapDistance := Value;
end;

procedure TTBXToolbar.TBMGetEffectiveColor(var Message: TMessage);
begin
  Message.WParam := EffectiveColor;
  Message.Result := 1;
end;

procedure TTBXToolbar.TBMGetViewType(var Message: TMessage);
begin
  if MenuBar then Message.Result := TVT_MENUBAR
  else Message.Result := TVT_NORMALTOOLBAR;
  if Floating then Message.Result := Message.Result or TVT_FLOATING;
  if Resizable then Message.Result := Message.Result or TVT_RESIZABLE;
  case ItemTransparency of
    itAuto:
      if not (Floating or Docked) then Message.Result := Message.Result or TVT_EMBEDDED;
    itDisable:
      Message.Result := Message.Result or TVT_EMBEDDED;
  end;
end;

procedure TTBXToolbar.Rebuild;
begin
  if Floating then
  begin
    // Is not set yet, when called from within SetParent > WMDpiChangedAfterParent while undocking on
    // per-monitor-DPI system. We will call Rebuild again at the end of the SetParent
    if Assigned(Parent) then
      UpdateNCArea(TTBXFloatingWindowParent(Parent), GetWinViewType(Self));
  end
    else
  begin
    UpdateNCArea(Self, GetWinViewType(Self));
  end;
  Invalidate;
  Arrange;
end;

procedure TTBXToolbar.TBMThemeChange(var Message: TMessage);
begin
  case Message.WParam of
    TSC_BEFOREVIEWCHANGE: BeginUpdate;
    TSC_AFTERVIEWCHANGE:
      begin
        EndUpdate;
        UpdateEffectiveColor;
        Rebuild;
        UpdateChildColors;
      end;
    TSC_APPACTIVATE, TSC_APPDEACTIVATE:
      if MenuBar then Invalidate;
  end;
end;

procedure TTBXToolbar.WMDpiChangedBeforeParent(var Message: TMessage);
begin
  BeginUpdate;
end;

procedure TTBXToolbar.WMDpiChangedAfterParent(var Message: TMessage);
begin
  EndUpdate;
  Rebuild;
end;

procedure TTBXToolbar.UpdateChildColors;
var
  M: TMessage;
begin
  M.Msg := CM_PARENTCOLORCHANGED;
  M.WParam := 1;
  M.LParam := EffectiveColor;
  M.Result := 0;
  Broadcast(M);
end;

procedure TTBXToolbar.UpdateEffectiveColor;
begin
  if Color = clNone then
  begin
    if Embedded and (Parent <> nil) then
      FEffectiveColor := GetEffectiveColor(Parent)
    else
      FEffectiveColor := CurrentTheme.GetViewColor(GetViewType(View));
  end
  else FEffectiveColor := Color;
end;

procedure TTBXToolbar.WMEraseBkgnd(var Message: TWmEraseBkgnd);
var
  Canvas: TCanvas;
  R, CR: TRect;
  Transparent: Boolean;
begin
  Transparent := False;
  CR := ClientRect;
  if Color = clNone then
    if Embedded then
    begin
      Transparent := True;
      DrawParentBackground(Self, Message.DC, CR);
    end;

  Canvas := TCanvas.Create;
  Canvas.Handle := Message.DC;
  try
    if Docked then
    begin
      R := CurrentDock.ClientRect;
      R.TopLeft := ScreenToClient(CurrentDock.ClientToScreen(R.TopLeft));
      R.BottomRight := ScreenToClient(CurrentDock.ClientToScreen(R.BottomRight));
    end
    else R := Rect(0, 0, 0, 0);
    CurrentTheme.PaintBackgnd(Canvas, R, CR, CR, EffectiveColor, Transparent, GetWinViewType(Self));
    Message.Result := 1;
  finally
    Canvas.Handle := 0;
    Canvas.Free;
  end;
end;

//============================================================================//

{ TTBXChevronItem }

function TTBXChevronItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TTBXChevronItemViewer;
end;

procedure TTBXChevronItem.GetPopupPosition(ParentView: TTBView;
  PopupWindow: TTBPopupWindow; var PopupPositionRec: TTBPopupPositionRec);
begin
  if CurrentTheme.OfficeXPPopupAlignment then with PopupPositionRec do
  begin
    GetOfficeXPPopupPosition1(PopupPositionRec);
    inherited GetPopupPosition(ParentView, PopupWindow, PopupPositionRec);
    GetOfficeXPPopupPosition2(PopupPositionRec);
  end
  else inherited;
end;

function TTBXChevronItem.GetPopupWindowClass: TTBPopupWindowClass;
begin
  Result := TTBXChevronPopupWindow;
end;


//============================================================================//

{ TTBXChevronItemViewer }

procedure TTBXChevronItemViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsHoverItem, IsPushed, UseDisabledShadow: Boolean);
const
  CDesigning: array [Boolean] of Integer = (0, IO_DESIGNING);
var
  ItemInfo: TTBXItemInfo;
begin
  FillChar(ItemInfo, SizeOf(TTBXItemInfo), 0);
  ItemInfo.ViewType := GetViewType(View);
  ItemInfo.ItemOptions := IO_TOOLBARSTYLE or CDesigning[csDesigning in Item.ComponentState];
  ItemInfo.Enabled := Item.Enabled or View.Customizing;
  ItemInfo.Pushed := IsPushed;
  ItemInfo.Selected := False;
  ItemInfo.ImageShown := False;
  ItemInfo.ImageWidth := 0;
  ItemInfo.ImageHeight := 0;
  ItemInfo.IsPopupParent := IsPushed;
  if IsHoverItem then
  begin
    if not ItemInfo.Enabled and not TTBViewAccess(View).MouseOverSelected then ItemInfo.HoverKind := hkKeyboardHover
    else if ItemInfo.Enabled then ItemInfo.HoverKind := hkMouseHover;
  end
  else ItemInfo.HoverKind := hkNone;
  ItemInfo.IsVertical := View.Orientation = tbvoVertical;
  CurrentTheme.PaintChevron(Canvas, ClientAreaRect, ItemInfo);
end;

function TTBXChevronItemViewer.CaptionShown: Boolean;
begin
  Result := False;
end;


//============================================================================//

{ TTBXRootItem }

function TTBXRootItem.CreatePopupEx(SelectFirstItem: Boolean;
  const AControlRect: TRect; Alignment: TTBPopupAlignment): TTBPopupWindow;
var
  SavePopupRect: TRect;
  Pt: TPoint;
begin
  SavePopupRect := FPopupControlRect;
  try
    FPopupControlRect := AControlRect;
    Pt.X := AControlRect.Left;
    Pt.Y := AControlRect.Bottom;
    Result := inherited CreatePopup(nil, nil, False, SelectFirstItem, False, Pt, Alignment);
    if Result is TTBXPopupWindow then TTBXPopupWindow(Result).FControlRect := FPopupControlRect;
  finally
    FPopupControlRect := SavePopupRect;
  end;
end;

procedure TTBXRootItem.GetPopupPosition(ParentView: TTBView;
  PopupWindow: TTBPopupWindow; var PopupPositionRec: TTBPopupPositionRec);
var
  Y2: Integer;
  VT: Integer;
begin
  if IsRectEmpty(FPopupControlRect) then inherited
  else with PopupPositionRec do
  begin
    ParentItemRect := FPopupControlRect;
    if Y + H > MonitorRect.Bottom then
    begin
      Y2 := FPopupControlRect.Top - H;
      if Y2 >= MonitorRect.Top then Y := Y2;
    end;
    if Y < MonitorRect.Top then Y := MonitorRect.Top
    else if Y + H > MonitorRect.Bottom then Y := MonitorRect.Bottom - H;

    if Alignment = tbpaRight then X := FPopupControlRect.Right - W;

    if X + W > MonitorRect.Right then X := MonitorRect.Right - W;
    if X < MonitorRect.Left then X := MonitorRect.Left;
  end;
  if CurrentTheme.OfficeXPPopupAlignment then with PopupPositionRec do
  begin
    GetOfficeXPPopupPosition1(PopupPositionRec);
    inherited GetPopupPosition(ParentView, PopupWindow, PopupPositionRec);
    GetOfficeXPPopupPosition2(PopupPositionRec);
    VT := GetWinViewType(PopupWindow);
    PopupPositionRec.PlaySound := not (VT and PVT_LISTBOX = PVT_LISTBOX);
  end
  else inherited;
end;

function TTBXRootItem.GetPopupWindowClass: TTBPopupWindowClass;
begin
  Result := TTBXPopupWindow;
end;

function TTBXRootItem.OpenPopupEx(const SelectFirstItem, TrackRightButton: Boolean;
  const ControlRect: TRect; const Alignment: TTBPopupAlignment;
  const ReturnClickedItemOnly: Boolean): TTBCustomItem;
var
  ModalHandler: TTBModalHandler;
  Popup: TTBPopupWindow;
  DoneActionData: TTBDoneActionData;
  State: TTBViewState;
  {MP}
  I: Integer;
  MinWidth: Integer;
  Border: TPoint;
  Control: TControl;
begin
  ModalHandler := TTBModalHandler.Create(0);
  try
    {MP BEGIN}
    // Make sure menu is not narrower than its popup control (button)
    if not IsRectEmpty(ControlRect) then
    begin
      // see TTBPopupView.AutoSize and TTBXPopupWindow.GetNCSize
      Control := FindVCLWindow(Point(ControlRect.Left, ControlRect.Top));
      CurrentTheme.GetViewBorder(Control, PVT_POPUPMENU, Border);
      MinWidth := ControlRect.Width - (2*Border.X);
      for I := 0 to Count - 1 do
      begin
        if Items[I] is TTBXCustomItem then
          if TTBXCustomItem(Items[I]).MinWidth < MinWidth then
            TTBXCustomItem(Items[I]).MinWidth := MinWidth;
      end
    end;
    {MP END}
    Popup := CreatePopupEx(SelectFirstItem, ControlRect, Alignment);
    try
      State := Popup.View.State;
      Include(State, vsIgnoreFirstMouseUp);
      TTBViewAccess(Popup.View).SetState(State);
      ModalHandler.Loop(Popup.View, False, False, False, TrackRightButton);
      DoneActionData := TTBViewAccess(Popup.View).DoneActionData;
    finally
      { Remove vsModal state from the root view before any TTBView.Destroy
        methods get called, so that NotifyFocusEvent becomes a no-op }
      State := Popup.View.State;
      Exclude(State, vsModal);
      TTBViewAccess(Popup.View).SetState(State);
      Popup.Free;
    end;
  finally
    ModalHandler.Free;
  end;
  Result := ProcessDoneAction(DoneActionData, ReturnClickedItemOnly);
end;

function TTBXRootItem.PopupEx(const ControlRect: TRect;
  TrackRightButton: Boolean; Alignment: TTBPopupAlignment = tbpaLeft;
  ReturnClickedItemOnly: Boolean = False): TTBCustomItem;
begin
  Result := OpenPopupEx(False, TrackRightButton, ControlRect,
    Alignment, ReturnClickedItemOnly);
end;


//============================================================================//

{ TTBXPopupMenu }

function TTBXPopupMenu.GetRootItemClass: TTBRootItemClass;
begin
  Result := TTBXRootItem;
end;

function TTBXPopupMenu.PopupEx(const ControlRect: TRect;
  ReturnClickedItemOnly: Boolean = False): TTBCustomItem;
begin
  SetPopupPoint(Point(ControlRect.Left, ControlRect.Bottom));
  Result := TTBXRootItem(Items).PopupEx(ControlRect, TrackButton = tbRightButton,
    TTBPopupAlignment(Alignment), ReturnClickedItemOnly);
end;

procedure TTBXPopupMenu.TBMGetViewType(var Message: TMessage);
begin
  Message.Result := PVT_POPUPMENU;
end;


//============================================================================//

{ TTBXFloatingWindowParent }

procedure TTBXFloatingWindowParent.CancelNCHover;
begin
  if FCloseButtonHover then
  begin
    FCloseButtonHover := False;
    if HandleAllocated and IsWindowVisible(Handle) then
      DrawNCArea(False, 0, 0, [twrdCloseButton]);
  end;
end;

procedure TTBXFloatingWindowParent.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  CancelNCHover;
end;

procedure TTBXFloatingWindowParent.DrawNCArea(const DrawToDC: Boolean;
  const ADC: HDC; const Clip: HRGN; RedrawWhat: TTBToolWindowNCRedrawWhat);
const
  CDown: array [Boolean] of Integer = (0, CDBS_PRESSED);
  CHover: array [Boolean] of Integer = (0, CDBS_HOT);
  CBord: array [Boolean] of Integer = (0, WRP_BORDER);
  CCapt: array [Boolean] of Integer = (0, WRP_CAPTION);
  CBtn: array [Boolean] of Integer = (0, WRP_CLOSEBTN);
var
  DC: HDC;
  R: TRect;
  Canvas: TCanvas;
  WindowInfo: TTBXWindowInfo;
  DockWindow: TTBCustomDockableWindowAccess;
begin
  if not HandleAllocated then Exit;
  if not DrawToDC then DC := GetWindowDC(Handle)
  else DC := ADC;
  try
    if not DrawToDC then SelectNCUpdateRgn(Handle, DC, Clip);
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    with R do IntersectClipRect(DC, Left, Top, Right, Bottom);
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := DC;
      GetWindowRect(Handle, R);
      OffsetRect(R, -R.Left, -R.Top);
      DockWindow := TTBCustomDockableWindowAccess(DockableWindow);

      FillChar(WindowInfo, SizeOf(WindowInfo), 0);
      WindowInfo.ParentControl := Self;
      WindowInfo.ParentHandle := Handle;
      WindowInfo.WindowHandle := DockWindow.Handle;
      WindowInfo.ViewType := GetWinViewType(DockWindow);
      WindowInfo.ClientWidth := ClientWidth;
      WindowInfo.ClientHeight := ClientHeight;
      WindowInfo.ShowCaption := DockWindow.ShowCaption;
      WindowInfo.FloatingBorderSize := DockWindow.GetFloatingBorderSize;
      if DockWindow.CloseButton and DockWindow.ShowCaption then
      begin
        WindowInfo.CloseButtonState := CDBS_VISIBLE;
        if CloseButtonDown then WindowInfo.CloseButtonState := WindowInfo.CloseButtonState or CDBS_PRESSED
        else if CloseButtonHover then WindowInfo.CloseButtonState := WindowInfo.CloseButtonState or CDBS_HOT;
      end;
      WindowInfo.RedrawPart :=
        CBord[twrdBorder in RedrawWhat] or
        CCapt[twrdCaption in RedrawWhat] or
        CBtn[twrdCloseButton in RedrawWhat];
      WindowInfo.Caption := PChar(Caption);
      WindowInfo.EffectiveColor := GetEffectiveColor(DockWindow);
      WindowInfo.Active := not DockWindow.InactiveCaption;
      Canvas.Brush.Color := WindowInfo.EffectiveColor;
      CurrentTheme.PaintFloatingBorder(Canvas, R, WindowInfo);
    finally
      Canvas.Handle := 0;
      Canvas.Free;
    end;
  finally
    if not DrawToDC then ReleaseDC(Handle, DC);
  end;
end;

procedure TTBXFloatingWindowParent.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TTBXFloatingWindowParent.WMNCMouseLeave(var Message: TMessage);
begin
  if not MouseCapture then CancelNCHover;
  inherited;
end;

procedure TTBXFloatingWindowParent.WMNCMouseMove(var Message: TWMNCMouseMove);
var
  InArea: Boolean;
begin
  inherited;
  { Note: TME_NONCLIENT was introduced in Windows 98 and 2000 }
  CallTrackMouseEvent (Handle, TME_LEAVE or $10 {TME_NONCLIENT});
  InArea := Message.HitTest = 2001; {HT_TB2k_Close}
  if FCloseButtonHover <> InArea then
  begin
    FCloseButtonHover := InArea;
    if HandleAllocated and IsWindowVisible(Handle) then
      DrawNCArea(False, 0, 0, [twrdCloseButton]);
  end;
end;

procedure TTBXFloatingWindowParent.WMWindowPosChanging(var Message: TWMWindowPosChanging);
var
  R: TRect;
  MonInfo: TMonitorInfo;
begin
  if SnapDistance > 0 then with Message.WindowPos^ do
  begin
    if (cx = Width) and (cy = Height) then
    begin
      MonInfo.cbSize := SizeOf(MonInfo);
      GetMonitorInfo(Monitor.Handle, @MonInfo);
      R := MonInfo.rcWork;
      if Abs(x + Width - R.Right) < SnapDistance then x := R.Right - Width;
      if Abs(y + Height - R.Bottom) < SnapDistance then y := R.Bottom - Height;
      if Abs(x - R.Left) < SnapDistance then x := R.Left;
      if Abs(y - R.Top) < SnapDistance then y := R.Top;
    end;
  end;
  inherited;
end;


//============================================================================//

{ Additional system colors }

type
  TColorEntry = packed record
    ColorPtr: ^TColor;
    Name: string;
  end;

var
  ColorRegistry: array of TColorEntry;

procedure AddTBXColor(var AColor: TColor; const AName: string);
var
  L: Integer;
begin
  L := Length(ColorRegistry);
  SetLength(ColorRegistry, L + 1);
  with ColorRegistry[L] do
  begin
    ColorPtr := @AColor;
    Name := AName;
  end;
end;

function TBXColorToString(Color: TColor): string;
var
  I: Integer;
begin
  if not ColorToIdent(Color, Result) then
  begin
    for I := 0 to Length(ColorRegistry) - 1 do
      if ColorRegistry[I].ColorPtr^ = Color then
      begin
        Result := ColorRegistry[I].Name;
        Exit;
      end;
    FmtStr(Result, '%s%.8x', [HexDisplayPrefix, Color]);
  end;
end;

function TBXIdentToColor(const Ident: string; var Color: Longint): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(ColorRegistry) - 1 do
    if CompareText(ColorRegistry[I].Name, Ident) = 0 then
    begin
      Color := ColorRegistry[I].ColorPtr^;
      Result := True;
      Exit;
    end;
  Result := IdentToColor(Ident, Color);
end;

function TBXStringToColor(S: string): TColor;
begin
  if not TBXIdentToColor(S, Longint(Result)) then Result := StringToColor(S);
end;

procedure TBXGetColorValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  GetColorValues(Proc);
  for I := 0 to Length(ColorRegistry) - 1 do Proc(ColorRegistry[I].Name);
end;

procedure TBXSetTheme(const AThemeName: string);
begin
  TBXNexus.SetTheme(AThemeName);
end;

function TBXCurrentTheme: string;
begin
  Result := TBXNexus.GetTheme;
end;


//============================================================================//


{ TTBXNexus }

procedure TTBXNexus.AddNotifie(AObject: TObject);
begin
  if FNotifies.IndexOf(AObject) < 0 then FNotifies.Add(AObject);
  Exit; asm db 0,'TBX (C) 2001-2003 Alex Denisov',0 end;
end;

procedure TTBXNexus.Broadcast(Msg: Cardinal; WParam, LParam: Integer);
var
  M: TMessage;
  I: Integer;
begin
  if FNotifies.Count > 0 then
  begin
    M.Msg := Msg;
    M.WParam := WParam;
    M.LParam := LParam;
    M.Result := 0;
    for I := 0 to FNotifies.Count - 1 do TObject(FNotifies[I]).Dispatch(M);
  end;
end;

constructor TTBXNexus.Create(const DefaultTheme: string);
begin
  FNotifies := TList.Create;
  CurrentTheme := GetTBXTheme(DefaultTheme);
  AddTBXSysChangeNotification(Self);
end;

destructor TTBXNexus.Destroy;
begin
  RemoveTBXSysChangeNotification(Self);
  ReleaseTBXTheme(CurrentTheme);
  FNotifies.Free;
  inherited;
end;

function TTBXNexus.GetTheme: string;
begin
  Result := CurrentTheme.Name;
end;

procedure TTBXNexus.RemoveNotifie(AObject: TObject);
begin
  FNotifies.Remove(AObject);
end;

procedure TTBXNexus.SetTheme(const AThemeName: string);
begin
  if IsTBXThemeAvailable(AThemeName) then
  begin
    ReleaseTBXTheme(CurrentTheme);
    CurrentTheme := GetTBXTheme(AThemeName);
    Broadcast(TBM_THEMECHANGE, TSC_BEFOREVIEWCHANGE, 1);
    Broadcast(TBM_THEMECHANGE, TSC_VIEWCHANGE, 1);
    Broadcast(TBM_THEMECHANGE, TSC_AFTERVIEWCHANGE, 1);
  end;
end;

procedure TTBXNexus.TBXSysCommand(var Message: TMessage);
begin
  { Retranslate TBX_SYSCOMMAND to TBM_THEMECHANGE }
  if Message.Msg = TBX_SYSCOMMAND then
    Broadcast(TBM_THEMECHANGE, Message.WParam, 0);
end;

{ TTBXDock }

procedure TTBXDock.CMColorChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Self.ControlCount - 1 do
    if Controls[I] is TWinControl then
      InvalidateAll(TWinControl(Controls[I]));
end;

constructor TTBXDock.Create(AOwner: TComponent);
begin
  inherited;
  Color := clNone;
  AddThemeNotification(Self);
end;

destructor TTBXDock.Destroy;
begin
  RemoveThemeNotification(Self);
  inherited;
end;

procedure TTBXDock.DrawBackground(DC: HDC; const DrawRect: TRect);
const
  DOCK_POSITIONS: array [TTBDockPosition] of Integer = (DP_TOP, DP_BOTTOM, DP_LEFT, DP_RIGHT);
var
  Canvas: TCanvas;
begin
  if UseParentBackground then DrawParentBackground(Self, DC, ClientRect)
  else if ThemedBackground then
  begin
    Canvas := TCanvas.Create;
    Canvas.Handle := DC;
    CurrentTheme.PaintDock(Canvas, ClientRect, DrawRect, DOCK_POSITIONS[Position]);
    Canvas.Handle := 0;
    Canvas.Free;
  end
  else inherited;
end;

procedure TTBXDock.Resize;
var
  I, J: Integer;
  V: TTBItemViewer;
  R: TRect;
begin
  inherited Resize;
  if UsingBackground then
  begin
    for J := 0 to ToolbarCount - 1 do
    begin
      Invalidate;
      if Toolbars[J] is TTBXToolbar then with TTBXToolbar(Toolbars[J]) do
      begin
        for I := 0 to View.ViewerCount - 1 do
        begin
          V := View.Viewers[I];
          if V.Show and not IsRectEmpty(V.BoundsRect) and not (V.Item is TTBControlItem)
          then View.Invalidate(V);
        end;
        Update;
        if HandleAllocated then
          RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
        for I := 0 to View.ViewerCount - 1 do
        begin
          V := View.Viewers[I];
          if V.Show and not IsRectEmpty(V.BoundsRect) and not (V.Item is TTBControlItem)
          then
          begin
            R := V.BoundsRect;
            ValidateRect(Handle, @R);
          end;
        end;
      end
    end;
  end;
end;

procedure TTBXDock.SetUseParentBackground(Value: Boolean);
begin
  if Value <> FUseParentBackground then
  begin
    FUseParentBackground := Value;
    if HandleAllocated then
      RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or
        RDW_ERASE or RDW_ALLCHILDREN);
  end;
end;

procedure TTBXDock.TBMGetEffectiveColor(var Message: TMessage);
begin
  if Color <> clNone then Message.WParam := Color
  else if Parent <> nil then Message.WParam := GetEffectiveColor(Parent)
  else Message.WParam := WPARAM(clBtnFace);
  Message.Result := 1;
end;

procedure TTBXDock.TBMThemeChange(var Message: TMessage);
begin
  case Message.WParam of
    TSC_AFTERVIEWCHANGE:
      begin
        Invalidate;
      end;
  end;
end;

function TTBXDock.ThemedBackground: Boolean;
begin
  Result := (Color = clNone) and CurrentTheme.PaintDockBackground;
end;

function TTBXDock.UsingBackground: Boolean;
begin
  Result := UseParentBackground or (ThemedBackground and not FMoving) or
    inherited UsingBackground;
end;

procedure TTBXDock.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  R: TRect;
  C: TColor;
begin
  R := Self.ClientRect;
  if UsingBackground then
  begin
    DrawBackground(Message.DC, R);
    Message.Result := 1;
  end
  else
  begin
    C := Color;
    if C = clNone then C := GetEffectiveColor(Parent);
    FillRectEx(Message.DC, R, C);
    Message.Result := 1;
  end;
end;

procedure TTBXDock.WMMove(var Message: TWMMove);
begin
  FMoving := True;
  inherited;
  FMoving := False;
end;

procedure TTBXDock.WMSize(var Message: TWMSize);
begin
  FResizing := True;
  inherited;
  FResizing := False;
end;

{ TTBXMenuAnimation }

constructor TTBXMenuAnimation.Create(AAnimationMode: TAnimationMode = amSysDefault);
begin
  AnimationMode := AAnimationMode;
end;

function TTBXMenuAnimation.GetAvailableModes: TAnimationModes;
begin
  Result := [amNone, amSysDefault, amRandom, amUnfold, amSlide, amFade];
end;

function TTBXMenuAnimation.GetMenuAnimation: TMenuAnimation;

  function GetSysDefault: TMenuAnimation;
  const
    SPI_GETMENUFADE = $1012;
    SysDefAni: array[Boolean] of TMenuAnimation = (maSlide, maFade);
  begin
    if SysParamEnabled(SPI_GETMENUANIMATION)
      then Result := SysDefAni[SysParamEnabled(SPI_GETMENUFADE)]
      else Result := maNone;
  end;

  function GetRandom: TMenuAnimation;
  var Max: Integer;
  begin
    Max := Ord(High(TMenuAnimation));
    if not (amFade in AvailableModes) then
      Dec(Max);
    Result := Succ(TMenuAnimation(Random(Max)));
  end;

begin
  case AnimationMode of
    amSysDefault: Result := GetSysDefault;
    amRandom: Result := GetRandom;
    amUnfold: Result := maUnfold;
    amSlide: Result := maSlide;
    amFade: Result := maFade;
  else
    Result := maNone;
  end;
end;

procedure TTBXMenuAnimation.SetAnimationMode(Value: TAnimationMode);
var AvailModes: TAnimationModes;
begin
  AvailModes := AvailableModes;
  while not (Value in AvailModes) do
    Value := Pred(Value);
  FAnimationMode := Value;
end;

function TTBXMenuAnimation.SysParamEnabled(Param: Cardinal): Boolean;
var B: BOOL;
begin
  Result := SystemParametersInfo(Param, 0, @B, 0) and B;
end;

{ Work around delayed menu showing in Windows 2000+ }
var
  FixPlaySoundThreadHandle: Cardinal;

function FixPlaySoundThreadFunc(Param: Pointer): Integer; stdcall;
begin
  Sleep(250);
  PlaySound(nil, 0, 0);
  CloseHandle(FixPlaySoundThreadHandle); { Harakiri :~| }
  Result := $4E494150; { :) }
end;

procedure FixPlaySoundDelay;
var ThreadId: TThreadID;
begin
  if (FixPlaySoundThreadHandle = 0) then
      FixPlaySoundThreadHandle := CreateThread(nil, $1000,
        @FixPlaySoundThreadFunc, nil, 0, ThreadId);
end;

function CreateTBXPopupMenu(Owner: TComponent): TTBXPopupMenu;
begin
  Result := TTBXPopupMenu.Create(Owner);
end;

initialization
  FixPlaySoundDelay;
  RegisterTBXTheme('OfficeXP', TTBXOfficeXPTheme);
  RegisterTBXTheme('DarkOfficeXP', TTBXDarkOfficeXPTheme);
  TBXNexus := TTBXNexus.Create('OfficeXP');
  TBXMenuAnimation := TTBXMenuAnimation.Create;

finalization
  TBXNexus.Free;
  FreeAndNil(TBXMenuAnimation);
  ColorRegistry := nil;

end.
