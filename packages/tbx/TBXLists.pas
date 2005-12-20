unit TBXLists;

// TBX Package
// Copyright 2001-2004 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// Id: TBXLists.pas 7 2004-02-21 06:07:53Z

interface

{$I TB2Ver.inc}
{$I TBX.inc}

uses
  Windows, Messages, Classes, SysUtils, Controls, Forms, Graphics, TB2Item, TBX,
  TBXThemes, TBXUxThemes, ImgList;

type
  { TTBXScrollBar }

  TSBIncrement = 1..1000;
  TSBZone = (sbzEmpty, sbzPrev, sbzPagePrev, sbzHandle, sbzPageNext, sbzNext);
  TSBAutoScrollEvent = procedure(Sender: TObject; var Direction, Interval: Integer) of object;

  TTBXScrollBar = class
  private
    FBounds: TRect;
    FLeft: Integer;
    FHandle: HWND;
    FHeight: Integer;
    FIncrement: TSBIncrement;
    FKind: TScrollBarKind;
    FPosition: Integer;
    FRange: Integer;
    FRight: Integer;
    FTop: Integer;
    FWidth: Integer;
    FWindow: Integer;
    FOnChange: TNotifyEvent;
    FOnAutoScroll: TSBAutoScrollEvent;
    FOnRedrawRequest: TNotifyEvent;
    procedure SetBounds(const Value: TRect);
    procedure SetKind(Value: TScrollBarKind);
    procedure SetPosition(Value: Integer);
    procedure SetRange(Value: Integer);
    function  GetHandle: HWND;
  protected
    AutoScrollDirection: Integer;
    AutoScrolling: Boolean;
    AutoScrollInterval: Integer;
    Zones: array [TSBZone] of TRect;
    MouseDownZone: TSBZone;
    MouseDownPoint: TPoint;
    MouseDownPosition: Integer;
    LastMousePoint: TPoint;
    PrevCapture: HWND;
    UserChange: Boolean;
    procedure AdjustPosition(var NewPosition: Integer);
    procedure CreateWnd;
    procedure DestroyWnd;
    function  GetZone(X, Y: Integer): TSBZone;
    function  GetEffectiveWindow: Integer;
    function  GetEnabled: Boolean; virtual;
    procedure HandleZoneClick(AZone: TSBZone);
    procedure MouseDown(Button: TMouseButton; X, Y: Integer);  virtual;
    procedure MouseMove(X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; X, Y: Integer); virtual;
    procedure PaintButton(Canvas: TCanvas; Rect: TRect; Direction: Integer; Pushed, Enabled: Boolean);
    procedure PaintHandle(Canvas: TCanvas; Rect: TRect; Pushed, Enabled: Boolean);
    procedure PaintTrack(Canvas: TCanvas; Rect: TRect; IsNextZone, Pushed, Enabled: Boolean);
    procedure PaintTo(Canvas: TCanvas);
    procedure SBWndProc(var Message: TMessage);
    procedure StartAutoScroll(Direction, Interval: Integer);
    procedure StopAutoScroll;
    procedure StartTimer(ID: Integer; Elapse: Integer);
    procedure StopTimer(ID: Integer);
    procedure TimerElapsed(ID: Integer; var NewElapse: Integer); virtual;
    procedure UpdateZones;
    property Handle: HWND read GetHandle;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Redraw; virtual;
    procedure UpdatePosition(NewPosition: Integer);
    property Kind: TScrollBarKind read FKind write SetKind;
    property Bounds: TRect read FBounds write SetBounds;
    property Left: Integer read FLeft;
    property Height: Integer read FHeight;
    property Increment: TSBIncrement read FIncrement write FIncrement;
    property Position: Integer read FPosition write SetPosition;
    property Range: Integer read FRange write SetRange;
    property Right: Integer read FRight;
    property Top: Integer read FTop;
    property Width: Integer read FWidth;
    property Window: Integer read FWindow write FWindow;
    property OnAutoScroll: TSBAutoScrollEvent read FOnAutoScroll write FOnAutoScroll;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnRedrawRequest: TNotifyEvent read FOnRedrawRequest write FOnRedrawRequest;
  end;

  { TTBXCustomList }
  TTBXCustomList = class;

  TTBXLMeasureHeight = procedure(Sender: TTBXCustomList; ACanvas: TCanvas; var AHeight: Integer) of object;
  TTBXLMeasureWidth = procedure(Sender: TTBXCustomList; ACanvas: TCanvas; AIndex: Integer; var AWidth: Integer) of object;
  TTBXLPaintEvent = procedure(Sender: TTBXCustomList; ACanvas: TCanvas; ARect: TRect; AIndex, AHoverIndex: Integer; var DrawDefault: Boolean) of object;
  TTBXLAdjustImageIndex = procedure(Sender: TTBXCustomList; AItemIndex: Integer; var ImageIndex: Integer) of object;

  TTBXCustomListViewer = class;

  TTBXCustomList = class(TTBXCustomItem)
  private
    FViewers: TList;
    FItemIndex: Integer;
    FMinWidth: Integer;
    FMaxWidth: Integer;
    FMaxVisibleItems: Integer;
    FShowImages: Boolean;
    FOnChange: TNotifyEvent;
    FOnClearItem: TTBXLPaintEvent;
    FOnDrawItem: TTBXLPaintEvent;
    FOnAdjustImageIndex: TTBXLAdjustImageIndex;
    FOnMeasureHeight: TTBXLMeasureHeight;
    FOnMeasureWidth: TTBXLMeasureWidth;
    procedure SetItemIndex(Value: Integer);
  protected
    function  DoClearItem(ACanvas: TCanvas; ARect: TRect; AIndex, AHoverIndex: Integer): Boolean; virtual;
    function  DoDrawItem(ACanvas: TCanvas; ARect: TRect; AIndex, AHoverIndex: Integer): Boolean; virtual;
    procedure DoMeasureHeight(ACanvas: TCanvas; var AHeight: Integer); virtual;
    procedure DoMeasureWidth(ACanvas: TCanvas; AIndex: Integer; var AWidth: Integer); virtual;
    procedure DrawItem(ACanvas: TCanvas; AViewer: TTBXCustomListViewer; const ARect: TRect; AIndex, AHoverIndex: Integer); virtual;
    function  GetImageIndex(ItemIndex: Integer): Integer; virtual;
    function  GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    function  GetItemText(Index: Integer): string; virtual; abstract;
    function  GetCount: Integer; virtual; abstract;
    procedure HandleChange; virtual;
    procedure HandleHover(AIndex: Integer); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure MakeVisible(AIndex: Integer);
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property MaxVisibleItems: Integer read FMaxVisibleItems write FMaxVisibleItems default 8;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth default 0;
    property MinWidth: Integer read FMinWidth write FMinWidth default 32;
    property ShowImages: Boolean read FShowImages write FShowImages default False;
    property OnAdjustImageIndex: TTBXLAdjustImageIndex read FOnAdjustImageIndex write FOnAdjustImageIndex;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClearItem: TTBXLPaintEvent read FOnClearItem write FOnClearItem;
    property OnDrawItem: TTBXLPaintEvent read FOnDrawItem write FOnDrawItem;
    property OnMeasureHeight: TTBXLMeasureHeight read FOnMeasureHeight write FOnMeasureHeight;
    property OnMeasureWidth: TTBXLMeasureWidth read FOnMeasureWidth write FOnMeasureWidth;
  end;

  TTBXCustomListViewer = class(TTBXItemViewer)
  private
    FItemCount: Integer;
    FItemHeight: Integer;
    FHoverIndex: Integer;
    FHeight: Integer;
    FLastClientRect: TRect;
    FWheelAccumulator: Integer;
    FWidth: Integer;
    FOffset: Integer;
    FScrollBarWidth: Integer;
    FScrollBar: TTBXScrollBar;
    FVisibleItems: Integer;
    procedure ListChangeHandler(NewIndex: Integer);
    procedure SBAutoScrollHandler(Sender: TObject; var Direction, Interval: Integer);
    procedure SBChangeHandler(Sender: TObject);
    procedure SBRedrawHandler(Sender: TObject);
  protected
    MouseIsDown: Boolean;
    MouseInScrollBar: Boolean;
    IgnoreMouseUp: Boolean;
    IsChanging: Boolean;
    procedure AdjustAutoScrollHover(var AIndex: Integer; Direction: Integer); virtual;
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    procedure DrawItems(const Canvas: TCanvas; const ClientAreaRect: TRect);
    function  GetItemIndexAt(X, Y: Integer): Integer;
    function  GetItemRect(Index: Integer): TRect;
    function  GetItemHeight(ACanvas: TCanvas): Integer; virtual;
    function  GetItemWidth(ACanvas: TCanvas; Index: Integer): Integer; virtual;
    procedure HandleAutoScroll(var Direction, Interval: Integer); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MakeVisible(Index: Integer);
    procedure MouseDown(Shift: TShiftState; X, Y: Integer; var MouseDownOnMenu: Boolean); override;
    procedure MouseMove(X, Y: Integer); override;
    procedure MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean); override;
    procedure MouseWheel(WheelDelta: Integer; X, Y: Integer); override;
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect; IsHoverItem, IsPushed, UseDisabledShadow: Boolean); override;
    procedure UpdateItems;
    property HoverIndex: Integer read FHoverIndex write FHoverIndex;
    property Offset: Integer read FOffset; {vb+}
    property VisibleItems: Integer read FVisibleItems; {vb+}
  public
    constructor Create(AView: TTBView; AItem: TTBCustomItem; AGroupLevel: Integer); override;
    destructor Destroy; override;
  end;

  { TTBXStringList }

  TTBXStringList = class(TTBXCustomList)
  private
    FStrings: TStrings;
    procedure SetStrings(Value: TStrings);
  protected
    function GetItemText(Index: Integer): string; override;
    function GetCount: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ItemIndex;
    property MaxVisibleItems;
    property MaxWidth;
    property MinWidth;
    property Strings: TStrings read FStrings write SetStrings;
    property OnAdjustImageIndex;
    property OnChange;
    property OnClearItem;
    property OnClick;
    property OnDrawItem;
    property OnMeasureHeight;
    property OnMeasureWidth;
  end;

  TTBXStringListClass = class of TTBXStringList;

  {$IFNDEF MPEXCLUDE}

  { TTBXUndoList }

  TTBXUndoList = class(TTBXStringList)
  protected
    procedure DrawItem(ACanvas: TCanvas; AViewer: TTBXCustomListViewer; const ARect: TRect; AIndex, AHoverIndex: Integer); override;
    function  GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    procedure HandleHover(AIndex: Integer); override;
  end;

  TTBXUndoListViewer = class(TTBXCustomListViewer)
  protected
    procedure AdjustAutoScrollHover(var AIndex: Integer; Direction: Integer); override;
    procedure HandleAutoScroll(var Direction, Interval: Integer); override;
  end;
  {$ENDIF}

implementation

type TTBViewAccess = class(TTBView);

const
  SCROLL_TIMER = 1;
  AUTO_SCROLL_TIMER = 2;
  MIN_SB_HANDLE_SIZE = 8;

  CImageSpacing = 4;

//----------------------------------------------------------------------------//

{ TTBXScrollBar }

procedure TTBXScrollBar.AdjustPosition(var NewPosition: Integer);
var
  W: Integer;
begin
  W  := GetEffectiveWindow;
  if NewPosition + W > Range then NewPosition := Range - W;
  if NewPosition < 0 then NewPosition := 0;
end;

constructor TTBXScrollBar.Create;
begin
  FIncrement := 1;
end;

procedure TTBXScrollBar.CreateWnd;
begin
  if FHandle = 0 then FHandle := {$IFDEF JR_D6}Classes.{$ENDIF}AllocateHWnd(SBWndProc);
end;

destructor TTBXScrollBar.Destroy;
begin
  DestroyWnd;
  inherited;
end;

procedure TTBXScrollBar.DestroyWnd;
begin
  if FHandle <> 0 then
  begin
    {$IFDEF JR_D6}Classes.{$ENDIF}DeallocateHWnd(FHandle);
    FHandle := 0;
  end;
end;

function TTBXScrollBar.GetEffectiveWindow: Integer;
begin
  if Window <= 0 then
  begin
    if Kind = sbVertical then Result := Height
    else Result := Width;
  end
  else Result := Window;
end;

function TTBXScrollBar.GetEnabled: Boolean;
begin
  Result := Range > GetEffectiveWindow;
end;

function TTBXScrollBar.GetHandle: HWND;
begin
  if FHandle = 0 then CreateWnd;
  Result := FHandle;
end;

function TTBXScrollBar.GetZone(X, Y: Integer): TSBZone;
var
  I: Integer;
  Pt: TPoint;
begin
  Pt.X := X;
  Pt.Y := Y;
  for I := Ord(sbzPrev) to Ord(sbzNext) do
  begin
    Result := TSBZone(I);
    if PtInRect(Zones[Result], Pt) then Exit;
  end;
  Result := sbzEmpty;
end;

procedure TTBXScrollBar.HandleZoneClick(AZone: TSBZone);
begin
  UserChange := True;
  case AZone of
    sbzPrev: Position := Position - Increment;
    sbzPagePrev: Position := Position - GetEffectiveWindow;
    sbzPageNext: Position := Position + GetEffectiveWindow;
    sbzNext: Position := Position + Increment;
  end;
  UserChange := False;
end;

procedure TTBXScrollBar.MouseDown(Button: TMouseButton; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    MouseDownZone := GetZone(X, Y);
    MouseDownPoint := Point(X, Y);
    MouseDownPosition := Position;
    LastMousePoint := MouseDownPoint;
    if MouseDownZone in [sbzPrev, sbzPagePrev, sbzPageNext, sbzNext] then
    begin
      HandleZoneClick(MouseDownZone);
      StartTimer(SCROLL_TIMER, 500);
    end;
    Redraw;
  end;
end;

procedure TTBXScrollBar.MouseMove(X, Y: Integer);
var
  Delta: Integer;
  ClientSize, HandleSize: Integer;
begin
  LastMousePoint := Point(X, Y);
  if MouseDownZone = sbzHandle then
  begin
    if Kind = sbVertical then
    begin
      Delta := Y - MouseDownPoint.Y;
      ClientSize := Zones[sbzPageNext].Bottom - Zones[sbzPagePrev].Top;
    end
    else
    begin
      Delta := X - MouseDownPoint.X;
      ClientSize := Zones[sbzPageNext].Right - Zones[sbzPagePrev].Left;
    end;
    HandleSize := Round(ClientSize * Window / Range);
    if HandleSize < MIN_SB_HANDLE_SIZE then
      Delta := Round(Delta * (Range - Window) / (ClientSize - MIN_SB_HANDLE_SIZE))
    else
      Delta := Round(Delta * Range / ClientSize);

    if MouseDownPosition + Delta <> Position then
    begin
      UserChange := True;
      Position := MouseDownPosition + Delta;
      UserChange := False;
    end;
  end;
end;

procedure TTBXScrollBar.MouseUp(Button: TMouseButton; X, Y: Integer);
begin
  StopTimer(SCROLL_TIMER);
  if Button = mbLeft then
  begin
    MouseDownZone := sbzEmpty;
    Redraw;
  end;
end;

procedure TTBXScrollBar.PaintButton(Canvas: TCanvas; Rect: TRect;
  Direction: Integer; Pushed, Enabled: Boolean);
const
  DirectionFlags: array [0..3] of Cardinal = (DFCS_SCROLLLEFT, DFCS_SCROLLUP,
    DFCS_SCROLLRIGHT, DFCS_SCROLLDOWN);
  EnabledFlags: array [Boolean] of Cardinal = (DFCS_INACTIVE, 0);
  PushedFlags: array [Boolean] of Cardinal = (0, DFCS_PUSHED or DFCS_FLAT);

  DirectionXPFlags: array [0..3] of Cardinal = (ABS_LEFTNORMAL, ABS_UPNORMAL,
    ABS_RIGHTNORMAL, ABS_DOWNNORMAL);
var
  StateFlags: Cardinal;
begin
  if USE_THEMES then
  begin
    StateFlags := DirectionXPFlags[Direction];
    if not Enabled then Inc(StateFlags, 3)
    else if Pushed then Inc(StateFlags, 2);
    DrawThemeBackground(SCROLLBAR_THEME, Canvas.Handle, SBP_ARROWBTN, StateFlags, Rect, nil);
  end
  else
  begin
    DrawFrameControl(Canvas.Handle, Rect, DFC_SCROLL,
      DirectionFlags[Direction] or EnabledFlags[Enabled] or PushedFlags[Pushed]);
  end;
end;

procedure TTBXScrollBar.PaintHandle(Canvas: TCanvas; Rect: TRect; Pushed, Enabled: Boolean);
const
  PartXPFlags: array [TScrollBarKind] of Cardinal = (SBP_THUMBBTNHORZ, SBP_THUMBBTNVERT);
var
  StateFlags: Cardinal;
begin
  if USE_THEMES then
  begin
    StateFlags := SCRBS_NORMAL;
    if not Enabled then Inc(StateFlags, 3)
    else if Pushed then Inc(StateFlags, 2);
    DrawThemeBackground(SCROLLBAR_THEME, Canvas.Handle, PartXPFlags[Kind], StateFlags, Rect, nil);
  end
  else
  begin
    DrawEdge(Canvas.Handle, Rect, EDGE_RAISED, BF_RECT or BF_ADJUST);
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(Rect);
  end;
end;

procedure TTBXScrollBar.PaintTo(Canvas: TCanvas);
var
  R: TRect;
  E, IsVert: Boolean;
  I: Integer;
  Dummy: TPoint;
begin
  UpdateZones;
  IsVert := Kind = sbVertical;
  E := GetEnabled;

  OffsetWindowOrgEx(Canvas.Handle, -Bounds.Left, -Bounds.Top, Dummy);
  try

    if IsVert then I := 1 else I := 0;
    PaintButton(Canvas, Zones[sbzPrev], I, MouseDownZone = sbzPrev, E);
    PaintButton(Canvas, Zones[sbzNext], I + 2, MouseDownZone = sbzNext, E);

    if not IsRectEmpty(Zones[sbzEmpty]) then
    begin
      Canvas.Brush.Color := clScrollBar;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(Zones[sbzEmpty]);
    end;

    if not IsRectEmpty(Zones[sbzPagePrev]) or not IsRectEmpty(Zones[sbzPageNext]) then
    begin
      R := Zones[sbzPagePrev];
      PaintTrack(Canvas, R, False, MouseDownZone = sbzPagePrev, E);
      R := Zones[sbzPageNext];
      PaintTrack(Canvas, R, True, MouseDownZone = sbzPageNext, E);
    end;

    if not IsRectEmpty(Zones[sbzHandle]) then
      PaintHandle(Canvas, Zones[sbzHandle], MouseDownZone = sbzHandle, E);

  finally
    OffsetWindowOrgEx(Canvas.Handle, Bounds.Left, Bounds.Top, Dummy);
  end;
end;

procedure TTBXScrollBar.PaintTrack(Canvas: TCanvas; Rect: TRect;
  IsNextZone, Pushed, Enabled: Boolean);
const
  PartXPFlags: array [Boolean, TScrollBarKind] of Cardinal =
    ((SBP_LOWERTRACKHORZ, SBP_LOWERTRACKVERT), (SBP_UPPERTRACKHORZ, SBP_UPPERTRACKVERT));
var
  StateFlags: Cardinal;
begin
  if USE_THEMES then
  begin
    StateFlags := SCRBS_NORMAL;
    if not Enabled then Inc(StateFlags, 3)
    else if Pushed then Inc(StateFlags, 2);
    DrawThemeBackground(SCROLLBAR_THEME, Canvas.Handle, PartXPFlags[IsNextZone, Kind],
      StateFlags, Rect, nil);
  end
  else
  begin
    if Pushed then Canvas.Brush.Color := cl3DDkShadow
    else Canvas.Brush.Bitmap := AllocPatternBitmap(clBtnHighlight, clScrollBar);
    Canvas.FillRect(Rect);
  end;
end;

procedure TTBXScrollBar.Redraw;
begin
  if Assigned(FOnRedrawRequest) then FOnRedrawRequest(Self);
end;

procedure TTBXScrollBar.SBWndProc(var Message: TMessage);
var
  I: Integer;

  procedure DefaultHandler;
  begin
    with Message do
      Result := DefWindowProc(FHandle, Msg, wParam, lParam);
  end;

begin
  case Message.Msg of

    WM_TIMER: with TWMTimer(Message) do
      begin
        I := 0;
        TimerElapsed(TimerID, I);
        if I > 0 then StartTimer(TimerID, I)
        else StopTimer(TimerID);
        Result := 0;
      end;

  else
    DefaultHandler;
  end;
end;

procedure TTBXScrollBar.SetBounds(const Value: TRect);
begin
  FBounds := Value;
  with Value do
  begin
    FLeft := Left;
    FTop := Top;
    FWidth := Right - Left;
    FHeight := Bottom - Top;
  end;
  UpdateZones;
end;

procedure TTBXScrollBar.SetKind(Value: TScrollBarKind);
begin
  FKind := Value;
  UpdateZones;
end;

procedure TTBXScrollBar.SetPosition(Value: Integer);
begin
  AdjustPosition(Value);
  if Value <> FPosition then
  begin
    FPosition := Value;
    if UserChange then
    begin
      Redraw;
      if Assigned(FOnChange) then FOnChange(Self);
    end;
  end;
end;

procedure TTBXScrollBar.SetRange(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value <> FRange then
  begin
    FRange := Value;
    Redraw;
    SetPosition(Position);
  end;
end;

procedure TTBXScrollBar.StartAutoScroll(Direction, Interval: Integer);
begin
  if Direction <> 0 then
  begin
    AutoScrollDirection := Direction;
    AutoScrollInterval := Interval;
    if not AutoScrolling then
    begin
      StartTimer(AUTO_SCROLL_TIMER, Interval);
      AutoScrolling := True;
    end;
  end;
end;

procedure TTBXScrollBar.StartTimer(ID, Elapse: Integer);
begin
  SetTimer(Handle, ID, Elapse, nil);
end;

procedure TTBXScrollBar.StopAutoScroll;
begin
  if AutoScrolling then
  begin
    AutoScrolling := False;
    StopTimer(AUTO_SCROLL_TIMER);
  end;
end;

procedure TTBXScrollBar.StopTimer(ID: Integer);
begin
  KillTimer(Handle, ID);
end;

procedure TTBXScrollBar.TimerElapsed(ID: Integer; var NewElapse: Integer);
begin
  case ID of
    SCROLL_TIMER:
      if MouseDownZone <> sbzEmpty then
        if not (MouseDownZone in [sbzPagePrev, sbzPageNext]) or
          (GetZone(LastMousePoint.X, LastMousePoint.Y) = MouseDownZone) then
        begin
          HandleZoneClick(MouseDownZone);
          NewElapse := 100;
        end;
    AUTO_SCROLL_TIMER: if AutoScrolling then
      begin
        NewElapse := AutoScrollInterval;
        UpdatePosition(Position + AutoScrollDirection);
        if (Position = 0) or (Position + Window = Range) then NewElapse := 0;
        if Assigned(FOnAutoScroll) then
          FOnAutoScroll(Self, AutoScrollDirection, AutoScrollInterval);
        AutoScrolling := NewElapse > 0;
      end;
  end;
end;

procedure TTBXScrollBar.UpdatePosition(NewPosition: Integer);
begin
  UserChange := True;
  if NewPosition < 0 then NewPosition := 0;
  if NewPosition > Range - Window then NewPosition := Range - Window;
  Position := NewPosition;
  UserChange := False;
end;

procedure TTBXScrollBar.UpdateZones;
var
  SzL, SzT: Integer;
  ButtonSize: Integer;
  Lo, Hi: Integer;
  HandleSize, HandlePos: Integer;
  Window: Integer;
  IsVert: Boolean;

  procedure SetZone(var R: TRect; Lo, Hi: Integer);
  begin
    if IsVert then
    begin
      R.Left := 0;
      R.Right := Width;
      R.Top := Lo;
      R.Bottom := Hi;
    end
    else
    begin
      R.Left := Lo;
      R.Right := Hi;
      R.Top := 0;
      R.Bottom := Height;
    end;
  end;

begin
  IsVert := Kind = sbVertical;
  Window := GetEffectiveWindow;

  if IsVert then
  begin
    SzL := Height;
    SzT := Width;
  end
  else
  begin
    SzL := Width;
    SzT := Height;
  end;


  { Buttons }
  ButtonSize := SzT;
  if ButtonSize * 2 >= SzL - 2 then ButtonSize := (SzL - 2) div 2;
  SetZone(Zones[sbzPrev], 0, ButtonSize);
  SetZone(Zones[sbzNext], SzL - ButtonSize, SzL);

  { Handle }
  Lo := ButtonSize;
  Hi := SzL - ButtonSize;
  if GetEnabled and (Hi - Lo > MIN_SB_HANDLE_SIZE + 4) then
  begin
    HandleSize := Round((Hi - Lo) * Window / Range);
    if HandleSize >= MIN_SB_HANDLE_SIZE then
      HandlePos := Round((Hi - Lo) * Position / Range)
    else
    begin
      HandleSize := MIN_SB_HANDLE_SIZE;
      HandlePos := Round((Hi - Lo - MIN_SB_HANDLE_SIZE) * Position / (Range - Window));
    end;
    Inc(HandlePos, Lo);
    SetZone(Zones[sbzHandle], HandlePos, HandlePos + HandleSize);
    SetZone(Zones[sbzPagePrev], Lo, HandlePos);
    SetZone(Zones[sbzPageNext], HandlePos + HandleSize, Hi);
    Zones[sbzEmpty].Right := -1;
  end
  else
  begin
    { Invalidate invisible zones }
    Zones[sbzPagePrev].Right := -1;
    Zones[sbzHandle].Right := -1;
    Zones[sbzPageNext].Right := -1;
    SetZone(Zones[sbzEmpty], Lo, Hi);
  end;
end;

//----------------------------------------------------------------------------//

{ TTBXCustomList }

constructor TTBXCustomList.Create(AOwner: TComponent);
begin
  inherited;
  FMinWidth := 32;
  FMaxWidth := 0;
  FMaxVisibleItems := 8;
  FItemIndex := -1;
end;

function TTBXCustomList.DoClearItem(ACanvas: TCanvas; ARect: TRect; AIndex, AHoverIndex: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnClearItem) then FOnClearItem(Self, ACanvas, ARect, AIndex, AHoverIndex, Result);
end;

function TTBXCustomList.DoDrawItem(ACanvas: TCanvas; ARect: TRect; AIndex, AHoverIndex: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnDrawItem) then FOnDrawItem(Self, ACanvas, ARect, AIndex, AHoverIndex, Result);
end;

procedure TTBXCustomList.DoMeasureHeight(ACanvas: TCanvas; var AHeight: Integer);
begin
  if Assigned(FOnMeasureHeight) then FOnMeasureHeight(Self, ACanvas, AHeight);
end;

procedure TTBXCustomList.DoMeasureWidth(ACanvas: TCanvas; AIndex: Integer; var AWidth: Integer);
begin
  if Assigned(FOnMeasureWidth) then FOnMeasureWidth(Self, ACanvas, AIndex, AWidth);
end;

procedure TTBXCustomList.DrawItem(ACanvas: TCanvas; AViewer: TTBXCustomListViewer;
  const ARect: TRect; AIndex, AHoverIndex: Integer);
const
  FillColors: array [Boolean] of TColor = (clWindow, clHighlight);
  TextColors: array [Boolean] of TColor = (clWindowText, clHighlightText);
var
  S: string;
  R, R2: TRect;
  ImgList: TCustomImageList;
begin
  ACanvas.Brush.Color := FillColors[AIndex = AHoverIndex];
  if DoClearItem(ACanvas, ARect, AIndex, AHoverIndex) then ACanvas.FillRect(ARect);

  ACanvas.Font.Color := TextColors[AIndex = AHoverIndex];
  if DoDrawItem(ACanvas, ARect, AIndex, AHoverIndex) then
  begin
    R := ARect;
    InflateRect(R, -4, 1);
    ImgList := AViewer.GetImageList;
    if ShowImages and (ImgList <> nil) then
    begin
      R2.Left := R.Left;
      R2.Top := (R.Top + R.Bottom - ImgList.Height) div 2;
      R2.Right := R2.Left + ImgList.Width;
      R2.Bottom := R2.Top + ImgList.Height;
      if Enabled then ImgList.Draw(ACanvas, R2.Left, R2.Top, GetImageIndex(AIndex))
      else DrawTBXImage(ACanvas, R2, ImgList, GetImageIndex(AIndex), ISF_DISABLED);
      Inc(R.Left, ImgList.Width + CImageSpacing);
    end;

    S := GetItemText(AIndex);
    if Length(S) > 0 then
    begin
      ACanvas.Brush.Style := bsClear;
      DrawText(ACanvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
      ACanvas.Brush.Style := bsSolid;
    end;
  end;
end;

function TTBXCustomList.GetImageIndex(ItemIndex: Integer): Integer;
begin
  Result := ItemIndex;
  if Assigned(FOnAdjustImageIndex) then FOnAdjustImageIndex(Self, ItemIndex, Result);
end;

function TTBXCustomList.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TTBXCustomListViewer;
end;

procedure TTBXCustomList.HandleChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TTBXCustomList.HandleHover(AIndex: Integer);
begin
end;

procedure TTBXCustomList.MakeVisible(AIndex: Integer);
var
  I: Integer;
begin
  if FViewers <> nil then
    for I := 0 to FViewers.Count - 1 do
      TTBXCustomListViewer(FViewers[I]).MakeVisible(AIndex);
end;

procedure TTBXCustomList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then Images := nil;
end;

procedure TTBXCustomList.SetItemIndex(Value: Integer);
var
  I: Integer;
begin
  if Value < 0 then Value := -1;
  FItemIndex := Value;

  { Update viewers }
  if FViewers <> nil then
    for I := 0 to FViewers.Count - 1 do
      TTBXCustomListViewer(FViewers[I]).ListChangeHandler(Value);

  if Assigned(FOnChange) then FOnChange(Self);
end;

//----------------------------------------------------------------------------//

{ TTBXCustomListViewer }

procedure TTBXCustomListViewer.AdjustAutoScrollHover(var AIndex: Integer; Direction: Integer);
begin
  AIndex := -1; // turn off hover when autoscrolling
end;

procedure TTBXCustomListViewer.CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer);
var
  Item: TTBXCustomList;
  I, W: Integer;
begin
  Item := TTBXCustomList(Self.Item);
  Canvas.Font := TTBViewAccess(View).GetFont;
  FItemCount := Item.GetCount;
  FItemHeight := GetItemHeight(Canvas);

  FVisibleItems := FItemCount;
  if FVisibleItems > Item.MaxVisibleItems then FVisibleItems := Item.MaxVisibleItems
  else if FVisibleItems <= 0 then FVisibleItems := 1;

  AHeight := FVisibleItems * FItemHeight;

  AWidth := 0;
  for I := 0 to FItemCount - 1 do
  begin
    W := GetItemWidth(Canvas, I);
    if W > AWidth then AWidth := W;
  end;

  if FItemCount > FVisibleItems then FScrollBarWidth := GetSystemMetrics(SM_CXVSCROLL)
  else FScrollBarWidth := 0;
  Inc(AWidth, FScrollBarWidth);

  if AWidth < Item.MinWidth then AWidth := Item.MinWidth;
  if (Item.MaxWidth > Item.MinWidth) and (AWidth > Item.MaxWidth) then AWidth := Item.MaxWidth;
end;

constructor TTBXCustomListViewer.Create(AView: TTBView; AItem: TTBCustomItem; AGroupLevel: Integer);
var
  Index: Integer;
begin
  inherited;
  Index := TTBXCustomList(AItem).ItemIndex;
  FItemCount := TTBXCustomList(AItem).GetCount;
  if (Index >= 0) and (Index < FItemCount) then
    with TTBXCustomList(AItem) do
    begin
      FVisibleItems := GetCount;
      if FVisibleItems > MaxVisibleItems then FVisibleItems := MaxVisibleItems;
      if Index < FOffset then FOffset := Index
      else if Index >= FOffset + FVisibleItems then FOffset := Index - FVisibleItems + 1
    end;
  FHoverIndex := Index;
  if FHoverIndex > FItemCount then FHoverIndex := -1;
  AddToList(TTBXCustomList(AItem).FViewers, Self);
end;

destructor TTBXCustomListViewer.Destroy;
begin
  RemoveFromList(TTBXCustomList(Item).FViewers, Self);
  if FScrollBar <> nil then FScrollBar.Free;
  inherited;
end;

procedure TTBXCustomListViewer.DrawItems(const Canvas: TCanvas; const ClientAreaRect: TRect);
var
  I: Integer;
  R: TRect;
begin
  R := ClientAreaRect;
  R.Bottom := FItemHeight;
  Dec(R.Right, FScrollBarWidth);
  Canvas.Font := TTBViewAccess(View).GetFont;

  for I := FOffset to FItemCount - 1 do
  begin
    if RectVisible(Canvas.Handle, R) then
      TTBXCustomList(Item).DrawItem(Canvas, Self, R, I, HoverIndex);
    R.Top := R.Bottom;
    Inc(R.Bottom, FItemHeight);
    if R.Bottom > FHeight then Break;
  end;

  if R.Top < ClientAreaRect.Bottom then
  begin
    R.Bottom := ClientAreaRect.Bottom;
    Canvas.Brush.Color := clWindow;
    Canvas.FillRect(R);
  end;
end;

function TTBXCustomListViewer.GetItemHeight(ACanvas: TCanvas): Integer;
var
  ImgList: TCustomImageList;
begin
  Result := ACanvas.TextHeight('Q') + 2;
  with TTBXStringList(Item) do
  begin
    ImgList := GetImageList;
    if ShowImages and (ImgList <> nil) and (Result < ImgList.Height + 2) then
      Result := ImgList.Height + 2;
    DoMeasureHeight(ACanvas, Result);
  end;
end;

function TTBXCustomListViewer.GetItemIndexAt(X, Y: Integer): Integer;
begin
  if (X < 0) or (X > FWidth - FScrollBarWidth) then Result := -1
  else
  begin
    Result := (Y div FItemHeight) + FOffset;
    if (Result < FOffset) or (Result >= FOffset + FVisibleItems) or (Result >= FItemCount) then
      Result := - 1;
  end;
end;

function TTBXCustomListViewer.GetItemRect(Index: Integer): TRect;
begin
  { Note this method works properly only after Draw is called }
  Result := FLastClientRect;
  Inc(Result.Top, (Index - FOffset) * FItemHeight);
  Result.Bottom := Result.Top + FItemHeight;
  Dec(Result.Right, FScrollBarWidth);
end;

function TTBXCustomListViewer.GetItemWidth(ACanvas: TCanvas; Index: Integer): Integer;
var
  S: string;
  ImgList: TCustomImageList;
begin
  with TTBXStringList(Item) do
  begin
    S := GetItemText(Index);
    Result := ACanvas.TextWidth(S);
    if ShowImages then
    begin
      ImgList := GetImageList;
      if ImgList <> nil then
      begin
        Inc(Result, ImgList.Width);
        if Length(S) > 0 then Inc(Result, CImageSpacing);
      end;
    end;
    Inc(Result, 8);
    DoMeasureWidth(ACanvas, Index, Result)
  end;
end;

procedure TTBXCustomListViewer.HandleAutoScroll(var Direction, Interval: Integer);
begin
  // do nothing by default
end;

procedure TTBXCustomListViewer.KeyDown(var Key: Word; Shift: TShiftState);
var
  OldIndex, NewIndex: Integer;
begin
  OldIndex := FHoverIndex;
  case Key of
    VK_UP: NewIndex := OldIndex - 1;
    VK_DOWN: NewIndex := OldIndex + 1;
    VK_PRIOR: NewIndex := OldIndex - FVisibleItems;
    VK_NEXT: NewIndex := OldIndex + FVisibleItems;
    VK_HOME: NewIndex := 0;
    VK_END: NewIndex := FItemCount - 1;
    VK_RETURN:
      begin
        TTBXCustomList(Item).ItemIndex := FHoverIndex;
        Exit;
      end;
  else
    Exit;
  end;
  Key := 0;
  if NewIndex < 0 then NewIndex := 0;
  if NewIndex >= FItemCount then NewIndex := FItemCount - 1;
  TTBXCustomList(Item).ItemIndex := NewIndex;
end;

procedure TTBXCustomListViewer.ListChangeHandler(NewIndex: Integer);
begin
  if not IsChanging and (NewIndex <> HoverIndex) then
  begin
    IsChanging := True;
    HoverIndex := NewIndex;
    TTBXCustomList(Item).HandleHover(NewIndex);
    MakeVisible(HoverIndex);
    UpdateItems;
    IsChanging := False;
  end;
end;

procedure TTBXCustomListViewer.MakeVisible(Index: Integer);
begin
  if (Index >= 0) and (Index < FItemCount) then
  begin
    if Index < FOffset then FScrollBar.UpdatePosition(Index)
    else if Index >= FOffset + FVisibleItems then FScrollBar.UpdatePosition(Index - FVisibleItems + 1);
  end;
end;

procedure TTBXCustomListViewer.MouseDown(Shift: TShiftState; X, Y: Integer; var MouseDownOnMenu: Boolean);
begin
  if X > FWidth - FScrollBarWidth then
  begin
    Dec(X, FWidth - FScrollBarWidth);
    MouseInScrollBar := True;
    FScrollBar.MouseDown(mbLeft, X, Y);
    MouseDownOnMenu := False;
  end
  else
  begin
    MouseIsDown := True;
    MouseMove(X, Y);
  end;
  inherited;
  View.SetCapture;
end;

procedure TTBXCustomListViewer.MouseMove(X, Y: Integer);
var
  NewHoverIndex, OldHoverIndex, IndexLo, IndexHi, I: Integer;
  R: TRect;
  Canvas: TCanvas;
  DC: HDC;
  V, Dir: Integer;
begin
  if MouseInScrollBar then
  begin
    Dec(X, FWidth - FScrollBarWidth);
    FScrollBar.MouseMove(X, Y);
    Exit;
  end;

  if not View.Capture and (GetKeyState(VK_LBUTTON) < 0) then
  begin
    View.SetCapture;
    MouseIsDown := True;
  end;

  NewHoverIndex := GetItemIndexAt(X, Y);
  if FScrollBar <> nil then
  begin
    if MouseIsDown and ((Y < 0) or (Y >= FHeight)) then
    begin
      { Get AutoScroll Intervals }
      V := Y;
      if V >= FHeight then Dec(V, FHeight - 1);
      V := Abs(V);
      if Y < 0 then Dir := -1 else Dir := 1;
      case V of
        0..9: V := 150;
        10..29: V := 100;
        30..50: begin V := 100; Dir := Dir * 2; end;
      else
        V := 100;
        Dir := Dir * 4;
      end;

      if ((Dir < 0) and (FOffset > 0)) or
        ((Dir > 0) and (FOffset + FVisibleItems < FItemCount)) then
        FScrollBar.StartAutoScroll(Dir, V)
      else
        FScrollBar.StopAutoScroll;
      AdjustAutoScrollHover(NewHoverIndex, Dir);
    end
    else FScrollBar.StopAutoScroll;
  end;

  if not MouseIsDown and (NewHoverIndex = -1) then Exit;

  if NewHoverIndex <> FHoverIndex then
  begin
    Canvas := TCanvas.Create;
    DC := GetDC(View.Window.Handle);
    OldHoverIndex := FHoverIndex;
    FHoverIndex := NewHoverIndex;
    try
      SetWindowOrgEx(DC, -BoundsRect.Left, -BoundsRect.Top, nil);
      Canvas.Handle := DC;
      Canvas.Font := TTBViewAccess(View).GetFont;

      IndexLo := OldHoverIndex;
      IndexHi := FHoverIndex;
      if FHoverIndex < OldHoverIndex then
      begin
        IndexLo := FHoverIndex;
        IndexHi := OldHoverIndex;
      end;
      for I := IndexLo to IndexHi do
      begin
        R := GetItemRect(I);
        if (R.Top >= 0) and (R.Bottom <= FHeight) and RectVisible(DC, R) then
          TTBXCustomList(Item).DrawItem(Canvas, Self, R, I, HoverIndex);
      end;
    finally
      Canvas.Handle := 0;
      Canvas.Free;
      ReleaseDC(View.Window.Handle, DC);
    end;
    TTBXCustomList(Item).HandleHover(FHoverIndex);
  end;
end;

procedure TTBXCustomListViewer.MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean);
var
  DAD: TTBDoneActionData;
begin
  if FScrollBar <> nil then FScrollBar.StopAutoScroll;
  if MouseInScrollBar then
  begin
    inherited;
    Dec(X, FWidth - FScrollBarWidth);
    FScrollBar.MouseUp(mbLeft, X, Y);
    DAD := TTBViewAccess(TTBViewAccess(View).GetRootView).DoneActionData;
    DAD.DoneAction := tbdaNone;
    TTBViewAccess(TTBViewAccess(View).GetRootView).DoneActionData := DAD;
    MouseInScrollBar := False;
  end
  else if MouseIsDown then
  begin
    MouseIsDown := False;
    TTBXCustomList(Item).ItemIndex := FHoverIndex;
    inherited;
    DAD := TTBViewAccess(TTBViewAccess(View).GetRootView).DoneActionData;
    DAD.Sound := False;
    TTBViewAccess(TTBViewAccess(View).GetRootView).DoneActionData := DAD;
  end;
end;

procedure TTBXCustomListViewer.MouseWheel(WheelDelta, X, Y: Integer);
var
  IsNegative: Boolean;
begin
  if FScrollBar <> nil then
  begin
    Inc(FWheelAccumulator, WheelDelta);
    while Abs(FWheelAccumulator) >= WHEEL_DELTA do
    begin
      IsNegative := FWheelAccumulator < 0;
      FWheelAccumulator := Abs(FWheelAccumulator) - WHEEL_DELTA;
      if IsNegative then
      begin
        if FWheelAccumulator <> 0 then FWheelAccumulator := -FWheelAccumulator;
        FScrollBar.UpdatePosition(FScrollBar.Position + 1)
      end
      else
        FScrollBar.UpdatePosition(FScrollBar.Position - 1)
    end;
  end;
end;

procedure TTBXCustomListViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsHoverItem, IsPushed, UseDisabledShadow: Boolean);
begin
  { Cache some important info for later usage }
  FLastClientRect := ClientAreaRect;
  with ClientAreaRect do
  begin
    FWidth := Right - Left;
    FHeight := Bottom - Top;
  end;

  DrawItems(Canvas, ClientAreaRect);
  
  if FScrollBarWidth > 0 then
  begin
    if FScrollBar = nil then
    begin
      FScrollBar := TTBXScrollBar.Create;
      FScrollBar.Kind := sbVertical;
      FScrollBar.OnRedrawRequest := SBRedrawHandler;
      FScrollBar.OnChange := SBChangeHandler;
      FScrollBar.OnAutoScroll := SBAutoScrollHandler;
    end;
    FScrollBar.Bounds := Rect(ClientAreaRect.Right - FScrollBarWidth,
      ClientAreaRect.Top, ClientAreaRect.Right, ClientAreaRect.Bottom);
    FScrollBar.Range := FItemCount;
    FScrollBar.Window := FVisibleItems;
    FScrollBar.Position := FOffset;
    FScrollBar.PaintTo(Canvas);
  end;
end;

procedure TTBXCustomListViewer.SBAutoScrollHandler(Sender: TObject;
  var Direction, Interval: Integer);
begin
  HandleAutoScroll(Direction, Interval);
end;

procedure TTBXCustomListViewer.SBChangeHandler(Sender: TObject);
begin
  FOffset := FScrollBar.Position;
  UpdateItems;
end;

procedure TTBXCustomListViewer.SBRedrawHandler(Sender: TObject);
var
  DC: HDC;
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  DC := GetDC(View.Window.Handle);
  try
    SetWindowOrgEx(DC, -BoundsRect.Left, -BoundsRect.Top, nil);
    Canvas.Handle := DC;
    FScrollBar.PaintTo(Canvas);
  finally
    Canvas.Handle := 0;
    Canvas.Free;
    ReleaseDC(View.Window.Handle, DC);
  end;
end;

procedure TTBXCustomListViewer.UpdateItems;
var
  DC: HDC;
  Canvas: TCanvas;
begin
  if Assigned(FScrollBar) then FOffset := FScrollBar.Position
  else FOffset := 0;
  Canvas := TCanvas.Create;
  DC := GetDC(View.Window.Handle);
  try
    SetWindowOrgEx(DC, -BoundsRect.Left, -BoundsRect.Top, nil);
    Canvas.Handle := DC;
    DrawItems(Canvas, FLastClientRect);
  finally
    Canvas.Handle := 0;
    Canvas.Free;
    ReleaseDC(View.Window.Handle, DC);
  end;
end;


//----------------------------------------------------------------------------//

{ TTBXStringList }

constructor TTBXStringList.Create(AOwner: TComponent);
begin
  inherited;
  FStrings := TStringList.Create;
end;

destructor TTBXStringList.Destroy;
begin
  FStrings.Free;
  inherited;
end;

function TTBXStringList.GetCount: Integer;
begin
  Result := FStrings.Count;
end;

function TTBXStringList.GetItemText(Index: Integer): string;
begin
  Result := FStrings[Index];
end;

procedure TTBXStringList.SetStrings(Value: TStrings);
begin
  FStrings.Assign(Value);
end;


//----------------------------------------------------------------------------//

{$IFNDEF MPEXCLUDE}

{ TTBXUndoList }

procedure TTBXUndoList.DrawItem(ACanvas: TCanvas; AViewer: TTBXCustomListViewer;
  const ARect: TRect; AIndex, AHoverIndex: Integer);
const
  FillColors: array [Boolean] of TColor = (clWindow, clHighlight);
  TextColors: array [Boolean] of TColor = (clWindowText, clHighlightText);
var
  S: string;
  R: TRect;
begin
  ACanvas.Brush.Color := FillColors[AIndex <= AHoverIndex];
  ACanvas.FillRect(ARect);
  S := Strings[AIndex];
  if Length(S) > 0 then
  begin
    R := ARect;
    InflateRect(R, -4, 1);
    ACanvas.Font.Color := TextColors[AIndex <= AHoverIndex];
    ACanvas.Brush.Style := bsClear;
    DrawText(ACanvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_VCENTER);
    ACanvas.Brush.Style := bsSolid;
  end;
end;

function TTBXUndoList.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TTBXUndoListViewer;
end;

procedure TTBXUndoList.HandleHover(AIndex: Integer);
begin
  ItemIndex := AIndex;
end;


//----------------------------------------------------------------------------//

{ TTBXUndoListViewer }

procedure TTBXUndoListViewer.AdjustAutoScrollHover(var AIndex: Integer; Direction: Integer);
begin
  if Direction < 0 then AIndex := FOffset
  else if Direction > 0 then AIndex := FOffset + FVisibleItems - 1;
end;

procedure TTBXUndoListViewer.HandleAutoScroll(var Direction, Interval: Integer);
begin
  inherited;
  if Direction < 0 then HoverIndex := FOffset
  else if Direction > 0 then HoverIndex := FOffset + FVisibleItems - 1
  else Exit;
  TTBXCustomList(Item).HandleHover(HoverIndex);
  UpdateItems;
end;

{$ENDIF}

end.
