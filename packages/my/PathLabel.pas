unit PathLabel;

interface

{$WARN UNIT_PLATFORM OFF}

uses
  Messages, StdCtrls, Controls, Classes, Forms, Windows, Graphics;

type
  TCustomPathLabel = class;

  TPathLabelGetStatusEvent = procedure(Sender: TCustomPathLabel; var Active: Boolean) of object;
  TPathLabelPathClickEvent = procedure(Sender: TCustomPathLabel; Path: string) of object;

  TCustomPathLabel = class(TCustomLabel)
  private
    FColors: array[0..5] of TColor;
    FIndentHorizontal: Integer;
    FIndentVertical: Integer;
    FUnixPath: Boolean;
    FOnGetStatus: TPathLabelGetStatusEvent;
    FOnPathClick: TPathLabelPathClickEvent;
    FDisplayPath: string;
    FHotTrack: Boolean;
    FMouseInView: Boolean;
    FIsActive: Boolean;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    function GetColors(Index: Integer): TColor;
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetIndentHorizontal(AIndent: Integer);
    procedure SetIndentVertical(AIndent: Integer);
    procedure SetUnixPath(AUnixPath: Boolean);
  protected
    procedure AdjustBounds; override;
    procedure Click; override;
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; override;
    function IsActive: Boolean;
    function HotTrackPath(Path: string): string;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure DoPathClick(Path: string); virtual;

  public
    constructor Create(AnOwner: TComponent); override;
    procedure UpdateStatus;

    property ActiveColor: TColor index 1 read GetColors write SetColors
      default clActiveCaption;
    property ActiveTextColor: TColor index 3 read GetColors write SetColors
      default clCaptionText;
    property ActiveHotTrackColor: TColor index 5 read GetColors write SetColors
      default clGradientActiveCaption;
    property UnixPath: Boolean read FUnixPath write SetUnixPath default False;
    property IndentHorizontal: Integer read FIndentHorizontal
      write SetIndentHorizontal default 5;
    property IndentVertical: Integer read FIndentVertical
      write SetIndentVertical default 1;
    property InactiveColor: TColor index 0 read GetColors write SetColors
      default clInactiveCaption;
    property InactiveTextColor: TColor index 2 read GetColors write SetColors
      default clInactiveCaptionText;
    property InactiveHotTrackColor: TColor index 4 read GetColors write SetColors
      default clGradientInactiveCaption;
    property OnGetStatus: TPathLabelGetStatusEvent read FOnGetStatus write FOnGetStatus;
    property OnPathClick: TPathLabelPathClickEvent read FOnPathClick write FOnPathClick;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;

    property FocusControl;
    property Caption;
    property Hint stored False;
    property Align default alTop;
  end;

type
  TPathLabel = class(TCustomPathLabel)
  published
    property ActiveColor;
    property ActiveTextColor;
    property ActiveHotTrackColor;
    property UnixPath;
    property IndentHorizontal;
    property IndentVertical;
    property InactiveColor;
    property InactiveTextColor;
    property InactiveHotTrackColor;
    property HotTrack;
    property OnGetStatus;
    property OnPathClick;

    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentFont;
    property PopupMenu;
    property Transparent;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses
  { SysUtils must overload deprecated FileCtrl (implements MinimizeName) }
  FileCtrl, SysUtils;

procedure Register;
begin
  RegisterComponents('Martin', [TPathLabel]);
end;

{ TCustomPathLabel }

constructor TCustomPathLabel.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  WordWrap := False;
  Align := alTop;
  ShowAccelChar := False;
  FIndentHorizontal := 5;
  FIndentVertical := 1;
  FUnixPath := False;
  FHotTrack := False;
  FColors[0] := clInactiveCaption;
  FColors[1] := clActiveCaption;
  FColors[2] := clInactiveCaptionText;
  FColors[3] := clCaptionText;
  FColors[4] := clGradientInactiveCaption;
  FColors[5] := clGradientActiveCaption;
end;

procedure TCustomPathLabel.CMHintShow(var Message: TMessage);
begin
  with TCMHintShow(Message).HintInfo^ do
  begin
    HintPos.X := ClientOrigin.X + IndentHorizontal - 3;
    HintPos.Y := ClientOrigin.Y + IndentVertical - 3;
    if HotTrack then Inc(HintPos.Y, Height);
  end;
end; { CMHintShow }

procedure TCustomPathLabel.Click;
var
  HotPath: string;
  RemainingPath: string;
begin
  HotPath := HotTrackPath(FDisplayPath);
  if HotPath <> '' then
  begin
    if FDisplayPath = Caption then DoPathClick(HotPath)
      else
    begin
      // Displayed path is shortened.
      // The below is based on knowledge in MinimizePath algorithm
      RemainingPath := Copy(FDisplayPath, Length(HotPath) + 1,
        Length(FDisplayPath) - Length(HotPath));


      if RemainingPath = Copy(Caption, Length(Caption) - Length(RemainingPath) + 1,
           Length(RemainingPath)) then
      begin
        DoPathClick(Copy(Caption, 1, Length(Caption) - Length(RemainingPath)));
      end
        else
      if HotPath = Copy(Caption, 1, Length(HotPath)) then
      begin
        DoPathClick(HotPath);
      end
        else Assert(False);
    end;
  end;

  if Assigned(FocusControl) then FocusControl.SetFocus;
  inherited;
end; { Click }

procedure TCustomPathLabel.SetUnixPath(AUnixPath: Boolean);
begin
  if FUnixPath <> AUnixPath then
  begin
    FUnixPath := AUnixPath;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCustomPathLabel.SetColors(Index: integer; Value: TColor);
begin
  Assert(Index in [0..3]);
  if FColors[Index] <> Value then
  begin
    FColors[Index] := Value;
    UpdateStatus;
  end;
end; { SetColors }

procedure TCustomPathLabel.SetIndentHorizontal(AIndent: Integer);
begin
  if FIndentHorizontal <> AIndent then
  begin
    FIndentHorizontal := AIndent;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCustomPathLabel.SetIndentVertical(AIndent: Integer);
begin
  if FIndentVertical <> AIndent then
  begin
    FIndentVertical := AIndent;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCustomPathLabel.DoDrawText(var Rect: TRect; Flags: Longint);
var
  i: Integer;
  Path: string;
  HotPath: string;
  StandardColor: TColor;
begin
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
  if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
  Flags := DrawTextBiDiModeFlags(Flags);
  Canvas.Font := Font;
  Path := Caption;
  if FUnixPath then
    for i := 1 to Length(Path) do
      if Path[i] = '/' then Path[i] := '\';
  Path := MinimizeName(Path, Canvas, Rect.Right - Rect.Left);
  if FUnixPath then
    for i := 1 to Length(Path) do
      if Path[i] = '\' then Path[i] := '/';
  ShowHint := 
    (Path <> Caption) or
    (Canvas.TextWidth(Caption) > Rect.Right - Rect.Left);
  if not ShowHint then Hint := ''
    else
  if Hint <> Caption then Hint := Caption;

  FDisplayPath := Path;
  if not Enabled then
  begin
    OffsetRect(Rect, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawText(Canvas.Handle, PChar(Path), Length(Path), Rect, Flags);
    OffsetRect(Rect, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawText(Canvas.Handle, PChar(Path), Length(Path), Rect, Flags);
  end
    else
  begin
    HotPath := HotTrackPath(FDisplayPath);
    if HotPath <> '' then
    begin
      StandardColor := Canvas.Font.Color;
      Canvas.Font.Color := FColors[4 + Integer(FIsActive)];
      DrawText(Canvas.Handle, PChar(HotPath), Length(HotPath), Rect, Flags);
      Canvas.Font.Color := StandardColor;
      Inc(Rect.Left, Canvas.TextWidth(HotPath));
      Delete(Path, 1, Length(HotPath));
    end;
    DrawText(Canvas.Handle, PChar(Path), Length(Path), Rect, Flags);
  end;
end;

function TCustomPathLabel.HotTrackPath(Path: string): string;
var
  P: TPoint;
  DelimPos: Integer;
  Delim: Char;
  Len: Integer;
begin
  Result := '';
  if FHotTrack and FMouseInView and (Path <> '') then
  begin
    P := ScreenToClient(Mouse.CursorPos);
    Len := P.X - FIndentHorizontal;
    if (Len >= 0) and (Len < Canvas.TextWidth(Path)) then
    begin
      if FUnixPath then Delim := '/'
        else Delim := '\';

      Result := '';
      repeat
        Assert(Path <> '');

        DelimPos := Pos(Delim, Path);
        if DelimPos > 0 then
        begin
          Result := Result + Copy(Path, 1, DelimPos);
          Delete(Path, 1, DelimPos);
        end
          else
        begin
          Result := Result + Path;
          Path := '';
        end;

      until (Canvas.TextWidth(Result) >= Len) or (Path = '');
    end;
  end;
end;

function TCustomPathLabel.GetColors(Index: Integer): TColor;
begin
  Assert(Index in [0..3]);
  Result := FColors[Index];
end; { GetColors }

procedure TCustomPathLabel.Paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  Rect, CalcRect: TRect;
  DrawStyle: Longint;
begin
  with Canvas do
  begin
    if not Transparent then
    begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      FillRect(ClientRect);
    end;
    Brush.Style := bsClear;
    Rect := ClientRect;
    // DoDrawText takes care of BiDi alignments
    DrawStyle := DT_EXPANDTABS or WordWraps[WordWrap] or Alignments[Alignment];
    // MP
    Rect.Left := Rect.Left + FIndentHorizontal;
    Rect.Right := Rect.Right - FIndentHorizontal;
    Rect.Top := Rect.Top + FIndentVertical;
    Rect.Bottom := Rect.Bottom - FIndentVertical;
    // Calculate vertical layout
    if Layout <> tlTop then
    begin
      CalcRect := Rect;
      DoDrawText(CalcRect, DrawStyle or DT_CALCRECT);
      if Layout = tlBottom then OffsetRect(Rect, 0, Height - CalcRect.Bottom)
      else OffsetRect(Rect, 0, (Height - CalcRect.Bottom) div 2);
    end;
    DoDrawText(Rect, DrawStyle);
  end;
end;

procedure TCustomPathLabel.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DC: HDC;
  X: Integer;
  Rect: TRect;
  AAlignment: TAlignment;
begin
  if not (csReading in ComponentState) and AutoSize and (Caption <> '') then
  begin
    Rect := ClientRect;
    DC := GetDC(0);
    Canvas.Handle := DC;
    DoDrawText(Rect, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[WordWrap]);
    Canvas.Handle := 0;
    ReleaseDC(0, DC);
    X := Left;
    AAlignment := Alignment;
    if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
    if AAlignment = taRightJustify then Inc(X, Width - 2*FIndentHorizontal + Rect.Right);
    SetBounds(X, Top, 2*FIndentHorizontal + Rect.Right,
      2*FIndentVertical + Rect.Bottom);
  end;
end;

function TCustomPathLabel.IsActive: Boolean;
begin
  if csDestroying in ComponentState then Result := False
    else
  begin
    Result := Assigned(FocusControl) and FocusControl.Focused;
    if Assigned(OnGetStatus) then
      OnGetStatus(Self, Result);
  end;
end;

procedure TCustomPathLabel.UpdateStatus;
begin
  FIsActive := IsActive;
  Color := FColors[Integer(FIsActive)];
  // We don't want to stote Font properties in DFM
  // which would be if Font.Color is set to something else than clWindowText
  if not (csDesigning in ComponentState) then
    Font.Color := FColors[2 + Integer(FIsActive)];
end; { UpdateStatus }

procedure TCustomPathLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  NeedUpdate: Boolean;
begin
  NeedUpdate :=
    (Operation = opRemove) and (AComponent = FocusControl);
  inherited;
  if NeedUpdate then UpdateStatus;
end; { Notification }

procedure TCustomPathLabel.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited;
  if FMouseInView then Invalidate;
end;

procedure TCustomPathLabel.DoPathClick(Path: string);
begin
  if Assigned(OnPathClick) then
    OnPathClick(Self, Path);
end;

procedure TCustomPathLabel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseInView := True;
end;

procedure TCustomPathLabel.CMMouseLeave(var Message: TMessage);
begin
  FMouseInView := False;
  Invalidate;
  inherited;
end;

end.
