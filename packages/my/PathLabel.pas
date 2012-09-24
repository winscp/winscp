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
    FDisplayHotTrack: string;
    FDisplayMask: string;
    FHotTrack: Boolean;
    FMouseInView: Boolean;
    FIsActive: Boolean;
    FMask: string;
    FAutoSizeVertical: Boolean;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    function GetColors(Index: Integer): TColor;
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetIndentHorizontal(AIndent: Integer);
    procedure SetIndentVertical(AIndent: Integer);
    procedure SetUnixPath(AUnixPath: Boolean);
    procedure SetMask(Value: string);
    procedure SetAutoSizeVertical(Value: Boolean);
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
    property Mask: string read FMask write SetMask;
    property AutoSizeVertical: Boolean read FAutoSizeVertical write SetAutoSizeVertical default False;

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
    property AutoSizeVertical;
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
  FileCtrl, SysUtils, Math;

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

procedure TCustomPathLabel.SetMask(Value: string);
begin
  if FMask <> Value then
  begin
    FMask := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCustomPathLabel.SetColors(Index: integer; Value: TColor);
begin
  Assert(Index in [0..5]);
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

procedure TCustomPathLabel.SetAutoSizeVertical(Value: Boolean);
begin
  if FAutoSizeVertical <> Value then
  begin
    FAutoSizeVertical := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCustomPathLabel.DoDrawText(var Rect: TRect; Flags: Longint);
var
  i: Integer;
  StandardColor: TColor;
  Width: Integer;
  WidthMask: Integer;
  WidthPath: Integer;
  HotTrackOffset: Integer;
  HotTrackBottom: Integer;
  Separator: string;
  Str: string;
begin
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
  if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
  Flags := DrawTextBiDiModeFlags(Flags);
  Canvas.Font := Font;

  Width := (Rect.Right - Rect.Left);

  FDisplayPath := Caption;
  FDisplayMask := Mask;
  Separator := '';

  if FDisplayMask <> '' then
  begin
    if FUnixPath then
    begin
      if (Length(FDisplayPath) > 0) and (FDisplayPath[Length(FDisplayPath)] <> '/') then
        Separator := '/';
    end
      else
    begin
      if (Length(FDisplayPath) > 0) and (FDisplayPath[Length(FDisplayPath)] <> '\') then
        Separator := '\';
    end;

    FDisplayPath := FDisplayPath + Separator;

    WidthMask := Canvas.TextWidth(FDisplayMask);
    if WidthMask > Width div 3 then
      WidthPath := Width - (Width div 3)
    else
      WidthPath := Width - WidthMask;
  end
    else
  begin
    WidthMask := 0;
    WidthPath := Width;
  end;

  if FUnixPath then
    for i := 1 to Length(FDisplayPath) do
    begin
      case FDisplayPath[i] of
        '/': FDisplayPath[i] := '\';
        '\': FDisplayPath[i] := '/';
      end;
    end;
  FDisplayPath := MinimizeName(FDisplayPath, Canvas, WidthPath);
  if FUnixPath then
    for i := 1 to Length(FDisplayPath) do
    begin
      case FDisplayPath[i] of
        '\': FDisplayPath[i] := '/';
        '/': FDisplayPath[i] := '\';
      end;
    end;

  WidthPath := Canvas.TextWidth(FDisplayPath);

  if FDisplayMask <> '' then
  begin
    if WidthMask > Width - WidthPath then
    begin
      FDisplayMask := FDisplayMask + '...';
      repeat
        Delete(FDisplayMask, Length(FDisplayMask) - 3, 1);
        WidthMask := Canvas.TextWidth(FDisplayMask);
      until (WidthMask <= Width - WidthPath) or (Length(FDisplayMask) = 3);
    end;
  end;

  ShowHint :=
    (FDisplayPath <> Caption) or
    (FDisplayMask <> Mask) or
    (WidthPath + WidthMask > Width);

  if not ShowHint then Hint := ''
    else Hint := Caption + Separator + Mask;

  Str := FDisplayPath + FDisplayMask;
  if not Enabled then
  begin
    OffsetRect(Rect, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawText(Canvas.Handle, PChar(Str), Length(Str), Rect, Flags);
    OffsetRect(Rect, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawText(Canvas.Handle, PChar(Str), Length(Str), Rect, Flags);
  end
    else
  begin
    FDisplayHotTrack := HotTrackPath(FDisplayPath);
    if FDisplayHotTrack <> '' then
    begin
      StandardColor := Canvas.Font.Color;
      Canvas.Font.Color := FColors[4 + Integer(FIsActive)];
      DrawText(Canvas.Handle, PChar(FDisplayHotTrack), Length(FDisplayHotTrack), Rect, Flags);
      Canvas.Font.Color := StandardColor;
      HotTrackOffset := Canvas.TextWidth(FDisplayHotTrack);
      Inc(Rect.Left, HotTrackOffset);
      Delete(Str, 1, Length(FDisplayHotTrack));
      HotTrackBottom := Rect.Bottom;
    end
      else
    begin
      HotTrackOffset := 0;
      HotTrackBottom := 0;
    end;

    DrawText(Canvas.Handle, PChar(Str), Length(Str), Rect, Flags);

    Dec(Rect.Left, HotTrackOffset);
    Rect.Bottom := Max(Rect.Bottom, HotTrackBottom);
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
  Assert(Index in [0..5]);
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
  AWidth: Integer;
  AHeight: Integer;
begin
  if not (csReading in ComponentState) and
     (AutoSize or AutoSizeVertical) and
     (Caption <> '') then
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

    if AutoSize then AWidth := 2*FIndentHorizontal + Rect.Right
      else AWidth := Width;
    if AutoSize or AutoSizeVertical then AHeight := 2*FIndentVertical + Rect.Bottom
      else AHeight := Height;

    SetBounds(X, Top, AWidth, AHeight);
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
  // We don't want to store Font properties in DFM
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
  if FMouseInView and HotTrack and (FDisplayHotTrack <> HotTrackPath(FDisplayPath)) then
  begin
    Invalidate;
  end;
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
