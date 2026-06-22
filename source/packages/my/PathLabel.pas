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
    FOnMaskClick: TNotifyEvent;
    FDisplayPath: string;
    FShortenedDisplayPath: string;
    FShortenedPath: string;
    FDisplayHotTrack: string;
    FDisplayMask: string;
    FHotTrack: Boolean;
    FMouseInView: Boolean;
    FIsActive: Boolean;
    FIsEnabled: Boolean;
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
    procedure SetFocusControl(Value: TWinControl);
    function GetFocusControl: TWinControl;
    function CalculateAutoHotTrackColor(C: TColor): TColor;
    procedure CalculateAutoHotTrackColors;
    function CalculateAutoHotTrackColorComponent(C: Byte; Bright: Boolean): Byte;
    function UseHotTrack: Boolean;
  protected
    procedure AdjustBounds; override;
    procedure Click; override;
    procedure DoDrawTextIntern(var Rect: TRect; Flags: Longint; S: string);
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; override;
    function IsActive: Boolean;
    function TrackingActive: Boolean;
    function HotTrackPath: string;
    function GetSeparator: Char;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure DoPathClick(Path: string); virtual;
    procedure DoMaskClick;
    procedure DblClick; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;

  public
    constructor Create(AnOwner: TComponent); override;
    procedure UpdateStatus;
    function UseRightToLeftAlignment: Boolean; override;

    property ActiveColor: TColor index 1 read GetColors write SetColors
      default clActiveCaption;
    property ActiveTextColor: TColor index 3 read GetColors write SetColors
      default clCaptionText;
    property UnixPath: Boolean read FUnixPath write SetUnixPath default False;
    property IndentHorizontal: Integer read FIndentHorizontal
      write SetIndentHorizontal default 5;
    property IndentVertical: Integer read FIndentVertical
      write SetIndentVertical default 1;
    property InactiveColor: TColor index 0 read GetColors write SetColors
      default clInactiveCaption;
    property InactiveTextColor: TColor index 2 read GetColors write SetColors
      default clInactiveCaptionText;
    property OnGetStatus: TPathLabelGetStatusEvent read FOnGetStatus write FOnGetStatus;
    property OnPathClick: TPathLabelPathClickEvent read FOnPathClick write FOnPathClick;
    property OnMaskClick: TNotifyEvent read FOnMaskClick write FOnMaskClick;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property Mask: string read FMask write SetMask;
    property AutoSizeVertical: Boolean read FAutoSizeVertical write SetAutoSizeVertical default False;

    property FocusControl: TWinControl read GetFocusControl write SetFocusControl;
    property Caption;
    property Hint stored False;
    property Align default alTop;
  end;

type
  TPathLabel = class(TCustomPathLabel)
  published
    property ActiveColor;
    property ActiveTextColor;
    property UnixPath;
    property IndentHorizontal;
    property IndentVertical;
    property InactiveColor;
    property InactiveTextColor;
    property AutoSizeVertical;
    property HotTrack;
    property OnGetStatus;
    property OnPathClick;
    property OnMaskClick;

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
  FileCtrl, SysUtils, Math, PasTools, StrUtils;

const
  // magic value
  HotTrackMask: string = #1;

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
  FIsEnabled := True;
  CalculateAutoHotTrackColors;
end;

procedure TCustomPathLabel.CMHintShow(var Message: TMessage);
begin
  with TCMHintShow(Message).HintInfo^ do
  begin
    HintPos.X := ClientOrigin.X + IndentHorizontal;
    HintPos.Y := ClientOrigin.Y + IndentVertical;
    if UseHotTrack then Inc(HintPos.Y, Height);
  end;
end; { CMHintShow }

procedure TCustomPathLabel.Click;
var
  HotPath: string;
begin
  if FIsEnabled then
  begin
    HotPath := HotTrackPath;
    if HotPath <> '' then
    begin
      if HotPath = HotTrackMask then DoMaskClick
        else
      begin
        if FShortenedPath <> '' then
        begin
          // No matter the shortening algo, when the clicked path is the full displayed path, use full original path
          if HotPath = FDisplayPath then HotPath := Caption
            else
          begin
            Assert(FShortenedDisplayPath <> '');
            Assert((Length(HotPath) >= Length(FShortenedDisplayPath)) or (not ContainsStr(HotPath, '...')));
            if Length(HotPath) >= Length(FShortenedDisplayPath) then
            begin
              Assert(StartsStr(FShortenedDisplayPath, HotPath));
              if StartsStr(FShortenedDisplayPath, HotPath) then
              begin
                Delete(HotPath, 1, Length(FShortenedDisplayPath));
                HotPath := FShortenedPath + HotPath;
              end;
            end;
          end;
        end;

        if EndsStr(GetSeparator, HotPath) and
           (HotPath <> GetSeparator) then
          SetLength(HotPath, Length(HotPath) - 1);

        DoPathClick(HotPath);
      end;
    end;

    if Assigned(FocusControl) then FocusControl.SetFocus;

    inherited;
  end;
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
  Assert(Index in [0..3]);
  if FColors[Index] <> Value then
  begin
    FColors[Index] := Value;

    CalculateAutoHotTrackColors;

    UpdateStatus;
  end;
end; { SetColors }

// taken from PngImageListEditor

const
  WeightR: single = 0.764706;
  WeightG: single = 1.52941;
  WeightB: single = 0.254902;

function ColorDistance(C1, C2: Integer): Single;
var
  DR, DG, DB: Integer;
begin
  DR := (C1 and $FF) - (C2 and $FF);
  Result := Sqr(DR * WeightR);
  DG := (C1 shr 8 and $FF) - (C2 shr 8 and $FF);
  Result := Result + Sqr(DG * WeightG);
  DB := (C1 shr 16) - (C2 shr 16);
  Result := Result + Sqr(DB * WeightB);
  Result := Sqrt(Result);
end;

function GetAdjustedThreshold(BkgndIntensity, Threshold: Single): Single;
begin
  if BkgndIntensity < 220 then
    Result := (2 - BkgndIntensity / 220) * Threshold
  else
    Result := Threshold;
end;

function IsContrastEnough(AColor, ABkgndColor: Integer; DoAdjustThreshold: Boolean; Threshold: Single): Boolean;
begin
  if DoAdjustThreshold then
    Threshold := GetAdjustedThreshold(ColorDistance(ABkgndColor, $000000),
      Threshold);
  Result := ColorDistance(ABkgndColor, AColor) > Threshold;
end;

procedure AdjustContrast(var AColor: Integer; ABkgndColor: Integer; Threshold: Single);
var
  X, Y, Z: Single;
  R, G, B: Single;
  RR, GG, BB: Integer;
  I1, I2, S, Q, W: Single;
  DoInvert: Boolean;
begin
  I1 := ColorDistance(AColor, $000000);
  I2 := ColorDistance(ABkgndColor, $000000);
  Threshold := GetAdjustedThreshold(I2, Threshold);

  if I1 > I2 then
    DoInvert := I2 < 442 - Threshold
  else
    DoInvert := I2 < Threshold;

  X := (ABkgndColor and $FF) * WeightR;
  Y := (ABkgndColor shr 8 and $FF) * WeightG;
  Z := (ABkgndColor shr 16) * WeightB;

  R := (AColor and $FF) * WeightR;
  G := (AColor shr 8 and $FF) * WeightG;
  B := (AColor shr 16) * WeightB;

  if DoInvert then begin
    R := 195 - R;
    G := 390 - G;
    B := 65 - B;
    X := 195 - X;
    Y := 390 - Y;
    Z := 65 - Z;
  end;

  S := Sqrt(Sqr(B) + Sqr(G) + Sqr(R));
  if S < 0.01 then
    S := 0.01;

  Q := (R * X + G * Y + B * Z) / S;

  X := Q / S * R - X;
  Y := Q / S * G - Y;
  Z := Q / S * B - Z;

  W := Sqrt(Sqr(Threshold) - Sqr(X) - Sqr(Y) - Sqr(Z));

  R := (Q - W) * R / S;
  G := (Q - W) * G / S;
  B := (Q - W) * B / S;

  if DoInvert then begin
    R := 195 - R;
    G := 390 - G;
    B := 65 - B;
  end;

  if R < 0 then
    R := 0
  else if R > 195 then
    R := 195;
  if G < 0 then
    G := 0
  else if G > 390 then
    G := 390;
  if B < 0 then
    B := 0
  else if B > 65 then
    B := 65;

  RR := Trunc(R * (1 / WeightR) + 0.5);
  GG := Trunc(G * (1 / WeightG) + 0.5);
  BB := Trunc(B * (1 / WeightB) + 0.5);

  if RR > $FF then
    RR := $FF
  else if RR < 0 then
    RR := 0;
  if GG > $FF then
    GG := $FF
  else if GG < 0 then
    GG := 0;
  if BB > $FF then
    BB := $FF
  else if BB < 0 then
    BB := 0;

  AColor := (BB and $FF) shl 16 or (GG and $FF) shl 8 or (RR and $FF);
end;

procedure SetContrast(var Color: TColor; BkgndColor: TColor; Threshold: Integer);
var
  T: Single;
begin
  if Color < 0 then
    Color := GetSysColor(Color and $FF);
  if BkgndColor < 0 then
    BkgndColor := GetSysColor(BkgndColor and $FF);
  T := Threshold;
  if not IsContrastEnough(Color, BkgndColor, True, T) then
    AdjustContrast(Integer(Color), BkgndColor, T);
end;

function TCustomPathLabel.CalculateAutoHotTrackColorComponent(C: Byte; Bright: Boolean): Byte;
var
  Delta: Byte;
begin
  Delta := Max(Round(C * 0.3), 80);
  if Bright then
    Result := Byte(Max(Integer(C) - Delta, 0))
  else
    Result := Byte(Min(C + Delta, 255));
end;

function TCustomPathLabel.CalculateAutoHotTrackColor(C: TColor): TColor;
var
  R, G, B: Byte;
  Bright: Boolean;
begin
  C := ColorToRGB(C);

  R := GetRValue(C);
  G := GetGValue(C);
  B := GetBValue(C);

  Bright := (R + G + B) > (256 / 2 * 3);

  R := CalculateAutoHotTrackColorComponent(R, Bright);
  G := CalculateAutoHotTrackColorComponent(G, Bright);
  B := CalculateAutoHotTrackColorComponent(B, Bright);

  Result := RGB(R, G, B);
end;

procedure TCustomPathLabel.CalculateAutoHotTrackColors;
begin
  FColors[4] := CalculateAutoHotTrackColor(FColors[2]);
  SetContrast(FColors[4], FColors[0], 50);
  FColors[5] := CalculateAutoHotTrackColor(FColors[3]);
  SetContrast(FColors[5], FColors[1], 50);
end;

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

function TCustomPathLabel.GetFocusControl: TWinControl;
begin
  Result := inherited FocusControl;
end;

procedure TCustomPathLabel.SetFocusControl(Value: TWinControl);
begin
  if FocusControl <> Value then
  begin
    inherited FocusControl := Value;
    UpdateStatus;
  end;
end;

function MinimizeStr(Buf: string; Canvas: TCanvas; Width: Integer): string;
var
  StrWidth: Integer;
begin
  if Canvas.TextWidth(Buf) > Width then
  begin
    Buf := Buf + '...';
    repeat
      Delete(Buf, Length(Buf) - 3, 1);
      StrWidth := Canvas.TextWidth(Buf);
    until (StrWidth <= Width) or (Length(Buf) = 4);
  end;
  Result := Buf;
end;

function ConvertPath(Path: string; S1, S2: Char): string;
var
  I: Integer;
begin
  for I := 1 to Length(Path) do
  begin
    if Path[I] = S1 then Path[I] := S2
      else
    if Path[I] = S2 then Path[I] := S1;
  end;
  Result := Path;
end;

function ConvertPathToWin(Path: string): string;
begin
  Result := ConvertPath(Path, '/', '\');
end;

function ConvertPathToUnix(Path: string): string;
begin
  Result := ConvertPath(Path, '\', '/');
end;

procedure TCustomPathLabel.DoDrawTextIntern(var Rect: TRect; Flags: Longint; S: string);
var
  I, I2, L: Integer;
  Width: Integer;
  WidthMask: Integer;
  WidthPath: Integer;
  WidthRemain: Integer;
  HotTrackOffset: Integer;
  HotTrackBottom: Integer;
  Separator, Buf, Buf2: string;
  Str: string;
  HotTrackColor: TColor;
  VirtualMask: string;
  IsEmptyMask: Boolean;
  IsVirtualMask: Boolean;
begin
  if (Flags and DT_CALCRECT <> 0) and ((S = '') or ShowAccelChar and
    (S[1] = '&') and (S[2] = #0)) then S := S + ' ';
  if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
  // have to apply DrawTextBiDiModeFlags if we ever deal with dibi
  Canvas.Font := Font;

  Width := (Rect.Right - Rect.Left);

  FDisplayPath := S;

  VirtualMask := Mask;
  IsVirtualMask := False;
  IsEmptyMask := (VirtualMask = '');
  if IsEmptyMask and UseHotTrack then
  begin
    VirtualMask := '*.*';
    IsVirtualMask := not FMouseInView;
  end;

  Separator := GetSeparator;
  if VirtualMask <> '' then
  begin
    if (Length(FDisplayPath) > 0) and (FDisplayPath[Length(FDisplayPath)] <> GetSeparator) then
      Buf := Separator;

    FDisplayPath := FDisplayPath + Buf;
    S := S + Buf;

    WidthMask := Canvas.TextWidth(VirtualMask);
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
    FDisplayPath := ConvertPathToWin(FDisplayPath);

  Buf := FDisplayPath;
  WidthRemain := WidthPath;
  if EndsStr('\', FDisplayPath) then
  begin
    Dec(WidthRemain, Canvas.TextWidth('\'));
    SetLength(Buf, Length(Buf) - 1);
  end;
  Buf2 := ExtractFileName(Buf);
  FShortenedDisplayPath := '...\';
  if WidthRemain < Canvas.TextWidth(FShortenedDisplayPath + Buf2) then
  begin
    Dec(WidthRemain, Canvas.TextWidth(FShortenedDisplayPath));
    Buf2 := MinimizeStr(Buf2, Canvas, WidthRemain);
    FShortenedPath := ExtractFilePath(Buf);
    Buf := FShortenedDisplayPath + Buf2;
  end
    else
  begin
    Buf2 := MinimizeName(Buf, Canvas, WidthRemain);
    if Buf2 = Buf then
    begin
      FShortenedPath := '';
      FShortenedDisplayPath := '';
    end
      else
    begin
      I := Length(Buf);
      repeat
        I2 := LastDelimiter('\', Copy(Buf, 1, I - 1));
        if I2 = 0 then
          Break;
        L := Length(Buf) - I2 + 1;
        if Copy(Buf, I2, L) = Copy(Buf2, Length(Buf2) - L + 1, L) then
          I := I2
        else
          Break;
      until False;
      FShortenedPath := Copy(Buf, 1, I);
      FShortenedDisplayPath := Copy(Buf2, 1, Length(Buf2) - (Length(Buf) - I));
      Buf := Buf2;
    end;
  end;
  if EndsStr('\', FDisplayPath) then
  begin
    Buf := Buf + '\';
  end;
  FDisplayPath := Buf;

  if FUnixPath then
  begin
    FDisplayPath := ConvertPathToUnix(FDisplayPath);
    FShortenedPath := ConvertPathToUnix(FShortenedPath);
    FShortenedDisplayPath := ConvertPathToUnix(FShortenedDisplayPath);
  end;

  WidthPath := Canvas.TextWidth(FDisplayPath);

  if VirtualMask <> '' then
  begin
    VirtualMask := MinimizeStr(VirtualMask, Canvas, Width - WidthPath);
    WidthMask := Canvas.TextWidth(VirtualMask);
  end;

  Str := FDisplayPath;
  if not IsVirtualMask then
  begin
    Str := Str + VirtualMask;
    FDisplayMask := VirtualMask;
  end
    else
  begin
    FDisplayMask := '';
  end;

  ShowHint :=
    (FDisplayPath <> S) or
    ((not IsEmptyMask) and (FDisplayMask <> Mask)) or
    (WidthPath + WidthMask > Width);

  if not ShowHint then Hint := ''
    else Hint := S + Mask;

  if not FIsEnabled then
  begin
    Canvas.Font.Color := clBtnShadow;
    DrawText(Canvas.Handle, PChar(Str), Length(Str), Rect, Flags);
  end
    else
  begin
    FDisplayHotTrack := HotTrackPath;

    HotTrackColor := TColor(0); // shut up
    if FDisplayHotTrack <> '' then
    begin
      if TrackingActive then
      begin
        HotTrackColor := FColors[4 + Integer(FIsActive)]
      end
        else
      begin
        // We do not have a path label with hot-track and not tracking-active,
        // so this is untested branch
        Assert(False);
        // As if it were active
        HotTrackColor := FColors[2 + 1];
      end;
    end;

    if (FDisplayHotTrack <> '') and (FDisplayHotTrack <> HotTrackMask) then
    begin
      Canvas.Font.Color := HotTrackColor;
      DrawText(Canvas.Handle, PChar(FDisplayHotTrack), Length(FDisplayHotTrack), Rect, Flags);
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

    if TrackingActive then
      Canvas.Font.Color := FColors[2 + Integer(FIsActive)]
    else
      Canvas.Font.Color := clWindowText;

    if FDisplayHotTrack = HotTrackMask then
    begin
      DrawText(Canvas.Handle, PChar(FDisplayPath), Length(FDisplayPath), Rect, Flags);
      HotTrackOffset := Canvas.TextWidth(FDisplayPath);
      HotTrackBottom := Rect.Bottom;
      Inc(Rect.Left, HotTrackOffset);
      Canvas.Font.Color := HotTrackColor;
      DrawText(Canvas.Handle, PChar(VirtualMask), Length(VirtualMask), Rect, Flags);
    end
      else
    begin
      DrawText(Canvas.Handle, PChar(Str), Length(Str), Rect, Flags);

    end;

    Dec(Rect.Left, HotTrackOffset);
    Rect.Bottom := Max(Rect.Bottom, HotTrackBottom);
  end;
end;

procedure TCustomPathLabel.DoDrawText(var Rect: TRect; Flags: Longint);
begin
  DoDrawTextIntern(Rect, Flags, Caption);
end;

function TCustomPathLabel.GetSeparator: Char;
begin
  if FUnixPath then Result := '/'
    else Result := '\';
end;

function TCustomPathLabel.HotTrackPath: string;
var
  P: TPoint;
  DelimPos: Integer;
  Len: Integer;
  Path: string;
begin
  Result := '';
  if UseHotTrack and FMouseInView and (FDisplayPath <> '') then
  begin
    P := ScreenToClient(Mouse.CursorPos);
    Len := P.X - FIndentHorizontal;
    if Len >= 0 then
    begin
      if Len < Canvas.TextWidth(FDisplayPath) then
      begin
        Result := '';
        Path := FDisplayPath;
        repeat
          Assert(Path <> '');

          if (not FUnixPath) and (Result = '') and IsUncPath(Path) then
          begin
            Result := ExtractFileDrive(Path);
            if Copy(Path, Length(Result) + 1, 1) = GetSeparator then
            begin
              Result := Result + GetSeparator;
            end;
            Delete(Path, 1, Length(Result));
          end
            else
          begin
            DelimPos := Pos(GetSeparator, Path);
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
          end;

        until (Canvas.TextWidth(Result) >= Len) or (Path = '');
      end
        else
      if Len < Canvas.TextWidth(FDisplayPath + FDisplayMask) then
      begin
        Result := HotTrackMask;
      end;
    end;
  end;
end;

function TCustomPathLabel.GetColors(Index: Integer): TColor;
begin
  Assert(Index in [0..5]);
  Result := FColors[Index];
end; { GetColors }

function TCustomPathLabel.UseRightToLeftAlignment: Boolean;
begin
  // Not sure how to properly deal with RTL atm.
  // See also a comment on DrawTextBiDiModeFlags in DoDrawTextIntern
  Result := False;
end;

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
  S: string;
begin
  if not (csReading in ComponentState) and
     (AutoSize or AutoSizeVertical) then
  begin
    Rect := ClientRect;
    DC := GetDC(0);
    Canvas.Handle := DC;
    S := Caption;
    if S = '' then S := 'SomeTextForSizing';
    DoDrawTextIntern(Rect, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[WordWrap], S);
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

function TCustomPathLabel.TrackingActive: Boolean;
begin
  Result := Assigned(FocusControl) or Assigned(OnGetStatus);
end;

procedure TCustomPathLabel.UpdateStatus;
var
  NewIsActive: Boolean;
  NewIsEnabled: Boolean;
  NewColor: TColor;
begin
  if TrackingActive then
  begin
    NewIsActive := IsActive;
    NewIsEnabled :=
      Enabled and
      ((not Assigned(FocusControl)) or FocusControl.Enabled);
    NewColor := FColors[Integer(NewIsActive)];
    if (NewIsActive <> FIsActive) or
       (NewIsEnabled <> FIsEnabled) or
       (NewColor <> Color) then
    begin
      FIsActive := NewIsActive;
      FIsEnabled := NewIsEnabled;
      Color := NewColor;
      Invalidate;
    end;
  end
    else
  begin
    Color := clBtnFace;
  end;
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

function TCustomPathLabel.UseHotTrack: Boolean;
begin
  Result := HotTrack and FIsEnabled;
end;

procedure TCustomPathLabel.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited;
  if FMouseInView and UseHotTrack and (FDisplayHotTrack <> HotTrackPath) then
  begin
    Invalidate;
  end;
end;

procedure TCustomPathLabel.DoPathClick(Path: string);
begin
  if Assigned(OnPathClick) then
    OnPathClick(Self, Path);
end;

procedure TCustomPathLabel.DoMaskClick;
begin
  if Assigned(OnMaskClick) then
    OnMaskClick(Self);
end;

procedure TCustomPathLabel.DblClick;
begin
  if FIsEnabled then
  begin
    inherited;
  end;
end;

procedure TCustomPathLabel.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  if FIsEnabled then
  begin
    inherited;
  end
    else
  begin
    Handled := True;
  end;
end;

procedure TCustomPathLabel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseInView := True;
  Invalidate;
end;

procedure TCustomPathLabel.CMMouseLeave(var Message: TMessage);
begin
  FMouseInView := False;
  Invalidate;
  inherited;
end;

end.
