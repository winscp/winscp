unit PathLabel;

interface

{$WARN UNIT_PLATFORM OFF}

uses
  Messages, StdCtrls, Controls, Classes, Forms, Windows, Graphics;

type
  TCustomPathLabel = class;

  TPathLabelGetStatusEvent = procedure(Sender: TCustomPathLabel; var Active: Boolean) of object;

  TCustomPathLabel = class(TCustomLabel)
  private
    FColors: array[0..3] of TColor;
    FIndentHorizontal: Integer;
    FIndentVertical: Integer;
    FUnixPath: Boolean;
    FOnGetStatus: TPathLabelGetStatusEvent;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
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

  public
    constructor Create(AnOwner: TComponent); override;
    procedure UpdateStatus;

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
    property UnixPath;
    property IndentHorizontal;
    property IndentVertical;
    property InactiveColor;
    property InactiveTextColor;
    property OnGetStatus;

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
  FColors[0] := clInactiveCaption;
  FColors[1] := clActiveCaption;
  FColors[2] := clInactiveCaptionText;
  FColors[3] := clCaptionText;
end;

procedure TCustomPathLabel.CMHintShow(var Message: TMessage);
begin
  with TCMHintShow(Message).HintInfo^ do
  begin
    HintPos.X := ClientOrigin.X + IndentHorizontal - 3;
    HintPos.Y := ClientOrigin.Y + IndentVertical - 3;
  end;
end; { CMHintShow }

procedure TCustomPathLabel.Click;
begin
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
  Text: string;
begin
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
  if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
  Flags := DrawTextBiDiModeFlags(Flags);
  Canvas.Font := Font;
  Text := Caption;
  if FUnixPath then
    for i := 1 to Length(Text) do
      if Text[i] = '/' then Text[i] := '\';
  Text := MinimizeName(Text, Canvas, Rect.Right - Rect.Left);
  if FUnixPath then
    for i := 1 to Length(Text) do
      if Text[i] = '\' then Text[i] := '/';
  ShowHint :=
    (Text <> Caption) or
    (Canvas.TextWidth(Caption) > Rect.Right - Rect.Left);
  if not ShowHint then Hint := ''
    else
  if Hint <> Caption then Hint := Caption;
  
  if not Enabled then
  begin
    OffsetRect(Rect, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
    OffsetRect(Rect, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  end
  else
    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
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
    {Rect.Left := Rect.Left + FIndentHorizontal;
    Rect.Right := Rect.Right - FIndentHorizontal;
    Rect.Top := Rect.Top + FIndentVertical;
    Rect.Bottom := Rect.Bottom - FIndentVertical;}
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
var
  Status: Boolean;
begin
  Status := IsActive;
  Color := FColors[Integer(Status)];
  // We don't want to stote Font properties in DFM
  // which would be if Font.Color is set to something else than clWindowText
  if not (csDesigning in ComponentState) then
    Font.Color := FColors[2 + Integer(Status)];
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

end.
