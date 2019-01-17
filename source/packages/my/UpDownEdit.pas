unit UpDownEdit;

interface

uses
  Windows, ComCtrls, Controls, ExtCtrls, Classes, Graphics, Messages, Forms,
  StdCtrls, Menus, SysUtils;

{ TUpDownEdit }

type
  TValueType = (vtInt, vtFloat, vtHex);

  TUpDownEditGetValue = procedure(Sender: TObject; Text: string;
    var Value: Extended; var Handled: Boolean) of object;
  TUpDownEditSetValue = procedure(Sender: TObject; Value: Extended;
    var Text: string; var Handled: Boolean) of object;

  TUpDownEdit = class(TCustomEdit)
  private
    FAlignment: TAlignment;
    FMinValue: Extended;
    FMaxValue: Extended;
    FIncrement: Extended;
    FDecimal: Byte;
    FChanging: Boolean;
    FEditorEnabled: Boolean;
    FValueType: TValueType;
    FArrowKeys: Boolean;
    FButtonsVisible: Boolean;
    FOnTopClick: TNotifyEvent;
    FOnBottomClick: TNotifyEvent;
    FUpDown: TCustomUpDown;
    FOnGetValue: TUpDownEditGetValue;
    FOnSetValue: TUpDownEditSetValue;
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
    function GetMinHeight: Integer;
    procedure GetTextHeight(var SysHeight, Height: Integer);
    function GetValue: Extended;
    function CheckValue(NewValue: Extended): Extended;
    function GetAsInteger: Longint;
    function IsIncrementStored: Boolean;
    function IsMaxStored: Boolean;
    function IsMinStored: Boolean;
    function IsValueStored: Boolean;
    procedure SetArrowKeys(Value: Boolean);
    procedure SetAsInteger(NewValue: Longint);
    procedure SetValue(NewValue: Extended);
    procedure SetValueType(NewType: TValueType);
    procedure SetDecimal(NewValue: Byte);
    function GetButtonWidth: Integer;
    procedure RecreateButton;
    procedure ResizeButton;
    procedure SetEditRect;
    procedure SetAlignment(Value: TAlignment);
    procedure SetButtonsVisible(Value: Boolean);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TMessage); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  protected
    procedure Change; override;
    function IsValidChar(Key: Char): Boolean; virtual;
    procedure UpClick(Sender: TObject); virtual;
    procedure DownClick(Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function DefBtnWidth: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AsInteger: Longint read GetAsInteger write SetAsInteger default 0;
    property Text;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    property ArrowKeys: Boolean read FArrowKeys write SetArrowKeys default True;
    property Decimal: Byte read FDecimal write SetDecimal default 2;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Increment: Extended read FIncrement write FIncrement stored IsIncrementStored;
    property MaxValue: Extended read FMaxValue write FMaxValue stored IsMaxStored;
    property MinValue: Extended read FMinValue write FMinValue stored IsMinStored;
    property ValueType: TValueType read FValueType write SetValueType default vtInt;
    property Value: Extended read GetValue write SetValue stored IsValueStored;
    property ButtonsVisible: Boolean read FButtonsVisible write SetButtonsVisible default True;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnBottomClick: TNotifyEvent read FOnBottomClick write FOnBottomClick;
    property OnTopClick: TNotifyEvent read FOnTopClick write FOnTopClick;
    property OnGetValue: TUpDownEditGetValue read FOnGetValue write FOnGetValue;
    property OnSetValue: TUpDownEditSetValue read FOnSetValue write FOnSetValue;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnEndDock;
    property OnStartDock;
  end;

procedure Register;

implementation

uses
  CommCtrl, PasTools, Math;

procedure Register;
begin
  RegisterComponents('Martin', [TUpDownEdit]);
end;

type
  TEmbededUpDown = class(TCustomUpDown)
  private
    FChanging: Boolean;
    procedure ScrollMessage(var Message: TWMVScroll);
    procedure WMHScroll(var Message: TWMHScroll); message CN_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message CN_VSCROLL;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnClick;
  end;

constructor TEmbededUpDown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Orientation := udVertical;
  Min := -1;
  Max := 1;
  Position := 0;
end;

destructor TEmbededUpDown.Destroy;
begin
  OnClick := nil;
  inherited Destroy;
end;

procedure TEmbededUpDown.ScrollMessage(var Message: TWMVScroll);
begin
  if Message.ScrollCode = SB_THUMBPOSITION then begin
    if not FChanging then begin
      FChanging := True;
      try
        if Message.Pos > 0 then Click(btNext)
        else if Message.Pos < 0 then Click(btPrev);
        if HandleAllocated then
          SendMessage(Handle, UDM_SETPOS, 0, 0);
      finally
        FChanging := False;
      end;
    end;
  end;
end;

procedure TEmbededUpDown.WMHScroll(var Message: TWMHScroll);
begin
  ScrollMessage(TWMVScroll(Message));
end;

procedure TEmbededUpDown.WMVScroll(var Message: TWMVScroll);
begin
  ScrollMessage(Message);
end;

procedure TEmbededUpDown.WMSize(var Message: TWMSize);
var
  Def: Integer;
begin
  inherited;
  Def := TUpDownEdit(Parent).DefBtnWidth;
  if Width <> Def then Width := Def;
end;

{ TUpDownEdit }

constructor TUpDownEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text := '0';
  ControlStyle := ControlStyle - [csSetCaption];
  FIncrement := 1.0;
  FDecimal := 2;
  FEditorEnabled := True;
  FArrowKeys := True;
  FButtonsVisible := True;
  RecreateButton;
end;

destructor TUpDownEdit.Destroy;
begin
  Destroying;
  FChanging := True;
  if FUpDown <> nil then
  begin
    FUpDown.Free;
    FUpDown := nil;
  end;
  inherited Destroy;
end;

procedure TUpDownEdit.RecreateButton;
begin
  if (csDestroying in ComponentState) then Exit;
  FUpDown.Free;
  FUpDown := nil;
  FUpDown := TEmbededUpDown.Create(Self);
  with TEmbededUpDown(FUpDown) do begin
    Visible := True;
    SetBounds(0, 0, DefBtnWidth, Self.Height);
    if (BiDiMode = bdRightToLeft) then Align := alLeft
      else Align := alRight;
    Parent := Self;
    OnClick := UpDownClick;
  end;
end;

procedure TUpDownEdit.SetArrowKeys(Value: Boolean);
begin
  FArrowKeys := Value;
  ResizeButton;
end;

procedure TUpDownEdit.UpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  if TabStop and CanFocus then SetFocus;
  case Button of
    btNext: UpClick(Sender);
    btPrev: DownClick(Sender);
  end;
end;

function TUpDownEdit.GetButtonWidth: Integer;
begin
  if FUpDown.Visible then Result := FUpDown.Width
    else Result := 0;
end;

function TUpDownEdit.DefBtnWidth: Integer;
begin
  Result := 15;
  if Parent <> nil then
  begin
    Result := ScaleByPixelsPerInch(Result, Self);
    Result := Math.Min(GetSystemMetricsForControl(Self, SM_CXVSCROLL), Result);
  end;
end;

procedure TUpDownEdit.ResizeButton;
begin
  if FUpDown <> nil then
  begin
    FUpDown.Width := DefBtnWidth;
    if (BiDiMode = bdRightToLeft) then FUpDown.Align := alLeft
      else FUpDown.Align := alRight;
    FUpDown.Visible := ButtonsVisible;
  end
end;

procedure TUpDownEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if ArrowKeys and (Key in [VK_UP, VK_DOWN]) then
  begin
    if Key = VK_UP then UpClick(Self)
      else
    if Key = VK_DOWN then DownClick(Self);
    Key := 0;
  end;
end;

procedure TUpDownEdit.Change;
begin
  if not FChanging then inherited Change;
end;

procedure TUpDownEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
    MessageBeep(MB_ICONHAND)
  end;
  if Key <> #0 then
  begin
    inherited KeyPress(Key);
    if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) then
    begin
      { must catch and remove this, since is actually multi-line }
      GetParentForm(Self).Perform(CM_DIALOGKEY, Byte(Key), 0);
      if Key = Char(VK_RETURN) then Key := #0;
    end;
  end;
end;

function TUpDownEdit.IsValidChar(Key: Char): Boolean;
var
  ValidChars: TSysCharSet;
begin
  ValidChars := ['+', '-', '0'..'9'];
  if ValueType = vtFloat then
  begin
    if Pos(FormatSettings.DecimalSeparator, Text) = 0 then
      ValidChars := ValidChars + [FormatSettings.DecimalSeparator];
    if Pos('E', AnsiUpperCase(Text)) = 0 then
      ValidChars := ValidChars + ['e', 'E'];
  end
    else
  if ValueType = vtHex then
  begin
    ValidChars := ValidChars + ['A'..'F', 'a'..'f'];
  end;
  Result := CharInSet(Key, ValidChars) or (Key < #32);
  if not FEditorEnabled and Result and ((Key >= #32) or
    (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then Result := False;
end;

procedure TUpDownEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[Boolean, TAlignment] of DWORD =
    ((ES_LEFT, ES_RIGHT, ES_CENTER), (ES_RIGHT, ES_LEFT, ES_CENTER));
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN or
    Alignments[UseRightToLeftAlignment, FAlignment];
end;

procedure TUpDownEdit.CreateWnd;
begin
  inherited CreateWnd;
  ResizeButton; // now we know the scaling factor
  SetEditRect;
  SetValue(Value);
end;

procedure TUpDownEdit.SetEditRect;
var
  Loc: TRect;
begin
  if (BiDiMode = bdRightToLeft) then
      SetRect(Loc, GetButtonWidth + 1, 0, ClientWidth - 1, ClientHeight + 1)
    else
      SetRect(Loc, 0, 0, ClientWidth - GetButtonWidth - 2, ClientHeight + 1);
  SendMessage(Handle, EM_SETRECTNP, 0, Longint(@Loc));
end;

procedure TUpDownEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TUpDownEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  MinHeight := GetMinHeight;
  { text edit bug: if size to less than minheight, then edit ctrl does
    not display the text }
  if Height < MinHeight then
    Height := MinHeight
  else begin
    ResizeButton;
    SetEditRect;
  end;
end;

procedure TUpDownEdit.GetTextHeight(var SysHeight, Height: Integer);
var
  DC: HDC;
  SaveFont: HFont;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  SysHeight := SysMetrics.tmHeight;
  Height := Metrics.tmHeight;
end;

function TUpDownEdit.GetMinHeight: Integer;
var
  I, H: Integer;
begin
  GetTextHeight(I, H);
  if I > H then I := H;
  Result := H + (GetSystemMetricsForControl(Self, SM_CYBORDER) * 4) + 1;
end;

procedure TUpDownEdit.UpClick(Sender: TObject);
var
  OldText: string;
begin
  if ReadOnly then MessageBeep(MB_ICONHAND)
  else begin
    FChanging := True;
    try
      OldText := inherited Text;
      Value := Value + FIncrement;
    finally
      FChanging := False;
    end;
    if CompareText(inherited Text, OldText) <> 0 then
    begin
      Modified := True;
      Change;
    end;
    if Assigned(FOnTopClick) then FOnTopClick(Self);
  end;
end;

procedure TUpDownEdit.DownClick(Sender: TObject);
var
  OldText: string;
begin
  if ReadOnly then MessageBeep(MB_ICONHAND)
  else begin
    FChanging := True;
    try
      OldText := inherited Text;
      Value := Value - FIncrement;
    finally
      FChanging := False;
    end;
    if CompareText(inherited Text, OldText) <> 0 then
    begin
      Modified := True;
      Change;
    end;
    if Assigned(FOnBottomClick) then FOnBottomClick(Self);
  end;
end;

procedure TUpDownEdit.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  ResizeButton;
  SetEditRect;
end;

procedure TUpDownEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResizeButton;
  SetEditRect;
end;

procedure TUpDownEdit.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  ResizeButton;
  SetEditRect;
end;

procedure TUpDownEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if FUpDown <> nil then
  begin
    FUpDown.Enabled := Enabled;
    ResizeButton;
  end;
end;

procedure TUpDownEdit.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TUpDownEdit.WMCut(var Message: TWMCut);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TUpDownEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  SetValue(Value);
end;

procedure TUpDownEdit.CMEnter(var Message: TMessage);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then SelectAll;
  inherited;
end;

function TUpDownEdit.GetValue: Extended;
var
  Handled: Boolean;
begin
  Handled := False;
  if Assigned(FOnGetValue) then FOnGetValue(Self, Text, Result, Handled);

  if not Handled then
  begin
    try
      if ValueType = vtFloat then Result := StrToFloat(Text)
      else if ValueType = vtHex then Result := StrToInt('$' + Text)
      else Result := StrToInt(Text);
    except
      if ValueType = vtFloat then Result := FMinValue
      else Result := Trunc(FMinValue);
    end;
  end;
end;

procedure TUpDownEdit.SetValue(NewValue: Extended);
var
  Handled: Boolean;
  AText: string;
begin
  NewValue := CheckValue(NewValue);

  Handled := False;
  if Assigned(FOnSetValue) then
  begin
    AText := Text;
    FOnSetValue(Self, NewValue, AText, Handled);
    if Handled then Text := AText;
  end;

  if not Handled then
  begin
    if ValueType = vtFloat then
      Text := FloatToStrF(NewValue, ffFixed, 15, FDecimal)
    else if ValueType = vtHex then
      Text := IntToHex(Round(NewValue), 1)
    else
      Text := IntToStr(Round(NewValue));
  end;
end;

function TUpDownEdit.GetAsInteger: Longint;
begin
  Result := Trunc(GetValue);
end;

procedure TUpDownEdit.SetAsInteger(NewValue: Longint);
begin
  SetValue(NewValue);
end;

procedure TUpDownEdit.SetValueType(NewType: TValueType);
begin
  if FValueType <> NewType then
  begin
    FValueType := NewType;
    Value := GetValue;
    if FValueType in [vtInt, vtHex] then
    begin
      FIncrement := Round(FIncrement);
      if FIncrement = 0 then FIncrement := 1;
    end;
  end;
end;

function TUpDownEdit.IsIncrementStored: Boolean;
begin
  Result := FIncrement <> 1.0;
end;

function TUpDownEdit.IsMaxStored: Boolean;
begin
  Result := (MaxValue <> 0.0);
end;

function TUpDownEdit.IsMinStored: Boolean;
begin
  Result := (MinValue <> 0.0);
end;

function TUpDownEdit.IsValueStored: Boolean;
begin
  Result := (GetValue <> MinValue);
end;

procedure TUpDownEdit.SetDecimal(NewValue: Byte);
begin
  if FDecimal <> NewValue then
  begin
    FDecimal := NewValue;
    Value := GetValue;
  end;
end;

function TUpDownEdit.CheckValue(NewValue: Extended): Extended;
begin
  Result := NewValue;
  if (FMaxValue <> FMinValue) then
  begin
    if NewValue < FMinValue then
      Result := FMinValue
    else if NewValue > FMaxValue then
      Result := FMaxValue;
  end;
end;

procedure TUpDownEdit.SetButtonsVisible(Value: Boolean);
begin
  if ButtonsVisible <> Value then
  begin
    FButtonsVisible := Value;
    ResizeButton;
    SetEditRect;
  end;
end;

initialization
end.
