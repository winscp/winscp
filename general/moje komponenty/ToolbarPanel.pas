unit ToolbarPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ActnList, Buttons, ImgList;

type
  TSpeedButtonDisplay = (bdImage, bdEllipse, bdCaption, bdShortCut);

const
  MinToolbarPanelWidth = 23;
  DefaultToolbarPanelHeight = 22;
  DefaultToolbarSpeedButtonMargin = 3;
  DefaultToolbarSpeedButtonSpacing = 1;

type
  TCustomToolbarPanel = class;

  TToolbarSpeedButton = class(TSpeedButton)
  private
    FActionCaption: string;
    FDisplay: TSpeedButtonDisplay;
    FShortCut: TShortCut;
    FPreserveEnabled: Boolean;
    function GetSimulateDisabled: Boolean;
    procedure SetActionCaption(Value: string);
    procedure SetDisplay(Value: TSpeedButtonDisplay);
    procedure SetShortCut(Value: TShortCut);
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function AssembleCaption(ADisplay: TSpeedButtonDisplay): string;
    procedure CopyImage(Index: Integer);
    procedure ChangeButton;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetEnabled: Boolean; override;
    procedure Paint; override;
    procedure SetEnabled(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    function CalcWidth(ADisplay: TSpeedbuttonDisplay): Integer;
    property ActionCaption: string read FActionCaption write SetActionCaption;
    property Display: TSpeedButtonDisplay read FDisplay write SetDisplay;
    property Margin default DefaultToolbarSpeedButtonMargin;
    property ShortCut: TShortCut read FShortCut write SetShortCut;
    property SimulateDisabled: Boolean read GetSimulateDisabled;
    property Spacing default DefaultToolbarSpeedButtonSpacing;
  end;

  TCustomToolbarPanel = class(TCustomPanel)
  private
    FActionList: TCustomActionList;
    FAlreadyCreated: Boolean;
    FCategory: string;
    FDisplay: TSpeedButtonDisplay;
    FFlat: Boolean;
    FProgressiveHidding: Boolean;
    FSameWidth: Boolean;
    FStretch: Boolean;
    FImages: array[0..1] of TCustomImageList;
    FUpdating: Integer;
    FWantResize: Boolean;
    procedure ConstraintsChange(Sender: TObject);
    function GetButtonCount: Integer;
    function GetButtons(Index: Integer): TToolbarSpeedButton;
    procedure SetActionList(Value: TCustomActionList);
    procedure SetCategory(Value: string);
    procedure SetConstraints(Value: TSizeConstraints);
    procedure SetDisplay(Value: TSpeedButtonDisplay);
    procedure SetFlat(Value: Boolean);
    procedure SetProgressiveHidding(Value: Boolean);
    procedure SetSameWidth(Value: Boolean);
    procedure SetStretch(Value: Boolean);
    function ConstraintsStored: Boolean;
    function GetConstraints: TSizeConstraints;
    procedure SetImages(Index: integer; Value: TCustomImageList);
  protected
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure ResizeButtons;
    procedure UpdateConstraints;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateButtons;
    property ActionList: TCustomActionList read FActionList write SetActionList;
    property Align default alBottom;
    property Category: string read FCategory write SetCategory;
    property Flat: Boolean read FFlat write SetFlat;
    property Height default DefaultToolbarPanelHeight;
    property BevelOuter default bvNone;
    property ButtonCount: Integer read GetButtonCount;
    property Buttons[Index: Integer]: TToolbarSpeedButton read GetButtons;
    property Constraints: TSizeConstraints read GetConstraints write SetConstraints stored ConstraintsStored;
    property DisabledImages: TCustomImageList index 1 read FImages[1] write SetImages;
    property Display: TSpeedButtonDisplay read FDisplay write SetDisplay
      default bdShortcut;
    property Images: TCustomImageList index 0 read FImages[0] write SetImages;
    property ProgressiveHidding: Boolean read FProgressiveHidding
      write SetProgressiveHidding default True;
    property SameWidth: Boolean read FSameWidth write SetSameWidth
      default False;
    property Stretch: Boolean read FStretch write SetStretch;
  end;

type
  TToolbarPanel = class(TCustomToolbarPanel)
  published
    property Category;
    property ActionList;
    property Display;
    property SameWidth;
    property Stretch;
    property ProgressiveHidding;
    property Images;
    property DisabledImages;

    property Align;
    property Anchors;
    property BevelOuter;
    property BevelInner;
    property BevelWidth;
    property Constraints;
    property Enabled;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property ShowHint;
    property Visible;
    {property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property FullRepaint;
    property Font;
    property Locked;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;}
  end;

procedure Register;

implementation

uses
  Menus(*, {C++B5: DsgnIntf} DesignEditors, DesignIntf*);

resourcestring
  SToolbarPanelEditor = 'Refresh';

{type
  TToolbarPanelEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
 }
type
  TSpeedButtonActionLink = class(TControlActionLink)
  protected
    function DoShowHint(var HintStr: string): Boolean; override;
    procedure SetCaption(const Value: string); override;
    procedure SetShortcut(Value: TShortcut); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetEnabled(Value: Boolean); override;
  end;

procedure Register;
begin
  RegisterComponents('Martin', [TToolbarPanel]);
//  RegisterComponentEditor(TToolbarPanel, TToolbarPanelEditor);
end;

  { TToolbarSpeedButton }

constructor TToolbarSpeedButton.Create(AOwner: TComponent);
begin
  inherited;

  Margin := DefaultToolbarSpeedButtonMargin;
  Spacing := DefaultToolbarSpeedButtonSpacing;
end; { Create }

procedure TToolbarSpeedButton.CopyImage(Index: Integer);
var
  ImageList: TCustomImageList;
begin
  if Index < 0 then Glyph := nil
    else
  if Action is TContainedAction then
  begin
    ImageList := nil;
    if Parent is TToolbarPanel then
    begin
      if not Enabled then ImageList := (Parent as TToolbarPanel).DisabledImages;
      if ImageList = nil then ImageList := (Parent as TToolbarPanel).Images;
    end;
    if ImageList = nil then
      ImageList := (Action as TContainedAction).ActionList.Images;

    if Assigned(ImageList) then
      with Glyph do
      begin
        Width := ImageList.Width;
        Height := ImageList.Height;
        Canvas.Brush.Color := clFuchsia;//! for lack of a better color
        Canvas.FillRect(Rect(0,0, Width, Height));
        ImageList.Draw(Canvas, 0, 0, Index);
      end;
  end;
end; { CopyImage }

procedure TToolbarSpeedButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.ActionCaption = '') then
        Self.ActionCaption := Caption;
      if not CheckDefaults or (Self.ShortCut = 0) then
        Self.ShortCut := ShortCut;
      if not CheckDefaults or (Self.Enabled = True) then
        Self.Enabled := Enabled;
      if not CheckDefaults or (Self.Hint = '') then
        Self.Hint := Hint;
      if not CheckDefaults or (Self.Visible = True) then
        Self.Visible := Visible;
      if not CheckDefaults or not Assigned(Self.OnClick) then
        Self.OnClick := OnExecute;

      { Copy image from action's imagelist }
      CopyImage(ImageIndex);
    end;
end; { ActionChange }

function TToolbarSpeedButton.AssembleCaption(ADisplay: TSpeedButtonDisplay): string;
var
  Temp: string;
begin
  Result := '';
  if ADisplay = bdShortCut then
    Result := ShortCutToText(ShortCut);

  if (ADisplay in [bdCaption, bdShortCut, bdEllipse]) then
  begin
    Temp := ActionCaption;
    Temp := Trim(StringReplace(StripHotKey(Temp),
      '...', '', [rfReplaceAll, rfIgnoreCase]));

    if ADisplay = bdEllipse then
      Temp := Temp[1] + '...';

    if Temp <> EmptyStr then
    begin
      if Result <> EmptyStr then Result := Result + ' ';
      Result := Result + Temp;
    end;
  end;
end; { AssembleCaption }

function TToolbarSpeedButton.CalcWidth(ADisplay: TSpeedbuttonDisplay): Integer;
var
  W: Integer;
begin
  Result := MinToolbarPanelWidth;
  if Action is TCustomAction then
  begin
    W := Canvas.TextWidth(AssembleCaption(ADisplay));
    if W > 0 then Inc(Result, Spacing + Margin);
    Inc(Result, W);
  end;
end; { CalcWidth }

function TToolbarSpeedButton.GetSimulateDisabled: Boolean;
begin
  Result := (not Enabled) and
   (Parent is TToolbarPanel) and
   Assigned((Parent as TToolbarPanel).DisabledImages);
end; { GetSimulateDisabled }

procedure TToolbarSpeedButton.ChangeButton;
begin
  Caption := AssembleCaption(Display);
  if Parent is TCustomToolbarPanel then
    (Parent as TCustomToolbarPanel).ResizeButtons
end; { ChangeButton }

procedure TToolbarSpeedButton.Paint;
begin
  if SimulateDisabled then FPreserveEnabled := True;
  try
    inherited Paint;
  finally
    FPreserveEnabled := False;
  end;
end; { Paint }

procedure TToolbarSpeedButton.SetActionCaption(Value: string);
begin
  if FActionCaption <> Value then
  begin
    FActionCaption := Value;
    ChangeButton;
  end;
end; { SetActionCaption }

procedure TToolbarSpeedButton.SetDisplay(Value: TSpeedButtonDisplay);
begin
  if FDisplay <> Value then
  begin
    FDisplay := Value;
    ChangeButton;
  end;
end; { SetDisplay }

function TToolbarSpeedButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TSpeedButtonActionLink;
end; { GetActionLinkClass }

procedure TToolbarSpeedButton.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  if SimulateDisabled then Font.Color := clBtnShadow
    else Font.Color := clWindowText;
end; { SetEnabled }

function TToolbarSpeedButton.GetEnabled;
begin
  Result := FPreserveEnabled or inherited GetEnabled;
end;

procedure TToolbarSpeedButton.SetShortCut(Value: TShortCut);
begin
  if FShortCut <> Value then
  begin
    FShortCut := Value;
    ChangeButton;
  end;
end; { SetShortCut }

{ TCustomToolbarPanel }

constructor TCustomToolbarPanel.Create(AOwner: TComponent);
begin
  inherited;

  FAlreadyCreated := False;
  FUpdating := 0;
  FWantResize := False;
  Align := alBottom;
  Height := DefaultToolbarPanelHeight;
  Flat := True;
  BevelOuter := bvNone;
  FDisplay := bdShortcut;
  FSameWidth := False;
  FProgressiveHidding := True;
end; { Create }

function TCustomToolbarPanel.ConstraintsStored: Boolean;
begin
  Result := (not ProgressiveHidding) or
    (Constraints.MinWidth <> (Width - ClientWidth) + (ButtonCount * MinToolbarPanelWidth)) or
    (Constraints.MaxWidth <> 0) or (Constraints.MinHeight <> 0) or
    (Constraints.MaxHeight <> 0);
end;

procedure TCustomToolbarPanel.CreateButtons;
var
  Button: TSpeedButton;
  Index: Integer;
  CurAction: TContainedAction;
begin
  if not (csLoading in ComponentState) then
  begin
    while ControlCount > 0 do
      Controls[0].Free;
    if Assigned(ActionList) then
    begin
      BeginUpdate;
      try
        for Index := ActionList.ActionCount-1 downto 0 do
        begin
          CurAction := ActionList.Actions[Index];
          if (Category = EmptyStr) or
             (Category = CurAction.Category) then
          begin
            Button := TToolbarSpeedButton.Create(Self);
            with Button do
            begin
              Parent := Self;
              Action := CurAction;
              Align := alLeft;
              Flat := FFlat;
              //ShowHint := True;
            end;
          end;
        end;
        UpdateConstraints;
        ResizeButtons;
      finally
        FAlreadyCreated := True;
        EndUpdate;
      end;
    end;
  end;
end; { CreateButtons }

procedure TCustomToolbarPanel.BeginUpdate;
begin
  Inc(FUpdating);
  Assert(FUpdating < 5);
end; { BeginUpdate }

procedure TCustomToolbarPanel.ConstraintsChange(Sender: TObject);
begin
  UpdateConstraints;
  AdjustSize;
end;

procedure TCustomToolbarPanel.EndUpdate;
begin
  Assert(FUpdating > 0);
  Dec(FUpdating);
  if (FUpdating = 0) and FWantResize then
  begin
    FWantResize := False;
    ResizeButtons;
  end;
end; { EndUpdate }

function TCustomToolbarPanel.GetButtonCount: Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to ControlCount-1 do
    if Controls[Index] is TToolbarSpeedButton then
      Inc(Result);
end; { GetButtonCount }

function TCustomToolbarPanel.GetButtons(Index: Integer): TToolbarSpeedButton;
var
  I: Integer;
begin
  I := -1;
  repeat
    Inc(I);
    if Controls[I] is TToolbarSpeedButton then
      Dec(Index);
  until Index < 0;
  Result := (Controls[I] as TToolbarSpeedButton);
end; { GetButtons }

procedure TCustomToolbarPanel.Notification(AComponent: TComponent; Operation: TOperation);
var
  NotMine: Boolean;
begin
  inherited;
  if Operation = opRemove then
  begin
    NotMine := False;
    if AComponent = ActionList then ActionList := nil
      else
    if AComponent = FImages[0] then FImages[0] := nil
      else
    if AComponent = FImages[1] then FImages[1] := nil
      else NotMine := True;
    if not NotMine then CreateButtons;
  end;
end; { Notification }

procedure TCustomToolbarPanel.Paint;
begin
  if not FAlreadyCreated then CreateButtons;
  inherited Paint;
end;

procedure TCustomToolbarPanel.Resize;
begin
  inherited;
  ResizeButtons;
end; { Resiye }

procedure TCustomToolbarPanel.ResizeButtons;
var
  Widths: array of Integer;
  Displays: array of TSpeedButtonDisplay;
  Count: Integer;
  Sum: Integer;
  ButtonToReduce: Integer;

  procedure CalcWidths;
  var
    Width: Integer;
    Index: Integer;
    MaxReduc: Integer;
    MaxDisplay: TSpeedButtonDisplay;
    MaxWidth: Integer;
  begin
    Sum := 0;
    ButtonToReduce := -1;
    if SameWidth then
    begin
      MaxWidth := 0;
      for Index := 0 to Count-1 do
      begin
        Width := Buttons[Index].CalcWidth(Displays[Index]);
        if Width > MaxWidth then
        begin
          MaxWidth := Width;
          ButtonToReduce := Index;
        end;
      end;
      for Index := 0 to Count-1 do
        Widths[Index] := MaxWidth;
      Sum := Count * MaxWidth;
    end
      else
    begin
      MaxDisplay := bdImage;
      MaxReduc := -1;
      for Index := 0 to Count-1 do
      begin
        Widths[Index] := Buttons[Index].CalcWidth(Displays[Index]);
        if (Displays[Index] > MaxDisplay) or
           ((Displays[Index] = MaxDisplay) and (Displays[Index] > bdImage) and
            (Widths[Index] - Buttons[Index].CalcWidth(Pred(Displays[Index])) > MaxReduc)) then
        begin
          MaxDisplay := Displays[Index];
          MaxReduc := Widths[Index] - Buttons[Index].CalcWidth(Pred(Displays[Index]));
          ButtonToReduce := Index;
        end;
        Inc(Sum, Widths[Index]);
      end;
    end;
  end;

var
  Width: Integer;
  Index: Integer;
  MaxWidth: Integer;
begin
  if FUpdating > 0 then FWantResize := True
    else
  begin
    Count := ButtonCount;
    if Count = 0 then Exit;
    BeginUpdate;
    try
      SetLength(Widths, Count);
      SetLength(Displays, Count);

      for Index := 0 to Count-1 do
        Displays[Index] := Display;

      repeat
        CalcWidths;
        if ProgressiveHidding and (ButtonToReduce >= 0) and (Sum > ClientWidth) then
          Displays[ButtonToReduce] := Pred(Displays[ButtonToReduce]);
      until (not ProgressiveHidding) or (ButtonToReduce = -1) or
        (Sum <= ClientWidth);

      if Stretch and (Sum < ClientWidth) then
      begin
        MaxWidth := ClientWidth;
        for Index := 0 to Count-1 do
        begin
          Width := Widths[Index];
          Widths[Index] := ((Width * MaxWidth) div Sum);
          Sum := Sum - Width;
          MaxWidth := MaxWidth - Widths[Index];
        end;
      end;

      for Index := 0 to Count-1 do
        with Buttons[Index] do
        begin
          Display := Displays[Index];
          Width := Widths[Index];
        end;
    finally
      FWantResize := False;
      EndUpdate;
    end;
  end;
end; { ResizeButtons }

procedure TCustomToolbarPanel.SetActionList(Value: TCustomActionList);
begin
  if ActionList <> Value then
  begin
    FActionList := Value;
    if Assigned(ActionList) then ActionList.FreeNotification(Self);
    CreateButtons;
  end;
end; { SetActionList }

procedure TCustomToolbarPanel.SetCategory(Value: string);
begin
  if FCategory <> Value then
  begin
    FCategory := Value;
    if Assigned(ActionList) then CreateButtons;
  end;
end; { SetCategory }

procedure TCustomToolbarPanel.SetConstraints(Value: TSizeConstraints);
begin
  inherited;
  Value.OnChange := ConstraintsChange;
  UpdateConstraints;
end; { SetConstraints }

procedure TCustomToolbarPanel.SetDisplay(Value: TSpeedButtonDisplay);
begin
  if FDisplay <> Value then
  begin
    FDisplay := Value;
    ResizeButtons;
  end;
end; { SetDisplay }

procedure TCustomToolbarPanel.SetFlat(Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    if Assigned(ActionList) then CreateButtons;
  end;
end; { SetFlat }

procedure TCustomToolbarPanel.SetImages(Index: integer; Value: TCustomImageList);
begin
  if FImages[Index] <> Value then
  begin
    FImages[Index] := Value;
    if Assigned(Value) then
      Value.FreeNotification(Self);
    CreateButtons;
  end;
end; { SetImages }

procedure TCustomToolbarPanel.SetProgressiveHidding(Value: Boolean);
begin
  if FProgressiveHidding <> Value then
  begin
    FProgressiveHidding := Value;
    if Assigned(ActionList) then ResizeButtons;
    UpdateConstraints;
  end;
end; { SetProgressiveHiding }

procedure TCustomToolbarPanel.SetSameWidth(Value: Boolean);
begin
  if FSameWidth <> Value then
  begin
    FSameWidth := Value;
    if Assigned(ActionList) then ResizeButtons;
  end;
end; { SetSameWidth }

procedure TCustomToolbarPanel.SetStretch(Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    if Assigned(ActionList) then ResizeButtons;
  end;
end; { SetStretch }

procedure TCustomToolbarPanel.UpdateConstraints;
var
  MinMinWidth: Integer;
begin
  if ProgressiveHidding then
  begin
    MinMinWidth := (Width - ClientWidth) + (ButtonCount * MinToolbarPanelWidth);
    if Constraints.MinWidth < MinMinWidth then
      Constraints.MinWidth := MinMinWidth
  end;
end; { UpdateConstraints }

function TCustomToolbarPanel.GetConstraints: TSizeConstraints;
begin
  Result := inherited Constraints;
end;

{ TToolbarPanelEditor }

{procedure TToolbarPanelEditor.ExecuteVerb(Index: Integer);
begin
  if Assigned(Component) then
    (Component as TCustomToolbarPanel).CreateButtons;
end;

function TToolbarPanelEditor.GetVerb(Index: Integer): string;
begin
  Result := SToolbarPanelEditor;
end;

function TToolbarPanelEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;
 }
{ TSpeedButtonActionLink }

function TSpeedButtonActionLink.DoShowHint(var HintStr: string): Boolean;
begin
  Result := inherited DoShowHint(HintStr);
end;

procedure TSpeedButtonActionLink.SetCaption(const Value: string);
begin
  TToolbarSpeedButton(FClient).ActionCaption := Value;
end;

procedure TSpeedButtonActionLink.SetEnabled(Value: Boolean);
begin
  inherited;
  TToolbarSpeedButton(FClient).CopyImage((Action as TCustomAction).ImageIndex);
end;

procedure TSpeedButtonActionLink.SetImageIndex(Value: Integer);
begin
  TToolbarSpeedButton(FClient).CopyImage(Value);
end;

procedure TSpeedButtonActionLink.SetShortcut(Value: TShortcut);
begin
  TToolbarSpeedButton(FClient).Shortcut := Value;
end;

end.
