unit MoreButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TMBChangingEvent = procedure (Sender: TObject; var AllowChange: Boolean) of object;

type
  TMoreButton = class(TButton{Control})
  private
    //FDefault: Boolean;
    //FActive: Boolean;
    FExpanded: Boolean;
    FCaptions: array[Boolean] of string;
    FExpandedHeight: Integer;
    FOnChange: TNotifyEvent;
    FOnChanging: TMBChangingEvent;
    FPanel: TWinControl;
    FRepositionForm: Boolean;
    //FOwnerExpandedTop: Integer;

    //procedure SetDefault(Value: Boolean);
    //procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    //procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    //procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    //procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    function DefaultCaptions(Expanded: Boolean): string;
    procedure SetCaptions(Index: Integer; Value: string);
    procedure SetExpanded(Value: Boolean);
    procedure SetExpandedHeight(Value: Integer);
    procedure SetPanel(Value: TWinControl);
    function StoreAnchors: Boolean;
    function StoreCaptions(Index: Integer): Boolean;
    function StoreExpanded: Boolean;
    function StoreExpandedHeight: Boolean;
    //procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    function CanExpand: Boolean; virtual;
    //procedure CreateParams(var Params: TCreateParams); override;
    //procedure CreateWnd; override;
    procedure DoChange; virtual;
    //procedure SetButtonStyle(ADefault: Boolean); virtual;

    property Caption;
    property ModalResult;
    property Cancel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    //function UseRightToLeftAlignment: Boolean; override;
  published
    property CollapsedCaption: string index 0 read FCaptions[False] write SetCaptions stored StoreCaptions;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TMBChangingEvent read FOnChanging write FOnChanging;
    property Expanded: Boolean read FExpanded write SetExpanded stored StoreExpanded;
    property ExpandedCaption: string index 1 read FCaptions[True] write SetCaptions stored StoreCaptions;
    property ExpandedHeight: Integer read FExpandedHeight write SetExpandedHeight stored StoreExpandedHeight;
    property Panel: TWinControl read FPanel write SetPanel;
    property RepositionForm: Boolean read FRepositionForm write FRepositionForm;
    property Anchors stored StoreAnchors;

    //property Action;
    {property BiDiMode;
    property Constraints;
    property Default: Boolean read FDefault write SetDefault default False;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;}
    {property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;}
  end;

procedure Register;

resourcestring
  SDefaultExpandedCaption = '<< &Less';
  SDefaultCollapsedCaption = '&More >>';

const
  DefaultExpandedHeight = 50;
  DefaultExpanded = True;

implementation

procedure Register;
begin
  RegisterComponents('Martin', [TMoreButton]);
end;

  { TMoreButton }

constructor TMoreButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaptions[False] := DefaultCaptions(False);
  FCaptions[True] := DefaultCaptions(True);
  FExpandedHeight := DefaultExpandedHeight;
  FPanel := nil;
  FExpanded := DefaultExpanded;
  FOnChange := nil;
  FOnChanging := nil;
  FRepositionForm := False;

  {ControlStyle := [csSetCaption, csOpaque, csDoubleClicks];
  Width := 75;
  Height := 25;
  TabStop := True;}
  Caption := FCaptions[Expanded];
  Anchors := [akLeft, akBottom];
end;

function TMoreButton.CanExpand: Boolean;
begin
  Result := True;
  if Assigned(OnChanging) then OnChanging(Self, Result);
end; { CanExpand }

procedure TMoreButton.Click;
begin
  inherited;
  if CanExpand then
    Expanded := not Expanded;
end;

function TMoreButton.DefaultCaptions(Expanded: Boolean): string;
begin
  case Expanded of
    False: Result := SDefaultCollapsedCaption;
    True: Result := SDefaultExpandedCaption;
  end;
end;

procedure TMoreButton.DoChange;
begin
  if Assigned(OnChange) then OnChange(Self);
end; { DoChange }

procedure TMoreButton.SetCaptions(Index: Integer; Value: string);
begin
  Assert(Index in [0, 1]);
  if FCaptions[Boolean(Index)] <> Value then
  begin
    FCaptions[Boolean(Index)] := Value;
    if Expanded = Boolean(Index) then
      Caption := Value;
  end;
end;

procedure TMoreButton.SetExpanded(Value: Boolean);
var
  OwnerForm: TForm;
begin
  if Expanded <> Value then
  begin
    FExpanded := Value;
    Caption := FCaptions[Value];
    if Assigned(Owner) and (Owner is TForm) and Assigned(Panel) then
    begin
      OwnerForm := (Owner as TForm);
      if Value then
      begin
        OwnerForm.Height := OwnerForm.Height + ExpandedHeight;
        if RepositionForm then
          OwnerForm.Top := OwnerForm.Top - (ExpandedHeight div 2);
      end
        else
      begin
        FExpandedHeight := Panel.Height;
        OwnerForm.Height := OwnerForm.Height - ExpandedHeight;
        if RepositionForm then
          OwnerForm.Top := OwnerForm.Top + (ExpandedHeight div 2);
      end;
      Panel.Visible := Value;
      Panel.Enabled := Value;
    end;
    DoChange;
  end;
end;

procedure TMoreButton.SetExpandedHeight(Value: Integer);
begin
  if ExpandedHeight <> Value then
  begin
    if Expanded and Assigned(Owner) and (Owner is TControl) and Assigned(Panel) then
      (Owner as TControl).Height :=
        (Owner as TControl).Height + (Value - FExpandedHeight);
    FExpandedHeight := Value;
  end;
end;

procedure TMoreButton.SetPanel(Value: TWinControl);
begin
  if Panel <> Value then
  begin
    FPanel := Value;
    if Assigned(Panel) then
    begin
      FExpanded :=
        not ((FPanel.Height = 0) and (not FPanel.Enabled) and (not FPanel.Visible));
      Caption := FCaptions[FExpanded];
      if FExpanded then
        FExpandedHeight := FPanel.Height;
      //FPanel.Anchors := FPanel.Anchors + [akBottom];
    end;
  end;
end;

function TMoreButton.StoreAnchors: Boolean;
begin
  Result := (Anchors <> [akLeft, akBottom]);
end;

function TMoreButton.StoreCaptions(Index: Integer): Boolean;
begin
  Assert(Index in [0, 1]);
  Result := FCaptions[Boolean(Index)] <> DefaultCaptions(Boolean(Index));
end;

function TMoreButton.StoreExpanded: Boolean;
begin
  Result := (not Assigned(Panel)) and (Expanded <> DefaultExpanded);
end;

function TMoreButton.StoreExpandedHeight: Boolean;
begin
  Result :=
    ((not Expanded) or (not Assigned(Panel))) and
    (ExpandedHeight <> DefaultExpandedHeight);
end;

  { Following is tanken from TButton }

{function TMoreButton.UseRightToLeftAlignment: Boolean;
begin
  Result := False;
end;

procedure TMoreButton.SetButtonStyle(ADefault: Boolean);
const
  BS_MASK = $000F;
var
  Style: Word;
begin
  if HandleAllocated then
  begin
    if ADefault then Style := BS_DEFPUSHBUTTON else Style := BS_PUSHBUTTON;
    if GetWindowLong(Handle, GWL_STYLE) and BS_MASK <> Style then
      SendMessage(Handle, BM_SETSTYLE, Style, 1);
  end;
end;

procedure TMoreButton.SetDefault(Value: Boolean);
var
  Form: TCustomForm;
begin
  FDefault := Value;
  if HandleAllocated then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.Perform(CM_FOCUSCHANGED, 0, Longint(Form.ActiveControl));
  end;
end;

procedure TMoreButton.CreateParams(var Params: TCreateParams);
const
  ButtonStyles: array[Boolean] of DWORD = (BS_PUSHBUTTON, BS_DEFPUSHBUTTON);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'BUTTON');
  Params.Style := Params.Style or ButtonStyles[FDefault];
end;

procedure TMoreButton.CreateWnd;
begin
  inherited CreateWnd;
  FActive := FDefault;
end;

procedure TMoreButton.CNCommand(var Message: TWMCommand);
begin
  if Message.NotifyCode = BN_CLICKED then Click;
end;

procedure TMoreButton.CMDialogKey(var Message: TCMDialogKey);
begin
  with Message do
    if  ((CharCode = VK_RETURN) and FActive) and
      (KeyDataToShiftState(Message.KeyData) = []) and CanFocus then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TMoreButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TMoreButton.CMFocusChanged(var Message: TCMFocusChanged);
begin
  with Message do
    if Sender is TButton then
      FActive := Sender = Self
    else
      FActive := FDefault;
  SetButtonStyle(FActive);
  inherited;
end;

procedure TMoreButton.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  DefaultHandler(Message);
end;
}
end.
