unit MoreButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TMBChangingEvent = procedure (Sender: TObject; var AllowChange: Boolean) of object;

type
  TMoreButton = class(TButton)
  private
    FExpanded: Boolean;
    FCaptions: array[Boolean] of string;
    FExpandedHeight: Integer;
    FOnChange: TNotifyEvent;
    FOnChanging: TMBChangingEvent;
    FPanel: TWinControl;
    FRepositionForm: Boolean;
    FWasEnabled: Boolean;

    function DefaultCaptions(Expanded: Boolean): string;
    procedure SetCaptions(Index: Integer; Value: string);
    procedure SetExpanded(Value: Boolean);
    procedure SetExpandedHeight(Value: Integer);
    procedure SetPanel(Value: TWinControl);
    function StoreAnchors: Boolean;
    function StoreCaptions(Index: Integer): Boolean;
    function StoreExpanded: Boolean;
    function StoreExpandedHeight: Boolean;
  protected
    function CanExpand: Boolean; virtual;
    procedure DoChange; virtual;

    property Caption;
    property ModalResult;
    property Cancel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
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
      if Value then Panel.Enabled := FWasEnabled
        else
      begin
        FWasEnabled := Panel.Enabled;
        Panel.Enabled := False;
      end;
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

end.
