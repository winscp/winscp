unit PasswordEdit;

interface

uses
  StdCtrls, Classes, Controls;

type
  TPasswordEdit = class(TCustomEdit)
  protected
    FPassword: Boolean;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetPassword(Value: Boolean);

  public
    constructor Create(AOwner: TComponent); override;

  published
    property Password: Boolean read FPassword write SetPassword default True;

    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    //property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses Windows, SysUtils;

procedure Register;
begin
  RegisterComponents('Martin', [TPasswordEdit]);
end;

constructor TPasswordEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPassword := True;
end;

procedure TPasswordEdit.SetPassword(Value: Boolean);
begin
  if Password <> Value then
  begin
    FPassword := Value;
    RecreateWnd;
  end;
end;

procedure TPasswordEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if FPassword then
  begin
    Params.Style := Params.Style or ES_PASSWORD;
  end;
end;

initialization
end.
