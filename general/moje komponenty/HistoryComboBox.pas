unit HistoryComboBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  THistorySaveOn = set of (soExit, soDropDown);

const
  DefaultHistorySaveOn = [soExit, soDropDown];
  DefaultMaxHistorySize = 30;

type
  THistoryComboBox = class(TComboBox)
  private
    { Private declarations }
    FSaveOn: THistorySaveOn;
    FMaxHistorySize: Integer;

    procedure SetMaxHistorySize(AMaxHistorySize: Integer);
    function StoreSaveOn: Boolean;
  protected
    { Protected declarations }
    procedure DoExit; override;
    procedure DropDown; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SaveToHistory; virtual;
  published
    { Published declarations }
    property SaveOn: THistorySaveOn read FSaveOn write FSaveOn stored StoreSaveOn;
    property MaxHistorySize: Integer read FMaxHistorySize write SetMaxHistorySize default DefaultMaxHistorySize;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Martin', [THistoryComboBox]);
end;

  { THistoryComboBox }

constructor THistoryComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FSaveOn := DefaultHistorySaveOn;
  FMaxHistorySize := DefaultMaxHistorySize;
end;

procedure THistoryComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if ((Key = VK_DOWN) or (Key = VK_UP)) and
     (not (ssAlt in Shift)) then
      if Items.IndexOf(Text) < 0 then SaveToHistory;
  inherited;
end;

procedure THistoryComboBox.SetMaxHistorySize(AMaxHistorySize: Integer);
begin
  FMaxHistorySize := AMaxHistorySize;
  while Items.Count > FMaxHistorySize do
    Items.Delete(Items.Count-1);
end;

procedure THistoryComboBox.DoExit;
begin
  inherited;
  if soExit in SaveOn then SaveToHistory;
end;

procedure THistoryComboBox.DropDown;
begin
  inherited;
  if soDropDown in SaveOn then SaveToHistory;
end;

procedure THistoryComboBox.SaveToHistory;
var
  T: AnsiString;
begin
  T := Text;
  if T <> '' then
  begin
    while Items.IndexOf(T) >= 0 do Items.Delete(Items.IndexOf(T));
    Items.Insert(0, T);
    ItemIndex := 0;
  end;
  while Items.Count > FMaxHistorySize do
    Items.Delete(Items.Count-1);
end;

function THistoryComboBox.StoreSaveOn: Boolean;
begin
  Result := (SaveOn <> DefaultHistorySaveOn);
end;

end.
