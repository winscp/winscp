unit GrayedCheckBox;

interface

uses
  StdCtrls;

type
  TGrayedCheckBox = class(TCheckBox)
  protected
    procedure Toggle; override;
  end;

procedure Register;

implementation

uses
  Classes, Windows, SysUtils;

procedure Register;
begin
  RegisterComponents('Martin', [TGrayedCheckBox]);
end;

  { TGrayedCheckBox }

procedure TGrayedCheckBox.Toggle;
begin
  case State of
    cbUnchecked: State := cbChecked;
    cbChecked:
      if AllowGrayed then State := cbGrayed else State := cbUnchecked;
    cbGrayed: State := cbUnchecked;
  end;
end;

initialization
end.
