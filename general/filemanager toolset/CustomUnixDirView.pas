unit CustomUnixDirView;

interface

{$WARN UNIT_PLATFORM OFF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, NortonLikeListView, IEListView, CustomDirView,
  ListViewColProperties, UnixDirViewColProperties;

type
  TCustomUnixDirView = class(TCustomDirView)
  private
    procedure SetUnixColProperties(Value: TUnixDirViewColProperties);
    function GetUnixColProperties: TUnixDirViewColProperties;
    { Private declarations }
  protected
    { Protected declarations }
    function NewColProperties: TCustomListViewColProperties; override;
  public
    { Public declarations }
    property Items;
  published
    { Published declarations }
    property UnixColProperties: TUnixDirViewColProperties read GetUnixColProperties write SetUnixColProperties;
  end;

implementation

{ TCustomUnixDirView }

function TCustomUnixDirView.NewColProperties: TCustomListViewColProperties;
begin
  Result := TUnixDirViewColProperties.Create(Self);
end;

procedure TCustomUnixDirView.SetUnixColProperties(Value: TUnixDirViewColProperties);
begin
  if Value <> ColProperties then
    ColProperties := Value;
end;

function TCustomUnixDirView.GetUnixColProperties: TUnixDirViewColProperties;
begin
  Result := TUnixDirViewColProperties(ColProperties);
end;

end.
