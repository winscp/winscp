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
    function SortAscendingByDefault(Index: Integer): Boolean; override;
  public
    { Public declarations }
    property Items;
  published
    { Published declarations }
    property UnixColProperties: TUnixDirViewColProperties read GetUnixColProperties write SetUnixColProperties;
  end;

resourcestring
  SUnixDefaultRootName = '/ <root>';

implementation

{ TCustomUnixDirView }

function TCustomUnixDirView.NewColProperties: TCustomListViewColProperties;
begin
  Result := TUnixDirViewColProperties.Create(Self);
end;

function TCustomUnixDirView.SortAscendingByDefault(Index: Integer): Boolean;
begin
  Result := not (TUnixDirViewCol(Index) in [uvSize, uvChanged]);
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
