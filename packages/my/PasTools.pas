unit PasTools;

interface

uses
  Classes;

function Construct(ComponentClass: TComponentClass; Owner: TComponent): TComponent;

implementation

function Construct(ComponentClass: TComponentClass; Owner: TComponent): TComponent;
begin
  Result := ComponentClass.Create(Owner);
end;

end.
