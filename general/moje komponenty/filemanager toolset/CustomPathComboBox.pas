unit CustomPathComboBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, IEComboBox, ComCtrls;

type
  TCustomPathComboBox = class(TIECustomComboBox)
  private
    FDirView: TCustomListView;
    FLastItem: Integer;
    FShowFullPath: Boolean;

  protected
    FPath: string;
    procedure DoCloseUp(Canceled: Boolean); override;
    procedure DropDown; override;
    procedure SetPath(Value: string); virtual; abstract;
    function GetPath: string; virtual;
    procedure Change; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PathChanged; virtual;
    procedure SetShowFullPath(Value: Boolean);
    function GetItemTextEx(Index: Integer; ForList: Boolean): string; override;

    property Style default csOwnerDrawFixed;
    property Items stored False;

    property ItemHeight stored False;
    property MaxLength stored False;
    property Sorted stored False;
    property Text stored False;

  public
    constructor Create(AOwner: TComponent); override;
    property DirView: TCustomListView read FDirView write FDirView;
    property Path: string read GetPath write SetPath;
    property ShowFullPath: Boolean read FShowFullPath write SetShowFullPath default False;
  published
  end;

implementation

uses
  CustomDirView;

constructor TCustomPathComboBox.Create(AOwner: TComponent);
begin
  inherited;
  Style := csOwnerDrawFixed;
  FDirView := nil;
  FShowFullPath := False;
end; { Create }

procedure TCustomPathComboBox.Change;
begin
  inherited;
  if (not DroppedDown) and (ItemIndex <> FLastItem) then PathChanged;
end; { Change }

procedure TCustomPathComboBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDirView) then
    FDirView := nil;
end; { Notification }

procedure TCustomPathComboBox.DoCloseUp(Canceled: Boolean);
begin
  if (FLastItem <> ItemIndex) then
  begin
    if Canceled then ItemIndex := FLastItem
      else PathChanged;
  end;

  inherited;
end; { DoCloseUp }

function TCustomPathComboBox.GetPath: string;
begin
  Result := FPath;
end;

procedure TCustomPathComboBox.PathChanged;
begin
  //  TUnixPathComboBox doesn't call this inherited method
  if Assigned(FDirView) and (TCustomDirView(FDirView).Path <> Path) then
    TCustomDirView(FDirView).Path := Path;
  FLastItem := ItemIndex;
end; { PathChanged }

procedure TCustomPathComboBox.DropDown;
begin
  FLastItem := ItemIndex;
  inherited;
end; { DropDown }

procedure TCustomPathComboBox.SetShowFullPath(Value: Boolean);
begin
  if ShowFullPath <> Value then
  begin
    FShowFullPath := Value;
    Invalidate;
  end;
end;

function TCustomPathComboBox.GetItemTextEx(Index: Integer; ForList: Boolean): string;
begin
  if ShowFullPath and not ForList then Result := Path
    else Result := inherited GetItemTextEx(Index, ForList);
end;

end.
