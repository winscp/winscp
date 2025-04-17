unit UnixDirViewColProperties;

interface

uses
  Classes, ComCtrls, DirViewColProperties;

resourcestring
  SUnixDirViewRightsCol = 'Rights';
  SUnixDirViewOwnerCol = 'Owner';
  SUnixDirViewGroupCol = 'Group';
  SUnixDirViewLinkTargetCol = 'Link target';
  SUnixDirViewTypeCol = 'Type';

const
  UnixDirViewColumns = 9;
  DefaultUnixDirViewCaptions: array[0..UnixDirViewColumns-1] of Pointer =
    (@SDirViewNameCol, @SDirViewSizeCol, @SDirViewChangedCol2,
     @SUnixDirViewRightsCol, @SUnixDirViewOwnerCol, @SUnixDirViewGroupCol,
     @SDirViewExtCol, @SUnixDirViewLinkTargetCol, @SUnixDirViewTypeCol);
  DefaultUnixDirViewWidths: array[0..UnixDirViewColumns-1] of Integer =
    (150, 80, 130, 100, 130, 130, 0, 150, 125);
  DefaultUnixDirViewAlignments: array[0..UnixDirViewColumns-1] of TAlignment =
    (taLeftJustify, taRightJustify, taLeftJustify, taLeftJustify,
     taLeftJustify, taLeftJustify, taLeftJustify, taLeftJustify, taLeftJustify);
  DefaultUnixDirViewVisible: array[0..UnixDirViewColumns-1] of Boolean =
    (True, True, True, True, True, True, False, False, False);

type
  TUnixDirViewCol = (uvName, uvSize, uvChanged, uvRights, uvOwner, uvGroup, uvExt,
    uvLinkTarget, uvType);

  TUnixDirViewColProperties = class(TCustomDirViewColProperties)
  private
    function StoreAlignment(Index: Integer): Boolean;
    function StoreCaption(Index: Integer): Boolean;
    function StoreWidth(Index: Integer): Boolean;
    function GetSortDirColumn: TUnixDirViewCol;
    procedure SetSortDirColumn(Value: TUnixDirViewCol);
  protected
  public
    constructor Create(DirView: TCustomListView);
  published
    property SortAscending;

    property SortDirColumn: TUnixDirViewCol read GetSortDirColumn write SetSortDirColumn default uvName;

    property NameCaption: string index uvName read GetCaptions write SetCaptions stored StoreCaption;
    property NameWidth: Integer index uvName read GetWidths write SetWidths stored StoreWidth;
    property NameVisible: Boolean index uvName read GetVisible write SetVisible default True;
    property NameAlignment: TAlignment index uvName read GetAlignments write SetAlignments stored StoreAlignment;

    property SizeCaption: string index uvSize read GetCaptions write SetCaptions stored StoreCaption;
    property SizeWidth: Integer index uvSize read GetWidths write SetWidths stored StoreWidth;
    property SizeVisible: Boolean index uvSize read GetVisible write SetVisible default True;
    property SizeAlignment: TAlignment index uvSize read GetAlignments write SetAlignments stored StoreAlignment;

    property ChangedCaption: string index uvChanged read GetCaptions write SetCaptions stored StoreCaption;
    property ChangedWidth: Integer index uvChanged read GetWidths write SetWidths stored StoreWidth;
    property ChangedVisible: Boolean index uvChanged read GetVisible write SetVisible default True;
    property ChangedAlignment: TAlignment index uvChanged read GetAlignments write SetAlignments stored StoreAlignment;

    property RightsCaption: string index uvRights read GetCaptions write SetCaptions stored StoreCaption;
    property RightsWidth: Integer index uvRights read GetWidths write SetWidths stored StoreWidth;
    property RightsVisible: Boolean index uvRights read GetVisible write SetVisible default True;
    property RightsAlignment: TAlignment index uvRights read GetAlignments write SetAlignments stored StoreAlignment;

    property OwnerCaption: string index uvOwner read GetCaptions write SetCaptions stored StoreCaption;
    property OwnerWidth: Integer index uvOwner read GetWidths write SetWidths stored StoreWidth;
    property OwnerVisible: Boolean index uvOwner read GetVisible write SetVisible default True;
    property OwnerAlignment: TAlignment index uvOwner read GetAlignments write SetAlignments stored StoreAlignment;

    property GroupCaption: string index uvGroup read GetCaptions write SetCaptions stored StoreCaption;
    property GroupWidth: Integer index uvGroup read GetWidths write SetWidths stored StoreWidth;
    property GroupVisible: Boolean index uvGroup read GetVisible write SetVisible default True;
    property GroupAlignment: TAlignment index uvGroup read GetAlignments write SetAlignments stored StoreAlignment;

    property ExtCaption: string index uvExt read GetCaptions write SetCaptions stored StoreCaption;
    property ExtWidth: Integer index uvExt read GetWidths write SetWidths stored StoreWidth;
    property ExtVisible: Boolean index uvExt read GetVisible write SetVisible default False;
    property ExtAlignment: TAlignment index uvExt read GetAlignments write SetAlignments stored StoreAlignment;

    property LinkTargetCaption: string index uvLinkTarget read GetCaptions write SetCaptions stored StoreCaption;
    property LinkTargetWidth: Integer index uvLinkTarget read GetWidths write SetWidths stored StoreWidth;
    property LinkTargetVisible: Boolean index uvLinkTarget read GetVisible write SetVisible default False;
    property LinkTargetAlignment: TAlignment index uvLinkTarget read GetAlignments write SetAlignments stored StoreAlignment;

    property TypeCaption: string index uvType read GetCaptions write SetCaptions stored StoreCaption;
    property TypeWidth: Integer index uvType read GetWidths write SetWidths stored StoreWidth;
    property TypeVisible: Boolean index uvType read GetVisible write SetVisible default True;
    property TypeAlignment: TAlignment index uvType read GetAlignments write SetAlignments stored StoreAlignment;
  end; { TUnixDirViewColProperties }


implementation

uses
  SysUtils;

  { TUnixDirViewColProperties }

constructor TUnixDirViewColProperties.Create(DirView: TCustomListView);
var
  Index: Integer;
begin
  inherited Create(DirView, UnixDirViewColumns);

  for Index := 0 to Count-1 do
  begin
    Captions[Index] := LoadResString(DefaultUnixDirViewCaptions[Index]);
    Visible[Index] := DefaultUnixDirViewVisible[Index];
    Widths[Index] := DefaultUnixDirViewWidths[Index];
    Alignments[Index] := DefaultUnixDirViewAlignments[Index];
  end;
end;

procedure TUnixDirViewColProperties.SetSortDirColumn(Value: TUnixDirViewCol);
begin
  SortColumn := Integer(Value);
end;

function TUnixDirViewColProperties.GetSortDirColumn: TUnixDirViewCol;
begin
  Result := TUnixDirViewCol(SortColumn);
end;

function TUnixDirViewColProperties.StoreAlignment(Index: Integer): Boolean;
begin
  Result := (Alignments[Index] <> DefaultUnixDirViewAlignments[Index]);
end;

function TUnixDirViewColProperties.StoreCaption(Index: Integer): Boolean;
begin
  Result := (Captions[Index] <> LoadResString(DefaultUnixDirViewCaptions[Index]));
end;

function TUnixDirViewColProperties.StoreWidth(Index: Integer): Boolean;
begin
  Result := (Widths[Index] <> DefaultUnixDirViewWidths[Index]);
end;

end.
