unit DirViewColProperties;

interface

uses
  Classes, ComCtrls, IEListView;

type
  TCustomDirViewColProperties = class(TIEListViewColProperties)
  protected
    function GetSortByExtension: Boolean;
    procedure SetSortByExtension(Value: Boolean);
    function GetSortStr: string; override;
    procedure SetSortStr(Value: string); override;
  public
    property SortByExtension: Boolean read GetSortByExtension write SetSortByExtension default False;
  end;

resourcestring
  SDirViewNameCol = 'Name';
  SDirViewSizeCol = 'Size';
  SDirViewTypeCol = 'Type';
  SDirViewChangedCol = 'Changed';
  SDirViewAttrCol = 'Attr';
  SDirViewExtCol = 'Ext';

const
  DirViewColumns = 6;
  DefaultDirViewCaptions: array[0..DirViewColumns-1] of Pointer =
    (@SDirViewNameCol, @SDirViewSizeCol, @SDirViewTypeCol, @SDirViewChangedCol,
     @SDirViewAttrCol, @SDirViewExtCol);
  DefaultDirViewWidths: array[0..DirViewColumns-1] of Integer =
    (150, 80, 125, 130, 45, 20);
  DefaultDirViewAlignments: array[0..DirViewColumns-1] of TAlignment =
    (taLeftJustify, taRightJustify, taLeftJustify, taLeftJustify,
     taLeftJustify, taLeftJustify);
  DefaultDirViewVisible: array[0..DirViewColumns-1] of Boolean =
    (True, True, True, True, True, False);

type
  TDirViewCol = (dvName, dvSize, dvType, dvChanged, dvAttr, dvExt);

  TDirViewColProperties = class(TCustomDirViewColProperties)
  private
    function StoreAlignment(Index: Integer): Boolean;
    function StoreCaption(Index: Integer): Boolean;
    function StoreWidth(Index: Integer): Boolean;
    function GetDirOrder(Index: Integer): TDirViewCol;
    function GetSortDirColumn: TDirViewCol;
    procedure SetDirOrder(Index: Integer; Value: TDirViewCol);
    procedure SetSortDirColumn(Value: TDirViewCol);
  public
    constructor Create(DirView: TCustomListView);
  published
    property MaxWidth;
    property MinWidth;
    property SortAscending;
    property SortByExtension;

    property SortDirColumn: TDirViewCol read GetSortDirColumn write SetSortDirColumn default dvName;

    property NameCaption: string index dvName read GetCaptions write SetCaptions stored StoreCaption;
    property NameWidth: Integer index dvName read GetWidths write SetWidths stored StoreWidth;
    property NameVisible: Boolean index dvName read GetVisible write SetVisible default True;
    property NameAlignment: TAlignment index dvName read GetAlignments write SetAlignments stored StoreAlignment;

    property SizeCaption: string index dvSize read GetCaptions write SetCaptions stored StoreCaption;
    property SizeWidth: Integer index dvSize read GetWidths write SetWidths stored StoreWidth;
    property SizeVisible: Boolean index dvSize read GetVisible write SetVisible default True;
    property SizeAlignment: TAlignment index dvSize read GetAlignments write SetAlignments stored StoreAlignment;

    property TypeCaption: string index dvType read GetCaptions write SetCaptions stored StoreCaption;
    property TypeWidth: Integer index dvType read GetWidths write SetWidths stored StoreWidth;
    property TypeVisible: Boolean index dvType read GetVisible write SetVisible default True;
    property TypeAlignment: TAlignment index dvType read GetAlignments write SetAlignments stored StoreAlignment;

    property ChangedCaption: string index dvChanged read GetCaptions write SetCaptions stored StoreCaption;
    property ChangedWidth: Integer index dvChanged read GetWidths write SetWidths stored StoreWidth;
    property ChangedVisible: Boolean index dvChanged read GetVisible write SetVisible default True;
    property ChangedAlignment: TAlignment index dvChanged read GetAlignments write SetAlignments stored StoreAlignment;

    property AttrCaption: string index dvAttr read GetCaptions write SetCaptions stored StoreCaption;
    property AttrWidth: Integer index dvAttr read GetWidths write SetWidths stored StoreWidth;
    property AttrVisible: Boolean index dvAttr read GetVisible write SetVisible default True;
    property AttrAlignment: TAlignment index dvAttr read GetAlignments write SetAlignments stored StoreAlignment;

    property ExtCaption: string index dvExt read GetCaptions write SetCaptions stored StoreCaption;
    property ExtWidth: Integer index dvExt read GetWidths write SetWidths stored StoreWidth;
    property ExtVisible: Boolean index dvExt read GetVisible write SetVisible default True;
    property ExtAlignment: TAlignment index dvExt read GetAlignments write SetAlignments stored StoreAlignment;

    property Column1: TDirViewCol index 0 read GetDirOrder write SetDirOrder default dvName;
    property Column2: TDirViewCol index 1 read GetDirOrder write SetDirOrder default dvSize;
    property Column3: TDirViewCol index 2 read GetDirOrder write SetDirOrder default dvType;
    property Column4: TDirViewCol index 3 read GetDirOrder write SetDirOrder default dvChanged;
    property Column5: TDirViewCol index 4 read GetDirOrder write SetDirOrder default dvAttr;
    property Column6: TDirViewCol index 5 read GetDirOrder write SetDirOrder default dvExt;
  end; { TDirViewColProperties }


implementation

uses
  SysUtils, CommCtrl, ListViewColProperties, CustomDirView;

  { TCustomDirViewColProperties }

procedure TCustomDirViewColProperties.SetSortByExtension(Value: Boolean);
begin
  TCustomDirView(FListView).SortByExtension := Value;
end;

function TCustomDirViewColProperties.GetSortByExtension: Boolean;
begin
  Result := TCustomDirView(FListView).SortByExtension;
end;

procedure TCustomDirViewColProperties.SetSortStr(Value: string);
begin
  inherited;
  CutToChar(Value, ';', True);
  CutToChar(Value, ';', True);
  SortByExtension := Boolean(StrToIntDef(CutToChar(Value, ';', True), Integer(SortByExtension)));
end;

function TCustomDirViewColProperties.GetSortStr: string;
begin
  Result := Format('%s;%d', [inherited GetSortStr, Integer(SortByExtension)]);
end;

{ TDirViewColProperties }

constructor TDirViewColProperties.Create(DirView: TCustomListView);
var
  Index: Integer;
begin
  inherited Create(DirView, DirViewColumns);

  for Index := 0 to Count-1 do
  begin
    Captions[Index] := LoadResString(DefaultDirViewCaptions[Index]);
    Visible[Index] := DefaultDirViewVisible[Index];
    Widths[Index] := DefaultDirViewWidths[Index];
    Alignments[Index] := DefaultDirViewAlignments[Index];
  end;
end;

function TDirViewColProperties.GetDirOrder(Index: Integer): TDirViewCol;
begin
  Result := TDirViewCol(GetOrder(Index));
end;

procedure TDirViewColProperties.SetDirOrder(Index: Integer;
  Value: TDirViewCol);
begin
  SetOrder(Index, Integer(Value));
end;

procedure TDirViewColProperties.SetSortDirColumn(Value: TDirViewCol);
begin
  SortColumn := Integer(Value);
end;

function TDirViewColProperties.GetSortDirColumn: TDirViewCol;
begin
  Result := TDirViewCol(SortColumn);
end;

function TDirViewColProperties.StoreAlignment(Index: Integer): Boolean;
begin
  Result := (Alignments[Index] <> DefaultDirViewAlignments[Index]);
end;

function TDirViewColProperties.StoreCaption(Index: Integer): Boolean;
begin
  Result := (Captions[Index] <> LoadResString(DefaultDirViewCaptions[Index]));
end;

function TDirViewColProperties.StoreWidth(Index: Integer): Boolean;
begin
  Result := (Widths[Index] <> DefaultDirViewWidths[Index]);
end;

end.
