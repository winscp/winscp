unit ListViewColProperties;

interface

uses
  Classes, ComCtrls, Contnrs;

type
  TCustomListViewColProperty = class(TObject)
    Alignment: TAlignment;
    Caption: string;
    Width: Integer;
    MaxWidth: Integer;
    MinWidth: Integer;
    Visible: Boolean;
    Order: Integer;

    constructor Create(AOrder: Integer);
  end;

type
  TCustomListViewColProperties = class(TPersistent)
  private
    FChanged: Boolean;
    FOnChange: TNotifyEvent;
    FUpdating: Integer;
    FProperties: TObjectList;
    FCreated: Boolean;
    function GetColumns: TListColumns;
    function GetCount: Integer;
    function GetOrderStr: string;
    procedure CheckBounds(Index: Integer);
    procedure SetWidthsStr(Value: string; PixelsPerInch: Integer);
    function GetWidthsStr: string;
    procedure SetOrderStr(Value: string);
  protected
    FListView: TCustomListView;
    FListViewManaged: Boolean;
    FConstraintsInitialized: Boolean;
    function GetAlignments(Index: Integer): TAlignment;
    function GetParamsStr: string; virtual;
    function GetVisible(Index: Integer): Boolean;
    function GetWidths(Index: Integer): Integer;
    procedure SetAlignments(Index: Integer; Value: TAlignment);
    procedure SetVisibleInternal(Index: Integer; Value: Boolean; SaveWidth: Boolean);
    procedure SetVisible(Index: Integer; Value: Boolean);
    procedure SetWidths(Index: Integer; Value: Integer);
    function GetCaptions(Index: Integer): string;
    procedure Changed; virtual;
    procedure SetCaptions(Index: Integer; Value: string); virtual;
    procedure SetParamsStr(Value: string); virtual;
    procedure UpdateListView;
    procedure UpdateFromListView;
    procedure UpdateOrderFromListView;
    procedure UpdateListViewOrder;
    function GetProperties(Index: Integer): TCustomListViewColProperty;
    function GetIndexByOrder(Order: Integer): Integer;
    function ColumnsExists: Boolean;
    procedure SetRuntimeVisible(Index: Integer; Value: Boolean; SaveWidth: Boolean);
    function GetColumn(Index: Integer): TListColumn;
    procedure CreateProperties(ACount: Integer);
    function DefaultConstraint(Value: Integer; Visible: Boolean; Def: Integer): Integer;

    property Columns: TListColumns read GetColumns stored False;
  public
    constructor Create(ListView: TCustomListView; ColCount: Integer);
    destructor Destroy; override;
    procedure EndUpdate;
    procedure BeginUpdate;
    procedure ListViewWndCreated;
    procedure ListViewWndDestroying;
    procedure ListViewWndDestroyed;
    procedure ChangeScale(M, D: Integer);
    property Count: Integer read GetCount stored False;
    property Alignments[Index: Integer]: TAlignment read GetAlignments write SetAlignments;
    property Captions[Index: Integer]: string read GetCaptions write SetCaptions;
    property Widths[Index: Integer]: Integer read GetWidths write SetWidths;
    property Visible[Index: Integer]: Boolean read GetVisible write SetVisible;
    procedure RecreateColumns;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property ParamsStr: string read GetParamsStr write SetParamsStr stored False;
  end; { TCustomListViewColProperties }

type
  TListViewColProperties = class(TCustomListViewColProperties)
  published
  end; { TListViewColProperties }

implementation

uses
  SysUtils, CommCtrl, Windows, PasTools, Controls, Forms;

const
  DefaultListViewMaxWidth = 1000;
  DefaultListViewMinWidth = 20;

{ TODO : V ListView zamezit zmenu velikosti neviditelnych sloupecku }

constructor TCustomListViewColProperty.Create(AOrder: Integer);
begin
  Alignment := taLeftJustify;
  Caption := '';
  Width := 50;
  Visible := True;
  Order := AOrder;
end;

  { TCustomListViewColProperties }

constructor TCustomListViewColProperties.Create(
  ListView: TCustomListView; ColCount: Integer);
var
  ACount: Integer;
begin
  // This contructor (and constructors of descendants)
  // is only even called from implementations of
  // TCustomNortonLikeListView.NewColProperties
  inherited Create;

  FConstraintsInitialized := False;
  FCreated := False;
  FUpdating := 0;
  FChanged := False;
  // ColCount is not 0 for file panels (TDirView and TCustomUnixDirView).
  // It is 0 otherwise.
  FListViewManaged := (ColCount = 0);
  FListView := ListView;

  FProperties := TObjectList.Create;
  if FListViewManaged then ACount := GetColumns.Count
    else ACount := ColCount;

  CreateProperties(ACount);

  if not Assigned(FListView) then
    raise Exception.Create('NIL ListView pointer.');
end;

destructor TCustomListViewColProperties.Destroy;
begin
  inherited;
  FProperties.Free;
end;

procedure TCustomListViewColProperties.SetWidthsStr(Value: string; PixelsPerInch: Integer);
var
  ColStr: string;
  Index: Integer;
  NeedInvalidate, NewVisible: Boolean;
  NewWidth: Integer;
begin
  Index := 0;
  NeedInvalidate := False;
  BeginUpdate;
  try
    while (Value <> '') and (Index < Count) do
    begin
      ColStr := CutToChar(Value, ';', True);
      NewWidth := LoadDimension(StrToInt(CutToChar(ColStr, ',', True)), PixelsPerInch, FListView);
      Widths[Index] := NewWidth;
      NewVisible := Boolean(StrToInt(CutToChar(ColStr, ',', True)));
      if Visible[Index] <> NewVisible then
      begin
        // As we are within BeginUpdate guard, the width set just above is not propadated to WinAPI.
        // As reading width retrieves it from WinAPI (if the control is already allocated), we actually save wrong width.
        SetVisibleInternal(Index, NewVisible, False);
        NeedInvalidate := True;
      end;
      Inc(Index);
    end;
  finally
    EndUpdate;
  end;

  // When visibility changes (particularly while reseting layout) redraw is needed
  // (Invalidate is called in SetVisible too, but maybe it has no effect there, because it is within BeginUpdate/EndUpdate)
  if NeedInvalidate and FListView.HandleAllocated then
    FListView.Invalidate;

end;

function TCustomListViewColProperties.GetWidthsStr: string;
var
  Index: Integer;
begin
  Result := '';
  for Index := 0 to Count-1 do
  begin
    Result := Format('%s;%d,%d', [Result, SaveDimension(Widths[Index]), Integer(Visible[Index])]);
  end;
  Delete(Result, 1, 1);
end;

procedure TCustomListViewColProperties.BeginUpdate;
begin
  Columns.BeginUpdate;
  Inc(FUpdating);
end;

procedure TCustomListViewColProperties.EndUpdate;
begin
  Columns.EndUpdate;
  Dec(FUpdating);
  if FUpdating = 0 then
  begin
    // call Changed() even when FChange is false
    Changed;

    FChanged := False;
  end;
end;

procedure TCustomListViewColProperties.Changed;
begin
  if FUpdating > 0 then FChanged := True
    else
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomListViewColProperties.CheckBounds(Index: Integer);
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.Create('Index out of bounds.');
end;

function TCustomListViewColProperties.GetProperties(Index: Integer): TCustomListViewColProperty;
begin
  Result := TCustomListViewColProperty(FProperties.Items[Index]);
end;

function TCustomListViewColProperties.GetIndexByOrder(Order: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if GetProperties(I).Order = Order then
    begin
      Result := I;
      Exit;
    end;
  end;

  raise Exception.Create('Column order out of bounds');
end;

function TCustomListViewColProperties.ColumnsExists: Boolean;
begin
  Result := FListView.HandleAllocated;
  if Result and (not FCreated) and (not FListViewManaged) then
    UpdateListView;
end;

procedure TCustomListViewColProperties.SetAlignments(Index: Integer; Value: TAlignment);
begin
  CheckBounds(Index);
  if Alignments[Index] <> Value then
  begin
    GetProperties(Index).Alignment := Value;
    if ColumnsExists then GetColumn(Index).Alignment := Value;
    Changed;
  end;
end;

procedure TCustomListViewColProperties.SetCaptions(Index: Integer; Value: string);
begin
  CheckBounds(Index);
  if Captions[Index] <> Value then
  begin
    if ColumnsExists then GetColumn(Index).Caption := Value
      else GetProperties(Index).Caption := Value;
    Changed;
  end;
end;

function TCustomListViewColProperties.GetAlignments(Index: Integer): TAlignment;
begin
  CheckBounds(Index);
  if ColumnsExists then Result := GetColumn(Index).Alignment
    else Result := GetProperties(Index).Alignment;
end;

function TCustomListViewColProperties.GetCaptions(Index: Integer): string;
begin
  CheckBounds(Index);
  if ColumnsExists then Result := GetColumn(Index).Caption
    else Result := GetProperties(Index).Caption;
end;

procedure TCustomListViewColProperties.SetOrderStr(Value: string);
var
  Order, Index: Integer;
  Properties: TCustomListViewColProperty;
  STemp: string;
  Phase: Boolean;
begin
  BeginUpdate;
  try
    for Index := 0 to Count - 1 do
      GetProperties(Index).Order := -1;

    // First order invisible columns (not True), then visible (not not True)
    Phase := True;
    Order := 0;

    repeat
      Phase := not Phase;
      STemp := Value;
      while (STemp <> '') and (Order < Count) do
      begin
        Index := StrToInt(CutToChar(STemp, ';', True));
        Properties := GetProperties(Index);
        if (Properties.Visible = Phase) and
           (Properties.Order < 0) { robustness }  then
        begin
          Properties.Order := Order;
          Inc(Order);
        end;
      end;

      // add missing columns from the same visibility class
      for Index := 0 to Count - 1 do
      begin
        Properties := GetProperties(Index);
        if (Properties.Visible = Phase) and
           (Properties.Order < 0) then
        begin
          Properties.Order := Order;
          Inc(Order);
        end;
      end;
    until Phase;

    if ColumnsExists then
      UpdateListViewOrder;
  finally
    EndUpdate;
  end;
end;

procedure TCustomListViewColProperties.SetParamsStr(Value: string);
var
  S: string;
  WidthsStr: string;
  OrderStr: string;
  PixelsPerInch: Integer;
begin
  // TFileFindDialog uses / as separator of its settings
  S := CutToChar(Value, '|', True);
  WidthsStr := CutToChar(S, '@', True);
  PixelsPerInch := LoadPixelsPerInch(S, FListView);
  SetWidthsStr(WidthsStr, PixelsPerInch);
  // Have to set order after visibility, otherwise we lost ordering of columns that are invisible by default,
  // but visible by configuration (as they would get ordered to the front)
  OrderStr := CutToChar(Value, '|', True);
  SetOrderStr(OrderStr);
end;

procedure TCustomListViewColProperties.SetVisibleInternal(Index: Integer; Value: Boolean; SaveWidth: Boolean);
var
  I: Integer;
  Properties: TCustomListViewColProperty;
begin
  CheckBounds(Index);
  if Visible[Index] <> Value then
  begin
    Properties := GetProperties(Index);

    if ColumnsExists then
      UpdateOrderFromListView;

    if Value then
    begin
      // shown column is moved to the back
      for I := 0 to Count - 1 do
      begin
        if GetProperties(I).Order > Properties.Order then
          Dec(GetProperties(I).Order);
      end;
      Properties.Order := Count - 1;

      if ColumnsExists then
        UpdateListViewOrder;

      // show only after reordering column
      Properties.Visible := True;

      if ColumnsExists then
        SetRuntimeVisible(Index, True, SaveWidth);
    end
      else
    begin
      // hide before reordering column
      Properties.Visible := False;

      if ColumnsExists then
        SetRuntimeVisible(Index, False, SaveWidth);

      // hidden column is moved to the front,
      // unless column to the left is not hidden already
      // (or unless it is first already, in which case the
      // condition in the loop is never satisfied)
      if (Properties.Order > 0) and
         GetProperties(GetIndexByOrder(Properties.Order - 1)).Visible then
      begin
        for I := 0 to Count - 1 do
        begin
          if GetProperties(I).Order < Properties.Order then
            Inc(GetProperties(I).Order);
        end;
        Properties.Order := 0;
      end;

      if ColumnsExists then
        UpdateListViewOrder;
    end;

    Changed;

    // It does not refresh itself at last one on Win7 when DoubleBuffered
    if FListView.HandleAllocated then
      FListView.Invalidate;
  end;
end;

procedure TCustomListViewColProperties.SetVisible(Index: Integer; Value: Boolean);
begin
  SetVisibleInternal(Index, Value, True);
end;

procedure TCustomListViewColProperties.SetRuntimeVisible(
  Index: Integer; Value: Boolean; SaveWidth: Boolean);
var
  Properties: TCustomListViewColProperty;
begin
  // This is probably only ever called from file panels (DirViews)
  // as other uses ("sychronization checklist" and "file find")
  // have FListViewManaged = False and never change Visible property
  // (though user can hide some columns manually in configuration storage)
  with GetColumn(Index) do
  begin
    Properties := GetProperties(Index);
    if Value then
    begin
      MaxWidth := Properties.MaxWidth;
      MinWidth := Properties.MinWidth;
      Width := Properties.Width;
    end
      else
    begin
      if SaveWidth then
        Properties.Width := Width;
      MaxWidth := 1;
      MinWidth := 0;
      Width := 0
    end;
  end;
end;

procedure TCustomListViewColProperties.SetWidths(Index: Integer; Value: Integer);
var
  Properties: TCustomListViewColProperty;
begin
  CheckBounds(Index);

  Properties := GetProperties(Index);

  if (Properties.MinWidth > 0) and (Value < Properties.MinWidth) then
  begin
    Value := Properties.MinWidth;
  end
    else
  if (Properties.MaxWidth > 0) and (Value > Properties.MaxWidth) then
  begin
    Value := Properties.MaxWidth;
  end;

  if Widths[Index] <> Value then
  begin
    Properties.Width := Value;
    if ColumnsExists and Visible[Index] then GetColumn(Index).Width := Value;
    Changed;
  end;
end;

function TCustomListViewColProperties.GetColumns: TListColumns;
begin
  Result := TListView(FListView).Columns;
end;

function TCustomListViewColProperties.GetColumn(Index: Integer): TListColumn;
begin
  Result := Columns[Index];
end;

function TCustomListViewColProperties.GetCount: Integer;
begin
  Result := FProperties.Count;
end;

function TCustomListViewColProperties.GetOrderStr: string;
var
  Index: Integer;
begin
  Result := '';
  if ColumnsExists then
    UpdateOrderFromListView;
  for Index := 0 to Count - 1 do
    Result := Format('%s;%d', [Result, GetIndexByOrder(Index)]);
  Delete(Result, 1, 1);
end;

function TCustomListViewColProperties.GetParamsStr: string;
begin
  // WORKAROUND
  // Adding an additional semicolon after the list,
  // to ensure that old versions that did not expect the pixels-per-inch part,
  // stop at the semicolon, otherwise they try to parse the
  // "last-column-width|pixels-per-inch" as integer and throw.
  // For the other instance of this hack, see GetListViewStr.
  // The new pixels-per-inch part is inserted after the widths part
  // as parsing of this was always robust to stop at "count" elements,
  // what order part was not (due to its logic of skipping hidden columns)
  Result := Format('%s;@%s|%s', [GetWidthsStr, SavePixelsPerInch(FListView), GetOrderStr]);
end;

function TCustomListViewColProperties.GetVisible(Index: Integer): Boolean;
begin
  CheckBounds(Index);
  Result := GetProperties(Index).Visible;
end;

function TCustomListViewColProperties.GetWidths(Index: Integer): Integer;
begin
  CheckBounds(Index);
  if ColumnsExists and Visible[Index] then
  begin
    Result := GetColumn(Index).Width;
  end
    else
  begin
    Result := GetProperties(Index).Width;
  end;
end;

procedure TCustomListViewColProperties.RecreateColumns;
var
  Copy: TListColumns;
begin
  Copy := TListColumns.Create(nil);
  try
    Copy.Assign(Columns);
    Columns.Assign(Copy);
  finally
    Copy.Free;
  end;
end;

procedure TCustomListViewColProperties.CreateProperties(ACount: Integer);
var
  Index: Integer;
  Properties: TCustomListViewColProperty;
begin
  for Index := 0 to ACount - 1 do
  begin
    Properties := TCustomListViewColProperty.Create(Index);
    FProperties.Add(Properties);
  end;
end;

function TCustomListViewColProperties.DefaultConstraint(Value: Integer; Visible: Boolean; Def: Integer): Integer;
begin
  if (Value > 0) and Visible then Result := Value
    else Result := Def;
end;

procedure TCustomListViewColProperties.ListViewWndCreated;
var
  Index: Integer;
  Properties: TCustomListViewColProperty;
  Column: TListColumn;
begin
  if FListViewManaged then
  begin
    if (FProperties.Count = 0) and (Columns.Count > 0) then
      CreateProperties(Columns.Count);

    UpdateFromListView;
  end
    else
  begin
    UpdateListView;
  end;

  Assert(ColumnsExists);
  for Index := 0 to Count - 1 do
  begin
    Column := GetColumn(Index);
    Properties := GetProperties(Index);

    if not FConstraintsInitialized then
    begin
      Properties.MaxWidth := DefaultConstraint(Column.MaxWidth, Properties.Visible, DefaultListViewMaxWidth);
      Properties.MinWidth := DefaultConstraint(Column.MinWidth, Properties.Visible, DefaultListViewMinWidth);
    end;

    // To apply the default constraints to columns that do not have their own
    if Properties.Visible then
    begin
      Column.MaxWidth := Properties.MaxWidth;
      if Column.Width > Column.MaxWidth then Column.Width := Column.MaxWidth;
      Column.MinWidth := Properties.MinWidth;
      if Column.Width < Column.MinWidth then Column.Width := Column.MinWidth;
    end;
  end;

  FConstraintsInitialized := True;
end;

procedure TCustomListViewColProperties.ListViewWndDestroying;
begin
  UpdateFromListView;
end;

procedure TCustomListViewColProperties.ListViewWndDestroyed;
begin
  if not FListViewManaged then
    FCreated := False;
end;

procedure TCustomListViewColProperties.ChangeScale(M, D: Integer);
var
  Index: Integer;
  Properties: TCustomListViewColProperty;
  Column: TListColumn;
begin
  for Index := 0 to Count - 1 do
  begin
    Properties := GetProperties(Index);
    // This is not perfect, as the constraints apply on re-scaled width before they are re-scaled themselves
    if Properties.Visible and ColumnsExists then
    begin
      Column := GetColumn(Index);
      Column.MaxWidth := MulDiv(Column.MaxWidth, M, D);
      Column.MinWidth := MulDiv(Column.MinWidth, M, D);
    end
      else
    begin
      Properties.MaxWidth := MulDiv(Properties.MaxWidth, M, D);
      Properties.MinWidth := MulDiv(Properties.MinWidth, M, D);
    end;
  end;
end;

procedure TCustomListViewColProperties.UpdateListViewOrder;
var
  Index: Integer;
  Properties: TCustomListViewColProperty;
  Temp: array of Integer;
begin
  SetLength(Temp, Count);
  // Seemingly useless,
  // but probably only because we swallow HDN_ENDDRAG in TCustomIEListView.WMNotify,
  // what prevents VLC from actually reordering columns collection
  ListView_GetColumnOrderArray(FListView.Handle, Count, PInteger(Temp));
  for Index := 0 to Count - 1 do
  begin
    Properties := GetProperties(Index);
    Temp[Properties.Order] := Index;
  end;
  ListView_SetColumnOrderArray(FListView.Handle, Count, PInteger(Temp));
end;

procedure TCustomListViewColProperties.UpdateListView;
var
  Index: Integer;
  Column: TListColumn;
  Properties: TCustomListViewColProperty;
begin
  // Only called when FListViewManaged = False
  BeginUpdate;
  try
    for Index := 0 to Count-1 do
    begin
      if Index < Columns.Count then
        Column := GetColumn(Index)
      else
        Column := Columns.Add;

      Properties := GetProperties(Index);

      Column.Alignment := Properties.Alignment;
      Column.Caption := Properties.Caption;
      SetRuntimeVisible(Index, Properties.Visible, False);
    end;

    UpdateListViewOrder;

  finally
    FCreated := True;
    EndUpdate;
  end;
end;

procedure TCustomListViewColProperties.UpdateOrderFromListView;
var
  Index: Integer;
  Temp: array of Integer;
begin
  SetLength(Temp, Count);
  ListView_GetColumnOrderArray(FListView.Handle, Count, PInteger(Temp));
  for Index := 0 to Count - 1 do
  begin
    GetProperties(Temp[Index]).Order := Index;
  end;
end;

procedure TCustomListViewColProperties.UpdateFromListView;
var
  Index: Integer;
  Column: TListColumn;
  Properties: TCustomListViewColProperty;
begin
  Assert(FProperties.Count = Columns.Count);
  for Index := 0 to Count-1 do
  begin
    Column := GetColumn(Index);
    Properties := GetProperties(Index);

    Properties.Alignment := Column.Alignment;
    Properties.Caption := Column.Caption;
    if Properties.Visible then
    begin
      Properties.Width := Column.Width;
      if Column.MaxWidth > 0 then
        Properties.MaxWidth := Column.MaxWidth;
      if Column.MinWidth > 0 then
        Properties.MinWidth := Column.MinWidth;
    end;
  end;

  UpdateOrderFromListView;
end;

end.
