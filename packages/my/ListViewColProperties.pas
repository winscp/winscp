unit ListViewColProperties;

interface

uses
  Classes, ComCtrls, Contnrs;

const
  DefaultListViewMaxWidth = 1000;
  DefaultListViewMinWidth = 20;

type
  TCustomListViewColProperty = class(TObject)
    Alignment: TAlignment;
    Caption: string;
    Width: Integer;
    Visible: Boolean;
    Order: Integer;

    constructor Create(AOrder: Integer);
  end;

type
  TCustomListViewColProperties = class(TPersistent)
  private
    FChanged: Boolean;
    FMaxWidth: Integer;
    FMinWidth: Integer;
    FOnChange: TNotifyEvent;
    FUpdating: Integer;
    FProperties: TObjectList;
    FCreated: Boolean;
    function GetColumns: TListColumns;
    function GetCount: Integer;
    function GetOrderStr: string;
    procedure CheckBounds(Index: Integer);
    procedure SetMaxWidth(Value: Integer);
    procedure SetMinWidth(Value: Integer);
    procedure SetWidthsStr(Value: string);
    function GetWidthsStr: string;
    procedure SetOrderStr(Value: string);
  protected
    FListView: TCustomListView;
    FListViewManaged: Boolean;
    function GetAlignments(Index: Integer): TAlignment;
    function GetParamsStr: string; virtual;
    function GetVisible(Index: Integer): Boolean;
    function GetWidths(Index: Integer): Integer;
    procedure SetAlignments(Index: Integer; Value: TAlignment);
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

    property Columns: TListColumns read GetColumns stored False;
    property Count: Integer read GetCount stored False;
  public
    constructor Create(ListView: TCustomListView; ColCount: Integer);
    destructor Destroy; override;
    procedure EndUpdate;
    procedure BeginUpdate;
    procedure ListViewWndCreated;
    procedure ListViewWndDestroying;
    procedure ListViewWndDestroyed;
    property Alignments[Index: Integer]: TAlignment read GetAlignments write SetAlignments;
    property Captions[Index: Integer]: string read GetCaptions write SetCaptions;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default DefaultListViewMaxWidth;
    property MinWidth: Integer read FMinWidth write SetMinWidth default DefaultListViewMinWidth;
    property Widths[Index: Integer]: Integer read GetWidths write SetWidths;
    property Visible[Index: Integer]: Boolean read GetVisible write SetVisible;
    procedure RecreateColumns;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property ParamsStr: string read GetParamsStr write SetParamsStr stored False;
  end; { TListViewColProperties }

type
  TListViewColProperties = class(TCustomListViewColProperties)
  published
    property MaxWidth;
    property MinWidth;
  end; { TListViewColProperties }

implementation

uses
  SysUtils, CommCtrl, Windows, PasTools, Controls;

const
  MaxOrderColumns = 30 - 1;

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
  inherited Create;

  FCreated := False;
  FUpdating := 0;
  FChanged := False;
  FListViewManaged := (ColCount = 0);
  FListView := ListView;

  FProperties := TObjectList.Create;
  if FListViewManaged then ACount := GetColumns.Count
    else ACount := ColCount;

  CreateProperties(ACount);

  if not Assigned(FListView) then
    raise Exception.Create('NIL ListView pointer.');

  MaxWidth := DefaultListViewMaxWidth;
  MinWidth := DefaultListViewMinWidth;
end;

destructor TCustomListViewColProperties.Destroy;
begin
  inherited;
  FProperties.Free;
end;

procedure TCustomListViewColProperties.SetWidthsStr(Value: string);
var
  ColStr: string;
  Index: Integer;
begin
  Index := 0;
  BeginUpdate;
  try
    while (Value <> '') and (Index < Count) do
    begin
      ColStr := CutToChar(Value, ';', True);
      Widths[Index] := StrToInt(CutToChar(ColStr, ',', True));
      Visible[Index] := Boolean(StrToInt(ColStr));
      Inc(Index);
    end;
  finally
    EndUpdate;
  end;
end;

function TCustomListViewColProperties.GetWidthsStr: string;
var
  Index: Integer;
begin
  Result := '';
  for Index := 0 to Count-1 do
    Result := Format('%s;%d,%d', [Result, Widths[Index], Integer(Visible[Index])]);
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
    until Phase;

    // this does not make sure hidden columns are first,
    // but it gets fixed on the next run
    for Index := 0 to Count - 1 do
    begin
      Properties := GetProperties(Index);
      if Properties.Order < 0 then
      begin
        Properties.Order := Order;
        Inc(Order);
      end;
    end;

    if ColumnsExists then
      UpdateListViewOrder;
  finally
    EndUpdate;
  end;
end;

procedure TCustomListViewColProperties.SetParamsStr(Value: string);
var
  W: string;
begin
  W := CutToChar(Value, '|', True);
  // set first orders, only than the widths/visibility,
  // as setting visibility can reorder hidden/visible columns
  // as needed, while setting order cannot ensure this
  SetOrderStr(Value);
  SetWidthsStr(W);
end;

procedure TCustomListViewColProperties.SetVisible(Index: Integer; Value: Boolean);
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
        SetRuntimeVisible(Index, True, True);
    end
      else
    begin
      // hide before reordering column
      Properties.Visible := False;

      if ColumnsExists then
        SetRuntimeVisible(Index, False, True);

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

procedure TCustomListViewColProperties.SetRuntimeVisible(
  Index: Integer; Value: Boolean; SaveWidth: Boolean);
begin
  with GetColumn(Index) do
  begin
    if Value then
    begin
      MaxWidth := FMaxWidth;
      MinWidth := FMinWidth;
      Width := GetProperties(Index).Width;
    end
      else
    begin
      if SaveWidth then
        GetProperties(Index).Width := Width;
      MaxWidth := 1;
      MinWidth := 0;
      Width := 0
    end;
  end;
end;

procedure TCustomListViewColProperties.SetWidths(Index: Integer; Value: Integer);
begin
  CheckBounds(Index);

  if Value < FMinWidth then Value := FMinWidth
    else
  if Value > FMaxWidth then Value := FMaxWidth;

  if Widths[Index] <> Value then
  begin
    GetProperties(Index).Width := Value;
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
  Result := Format('%s|%s', [GetWidthsStr, GetOrderStr]);
end;

function TCustomListViewColProperties.GetVisible(Index: Integer): Boolean;
begin
  CheckBounds(Index);
  Result := GetProperties(Index).Visible;
end;

function TCustomListViewColProperties.GetWidths(Index: Integer): Integer;
begin
  CheckBounds(Index);
  if ColumnsExists and Visible[Index] then Result := GetColumn(Index).Width
    else Result := GetProperties(Index).Width;
end;

procedure TCustomListViewColProperties.SetMaxWidth(Value: Integer);
var
  Index: Integer;
begin
  if FMaxWidth <> Value then
  begin
    FMaxWidth := Value;
    for Index := 0 to Count - 1 do
    begin
      if ColumnsExists and Visible[Index] then GetColumn(Index).MaxWidth := Value
        else
      if GetProperties(Index).Width > Value then GetProperties(Index).Width := Value;
    end;
  end;
end;

procedure TCustomListViewColProperties.SetMinWidth(Value: Integer);
var
  Index: Integer;
begin
  if FMinWidth <> Value then
  begin
    FMinWidth := Value;
    for Index := 0 to Count - 1 do
    begin
      if ColumnsExists and Visible[Index] then GetColumn(Index).MinWidth := Value
        else
      if GetProperties(Index).Width < Value then GetProperties(Index).Width := Value;
    end;
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
begin
  for Index := 0 to ACount - 1 do
    FProperties.Add(TCustomListViewColProperty.Create(Index));
end;

procedure TCustomListViewColProperties.ListViewWndCreated;
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

procedure TCustomListViewColProperties.UpdateListViewOrder;
var
  Index: Integer;
  Properties: TCustomListViewColProperty;
  Temp: array of Integer;
begin
  SetLength(Temp, Count);
  // likely useless
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
      Properties.Width := Column.Width;
  end;

  UpdateOrderFromListView;
end;

end.
