unit ListViewColProperties;

interface

uses
  Classes, ComCtrls;

const
  DefaultListViewMaxWidth = 1000;
  DefaultListViewMinWidth = 20;

type
  TCustomListViewColProperties = class(TPersistent)
  private
    FChanged: Boolean;
    FMaxWidth: Integer;
    FMinWidth: Integer;
    FOnChange: TNotifyEvent;
    FUpdating: Integer;
    FWidths: array of Integer;
    FVisible: array of Boolean;
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
    function GetOrder(Index: Integer): Integer;
    function GetParamsStr: string; virtual;
    function GetVisible(Index: Integer): Boolean;
    function GetWidths(Index: Integer): Integer;
    procedure SetAlignments(Index: Integer; Value: TAlignment);
    procedure SetVisible(Index: Integer; Value: Boolean);
    procedure SetWidths(Index: Integer; Value: Integer);
    procedure SetOrder(Index: Integer; Value: Integer);
    function GetCaptions(Index: Integer): string;
    procedure Changed; virtual;
    procedure SetCaptions(Index: Integer; Value: string); virtual;
    procedure SetParamsStr(Value: string); virtual;
    procedure UpdateFromListView;

    property Columns: TListColumns read GetColumns stored False;
    property Count: Integer read GetCount stored False;
  public
    constructor Create(ListView: TCustomListView; ColCount: Integer);
    procedure EndUpdate;
    procedure BeginUpdate;
    procedure ListViewWndCreated;
    property Alignments[Index: Integer]: TAlignment read GetAlignments write SetAlignments;
    property Captions[Index: Integer]: string read GetCaptions write SetCaptions;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default DefaultListViewMaxWidth;
    property MinWidth: Integer read FMinWidth write SetMinWidth default DefaultListViewMinWidth;
    property Widths[Index: Integer]: Integer read GetWidths write SetWidths;
    property Visible[Index: Integer]: Boolean read GetVisible write SetVisible;
    procedure RecreateColumns;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Order[Index: Integer]: Integer read GetOrder write SetOrder;
    property OrderStr: string read GetOrderStr write SetOrderStr stored False;
    property ParamsStr: string read GetParamsStr write SetParamsStr stored False;
    property WidthsStr: string read GetWidthsStr write SetWidthsStr stored False;
  end; { TListViewColProperties }

type
  TListViewColProperties = class(TCustomListViewColProperties)
  published
    property MaxWidth;
    property MinWidth;
  end; { TListViewColProperties }

function CutToChar(var Str: string; Ch: Char; Trim: Boolean): string;

implementation

uses
  SysUtils, CommCtrl;

{ TODO : V ListView zamezit zmenu velikosti neviditelnych sloupecku }

function CutToChar(var Str: string; Ch: Char; Trim: Boolean): string;
var
  P: Integer;
begin
  P := Pos(Ch, Str);
  if P > 0 then
  begin
    Result := Copy(Str, 1, P-1);
    Delete(Str, 1, P);
  end
    else
  begin
    Result := Str;
    Str := '';
  end;
  if Trim then Result := SysUtils.Trim(Result);
end;

  { TCustomListViewColProperties }

constructor TCustomListViewColProperties.Create(
  ListView: TCustomListView; ColCount: Integer);
var
  Index: Integer;
begin
  inherited Create;

  FUpdating := 0;
  FChanged := False;
  FListViewManaged := (ColCount = 0);

  SetLength(FWidths, ColCount);
  SetLength(FVisible, ColCount);
  for Index := 0 to ColCount-1 do
  begin
    FVisible[Index] := True;
    FWidths[Index] := 50;
  end;

  FListView := ListView;
  if not Assigned(FListView) then
    raise Exception.Create('NIL ListView pointer.');

  if ColCount > 0 then
    with Columns do
    begin
      BeginUpdate;
      try
        if Count > 0 then Clear;
        for Index := 0 to ColCount-1 do
          Add.Width := 50;
      finally
        EndUpdate;
      end;
    end;

  MaxWidth := DefaultListViewMaxWidth;
  MinWidth := DefaultListViewMinWidth;
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
    // call Changed() even when FChange is false;
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

procedure TCustomListViewColProperties.SetAlignments(Index: Integer; Value: TAlignment);
begin
  CheckBounds(Index);
  if Columns[Index].Alignment <> Value then
  begin
    Columns[Index].Alignment := Value;
    Changed;
  end;
end;

procedure TCustomListViewColProperties.SetCaptions(Index: Integer; Value: string);
begin
  CheckBounds(Index);
  if Columns[Index].Caption <> Value then
  begin
    Columns[Index].Caption := Value;
    Changed;
  end;
end;

function TCustomListViewColProperties.GetAlignments(Index: Integer): TAlignment;
begin
  CheckBounds(Index);
  Result := Columns[Index].Alignment;
end;

function TCustomListViewColProperties.GetCaptions(Index: Integer): string;
begin
  CheckBounds(Index);
  Result := Columns[Index].Caption;
end;

procedure TCustomListViewColProperties.SetOrder(Index: Integer; Value: Integer);
var
  Temp: array[0..19] of Integer;
  I, C: Integer;
begin
  CheckBounds(Index);
  if Order[Index] <> Value then
  begin
    C := Count;
    if C > High(Temp) + 1 then C := High(Temp) + 1;
    ListView_GetColumnOrderArray(FListView.Handle, C, @Temp);
    for I := 0 to C - 1 do
      if Temp[I] = Value then Temp[I] := Temp[Index];
    Temp[Index] := Value;
    ListView_SetColumnOrderArray(FListView.Handle, C, @Temp);
    Changed;
  end;
end;

procedure TCustomListViewColProperties.SetOrderStr(Value: string);
var
  Index: Integer;
begin
  Index := 0;
  BeginUpdate;
  try
    while (Value <> '') and (Index < Count) do
    begin
      Order[Index] := StrToInt(CutToChar(Value, ';', True));
      Inc(Index);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomListViewColProperties.SetParamsStr(Value: string);
begin
  WidthsStr := CutToChar(Value, '|', True);
  OrderStr := Value;
end;

procedure TCustomListViewColProperties.SetVisible(Index: Integer; Value: Boolean);
begin
  CheckBounds(Index);
  if Visible[Index] <> Value then
  begin
    FVisible[Index] := Value;
    with Columns[Index] do
      if Value then
      begin
        MaxWidth := FMaxWidth;
        MinWidth := FMinWidth;
        Width := FWidths[Index];
      end
        else
      begin
        FWidths[Index] := Width;
        MaxWidth := 1;
        MinWidth := 0;
        if FListView.HandleAllocated then Width := 0
          else Width := 10;
      end;
    Changed;
  end;
end;

procedure TCustomListViewColProperties.SetWidths(Index: Integer; Value: Integer);
begin
  CheckBounds(Index);
  if Widths[Index] <> Value then
  begin
    if Visible[Index] then Columns[Index].Width := Value
      else
    begin
      if Value < FMinWidth then Value := FMinWidth
        else
      if Value > FMaxWidth then Value := FMaxWidth;
      FWidths[Index] := Value;
    end;
    Changed;
  end;
end;

function TCustomListViewColProperties.GetColumns: TListColumns;
begin
  Result := TListView(FListView).Columns;
end;

function TCustomListViewColProperties.GetCount: Integer;
begin
  Result := Columns.Count;
end;

function TCustomListViewColProperties.GetOrder(Index: Integer): Integer;
var
  Temp: array[0..19] of Integer;
  C: Integer;
begin
  CheckBounds(Index);
  C := Count;
  if C > High(Temp) + 1 then C := High(Temp) + 1;
  ListView_GetColumnOrderArray(FListView.Handle, C, @Temp);
  Result := Temp[Index];
end;

function TCustomListViewColProperties.GetOrderStr: string;
var
  Index: Integer;
begin
  Result := '';
  for Index := 0 to Count-1 do
    Result := Format('%s;%d', [Result, Order[Index]]);
  Delete(Result, 1, 1);
end;

function TCustomListViewColProperties.GetParamsStr: string;
begin
  Result := Format('%s|%s', [WidthsStr, OrderStr]);
end;

function TCustomListViewColProperties.GetVisible(Index: Integer): Boolean;
begin
  CheckBounds(Index);
  Result := FVisible[Index];
end;

function TCustomListViewColProperties.GetWidths(Index: Integer): Integer;
begin
  CheckBounds(Index);
  if Visible[Index] then Result := Columns[Index].Width
    else Result := FWidths[Index];
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
      if Visible[Index] then Columns[Index].MaxWidth := Value
        else
      if FWidths[Index] > Value then FWidths[Index] := Value;
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
      if Visible[Index] then Columns[Index].MinWidth := Value
        else
      if FWidths[Index] < Value then FWidths[Index] := Value;
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

procedure TCustomListViewColProperties.ListViewWndCreated;
var
  Index: Integer;
begin
  if FListViewManaged then UpdateFromListView
    else
  for Index := 0 to Count-1 do
    if not Visible[Index] then
      Columns[Index].Width := 0;
end;

procedure TCustomListViewColProperties.UpdateFromListView;
var
  Index: Integer;
begin
  SetLength(FWidths, Count);
  SetLength(FVisible, Count);

  for Index := 0 to Count-1 do
  begin
    FVisible[Index] := True;
    FWidths[Index] := Columns[Index].Width;
  end;
end;

end.
