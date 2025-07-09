unit IEListView;
{==================================================================
 Component TCustomIEListView / Version 1.0, September 1999
 ==================================================================


    Description:
    ============
    Basic component for TDirView.

    Author:
    =======
    (c) Ingo Eckel 1999
    Sodener Weg 38
    65812 Bad Soden
    Germany
    (c) Martin Prikryl 2001 - 2003

    For detailed documentation see the documentation of TDirView.

 ==================================================================}

{Required compiler options for TCustomIEListView:}
{$A+,B-,X+,H+,P+}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, ActiveX, CommCtrl, Extctrls, ImgList,
  ComCtrls, NortonLikeListView, ListViewColProperties;

type
  TIEListViewColProperties = class(TCustomListViewColProperties)
  protected
    function GetSortAscending: Boolean;
    procedure SetSortColumn(Value: Integer);
    function GetSortColumn: Integer;
    function GetSortStr: string; virtual;
    procedure SetSortAscending(Value: Boolean);
    procedure SetSortStr(Value: string); virtual;
    function GetParamsStr: string; override;
    procedure SetParamsStr(Value: string); override;
  public
    constructor Create(ListView: TCustomListView; ColCount: Integer);
    property SortAscending: Boolean read GetSortAscending write SetSortAscending default True;
    property SortColumn: Integer read GetSortColumn write SetSortColumn;
    property SortStr: string read GetSortStr write SetSortStr stored False;
  end;

type
  TCustomIEListView = class;

  TListViewSecondaryColumnHeaderEvent =
    procedure(Sender: TCustomIEListView; Index: Integer; var SecondaryColumn: Integer) of object;

  TCustomIEListView = class(TCustomNortonLikeListView)
  private
    FSortColumn: Integer;
    FSortAscending: Boolean;
    FShowColumnIcon: Boolean;
    FParentForm: TCustomForm;

    FOnHeaderEndDrag: TNotifyEvent;
    FOnHeaderEndTrack: TNotifyEvent;
    FOnRecreate: TNotifyEvent;
    FOnSecondaryColumnHeader: TListViewSecondaryColumnHeaderEvent;

    FDragImageList: TDragImageList;

  protected
    procedure ColPropertiesChange(Sender: TObject); virtual;

    procedure SetShowColumnIcon(Value: Boolean); virtual;
    procedure SetSortColumn(Value: Integer); virtual;
    procedure SetSortAscending(Value: Boolean); virtual;
    procedure SortItems; virtual;
    procedure SetViewStyle(Value: TViewStyle); override; // CLEAN virtual
    procedure HeaderEndDrag(Sender: TObject); virtual;
    function SecondaryColumnHeader(Index: Integer): Integer;
    function NewColProperties: TCustomListViewColProperties; override;
    function SortAscendingByDefault(Index: Integer): Boolean; virtual;

    procedure CreateWnd; override;
    procedure ColClick(Column: TListColumn); override;
    procedure SetSort(Column: Integer; Ascending: Boolean);
    procedure WMNotify(var Msg: TWMNotify); message WM_NOTIFY;
    procedure CMRecreateWnd(var Message: TMessage); message CM_RECREATEWND;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetColumnImages; virtual;

    procedure SortBy(Column: Integer);

    property DragImageList: TDragImageList read FDragImageList;
    property ParentForm: TCustomForm read FParentForm;
    {Set the sort column of the listview}
    property SortColumn: Integer read FSortColumn write SetSortColumn;
    {Show the sorting symbol in the listview's header:}
    property ShowColumnIcon: Boolean
      read FShowColumnIcon write SetShowColumnIcon default True;
    {Sortorder of actual sort column}
    property SortAscending: Boolean
      read FSortAscending write SetSortAscending default True;
    property OnSecondaryColumnHeader: TListViewSecondaryColumnHeaderEvent
      read FOnSecondaryColumnHeader write FOnSecondaryColumnHeader;

  published
    property OnHeaderEndDrag: TNotifyEvent
      read  FOnHeaderEndDrag write FOnHeaderEndDrag;
    property OnHeaderEndTrack: TNotifyEvent
      read  FOnHeaderEndTrack write FOnHeaderEndTrack;
    property OnRecreate: TNotifyEvent
      read  FOnRecreate write FOnRecreate;

    property Align;
    property AllocBy;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Checkboxes;
    property Color;
    property ColumnClick default True;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property Enabled;
    property Font;
    property FlatScrollBars;
    property FullDrag;
    property GridLines;
    property HideSelection;
    property HotTrack;
    property HotTrackStyles;
    property IconOptions;
    property ReadOnly default False;
    property RowSelect;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnColumnRightClick;
    property OnCustomDraw;
    property OwnerDraw;
    {Used for internal purposes:}
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnDblClick;
    property OnDeletion;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEnter;
    property OnExit;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;

    property NortonLike;
  end; {Type TCustomIEListView}

type
  TIEListView = class(TCustomIEListView)
  published
    // copy from TListView, except for marked items
    property Action;
    property Align;
    property AllocBy;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Checkboxes;
    property Color;
    property Columns;
    property ColumnClick;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FlatScrollBars;
    property FullDrag;
    property GridLines;
    property HideSelection;
    property HotTrack;
    property HotTrackStyles;
    property HoverTime;
    property IconOptions;
    property Items;
    property LargeImages;
    property MultiSelect;
    property OwnerData;
    property OwnerDraw;
    property ReadOnly default False;
    property RowSelect;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowWorkAreas;
    property ShowHint;
    property SmallImages;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property ViewStyle;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnCompare;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDeletion;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSubItemImage;
    property OnDragDrop;
    property OnDragOver;
    property OnInfoTip;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
    property OnStartDrag;
    property OnSecondaryColumnHeader; // TCustomIEListView
  end;

var
  GlobalDragImageList: TDragImageList;

procedure Register;

implementation

uses
  PasTools, Types;

procedure Register;
begin
  RegisterComponents('Martin', [TIEListView]);
end;

  { TIEListViewColProperties }

constructor TIEListViewColProperties.Create(ListView: TCustomListView; ColCount: Integer);
begin
  inherited;
end;

procedure TIEListViewColProperties.SetParamsStr(Value: string);
begin
  SortStr := CutToChar(Value, '|', True);
  inherited SetParamsStr(Value);
end;

procedure TIEListViewColProperties.SetSortAscending(Value: Boolean);
begin
  TCustomIEListView(FListView).SortAscending := Value;
end;

procedure TIEListViewColProperties.SetSortColumn(Value: Integer);
begin
  if SortColumn <> Value then
  begin
    TCustomIEListView(FListView).SortColumn := Value;
    Changed;
  end;
end;

function TIEListViewColProperties.GetParamsStr: string;
begin
  Result := Format('%s|%s', [SortStr, inherited GetParamsStr]);
end;

function TIEListViewColProperties.GetSortAscending: Boolean;
begin
  Result := TCustomIEListView(FListView).SortAscending;
end;

function TIEListViewColProperties.GetSortColumn: Integer;
begin
  Result := TCustomIEListView(FListView).SortColumn;
end;

procedure TIEListViewColProperties.SetSortStr(Value: string);
var
  ASortColumn: Integer;
begin
  ASortColumn := StrToIntDef(CutToChar(Value, ';', True), SortColumn);
  if ASortColumn < Count then SortColumn := ASortColumn;
  SortAscending := Boolean(StrToIntDef(CutToChar(Value, ';', True), Integer(SortAscending)));
end;

function TIEListViewColProperties.GetSortStr: string;
begin
  Result := Format('%d;%d', [SortColumn, Integer(SortAscending)]);
end;

{ TCustomIEListView }

constructor TCustomIEListView.Create(AOwner: TComponent);
begin
  inherited;

  ColProperties.OnChange := ColPropertiesChange;
  FShowColumnIcon := True;
  FSortColumn := 0;
  FSortAscending := True;
end; {Create}

procedure TCustomIEListView.SetSort(Column: Integer; Ascending: Boolean);
begin
  if (SortColumn <> Column) or (SortAscending <> Ascending) then
  begin
    FSortColumn := Column;
    FSortAscending := Ascending;

    if Items.Count > 0 then SortItems;

    SetColumnImages;
  end;
end;

procedure TCustomIEListView.SortBy(Column: Integer);
begin
  if Column = SortColumn then SetSort(SortColumn, not SortAscending)
    else SetSort(Column, SortAscendingByDefault(Column));
end;

procedure TCustomIEListView.SetSortColumn(Value: Integer);
begin
  SetSort(Value, True);
end; {SetSortColumn}

procedure TCustomIEListView.SetViewStyle(Value: TViewStyle);
begin
  if Value <> ViewStyle then
  begin
    inherited SetViewStyle(Value);
    if ViewStyle = vsReport then
      SetColumnImages;
  end;
end; {SetViewStyle}

procedure TCustomIEListView.SetSortAscending(Value: Boolean);
begin
  SetSort(SortColumn, Value);
end; {SetSortAscending}

procedure TCustomIEListView.SetColumnImages;
var
  HdItem: THdItem;
  Index: Integer;
  SecondaryColumn: Integer;
  ShowImage: Boolean;
  NewFmt: Integer;
begin
  if ShowColumnHeaders and HandleAllocated then
  begin
    for Index := 0 to Columns.Count-1 do
    begin
      HdItem.Mask := HDI_FORMAT;
      Header_GetItem(FHeaderHandle, Index, HdItem);

      SecondaryColumn := SecondaryColumnHeader(Index);
      ShowImage := False;
      if FShowColumnIcon then
      begin
        if (Index = SortColumn) or
           ((SecondaryColumn >= 0) and (SecondaryColumn = SortColumn)) then
        begin
          ShowImage := True;
        end;
      end;

      // for hidden columns, do not show the icon
      // as on some systems it is still drawn, but on neighboring columns
      if ShowImage and (Columns[Index].Width > 0) then
      begin
        if SortAscending then
        begin
          NewFmt := (Hditem.fmt or HDF_SORTUP) and (not HDF_SORTDOWN);
        end
          else
        begin
          NewFmt := (Hditem.fmt or HDF_SORTDOWN) and (not HDF_SORTUP);
        end;
      end
        else
      begin
        NewFmt := HdItem.fmt and (not (HDF_SORTUP or HDF_SORTDOWN));
      end;

      if NewFmt <> HdItem.fmt then
      begin
        HdItem.Mask := HDI_FORMAT;
        HdItem.fmt := NewFmt;
        Header_SetItem(FHeaderHandle, Index, HDItem);
      end;
    end;
  end;
end; {SetColumnImage}

procedure TCustomIEListView.SetShowColumnIcon(Value: Boolean);
begin
  if Value <> ShowColumnIcon then
  begin
    FShowColumnIcon := Value;
    SetColumnImages;
  end;
end; {SetShowColumnIcon}

function TCustomIEListView.SecondaryColumnHeader(Index: Integer): Integer;
begin
  if Assigned(OnSecondaryColumnHeader) then
  begin
    OnSecondaryColumnHeader(Self, Index, Result);
  end
    else
  begin
    Result := -1;
  end;
end;

function TCustomIEListView.NewColProperties: TCustomListViewColProperties;
begin
  Result := TIEListViewColProperties.Create(Self, 0);
end;

function TCustomIEListView.SortAscendingByDefault(Index: Integer): Boolean;
begin
  Result := True;
end;

procedure TCustomIEListView.ColClick(Column: TListColumn);
begin
  SortBy(Column.Index);
  inherited;
end; {ColClick}

procedure TCustomIEListView.WMNotify(var Msg: TWMNotify);
begin
  if (FHeaderHandle <> 0) and (Msg.NMHdr^.hWndFrom = FHeaderHandle) then
    case Msg.NMHdr.code of
      HDN_BEGINDRAG:
        begin
          // We probably do not need to eat this message anymore,
          // we let's keep it in sync with HDN_ENDDRAG (see comment there)
        end;
      HDN_ENDDRAG:
        begin
          // Originally the code to eat this message was here to
          // workaround bug in D4 (until Update Pack 3).
          // But when we pass the message to VCL, it reorders
          // columns in Columns collection, what our code does not handle.
          // See also comment in TCustomListViewColProperties.UpdateListViewOrder
          HeaderEndDrag(Self);
          Invalidate;
          Exit;
        end;
       HDN_ENDTRACKA, HDN_ENDTRACKW:
         begin
           SetColumnImages;
           Invalidate;
           inherited;
           if Assigned(FOnHeaderEndTrack) then
             FOnHeaderEndTrack(Self);
           Exit;
         end;
       HDN_DIVIDERDBLCLICKA, HDN_DIVIDERDBLCLICKW:
         begin
           inherited;
           if Assigned(FOnHeaderEndTrack) then
             FOnHeaderEndTrack(Self);
           SetColumnImages;
         end;
    end;

  inherited;
end; { TCustomIEListView.WMNotify }

// Might not be always called, see comment in TDriveView.DestroyWnd
procedure TCustomIEListView.CMRecreateWnd(var Message: TMessage);
begin
  inherited;
  if Assigned(OnRecreate) then OnRecreate(Self);
end;

procedure TCustomIEListView.HeaderEndDrag(Sender : TObject);
begin
  if Assigned(FOnHeaderEndDrag) then
    FOnHeaderEndDrag(Self);
end; {HeaderEndDrag}

procedure TCustomIEListView.ColPropertiesChange(Sender: TObject);
begin
  SetColumnImages;
end;

procedure TCustomIEListView.CreateWnd;
begin
  inherited;

  FParentForm := GetParentForm(Self);
  if not (csDesigning in ComponentState) then
    FDragImageList := TDragImageList.Create(Self);
  if not Assigned(GlobalDragImageList) then
    GlobalDragImageList := DragImageList;
  SetColumnImages;
end; {CreateWnd}

destructor TCustomIEListView.Destroy;
begin
  if Assigned(FDragImageList) then
  begin
    if GlobalDragImageList = FDragImageList then
      GlobalDragImageList := nil;
    FDragImageList.Free;
  end;

  inherited;
end; {Destroy}

procedure TCustomIEListView.SortItems;
begin
  AlphaSort;
  ItemsReordered;
end;

end.
