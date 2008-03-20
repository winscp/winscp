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
  TDateTimeDisplay = (dtdDateTimeSec, dtdDateTime, dtdDate);

type
  TCustomIEListView = class;

  TListViewSecondaryColumnHeaderEvent =
    procedure(Sender: TCustomIEListView; Index: Integer; var SecondaryColumn: Integer) of object;

  TCustomIEListView = class(TCustomNortonLikeListView)
  private
    FSortColumn: Integer;
    FSortAscending: Boolean;
    FColumnIconPainted: Boolean;
    FShowColumnIcon: Boolean;
    FHeaderHandle: HWND;
    FParentForm: TCustomForm;
    FHeaderCanvas: TCanvas;

    FOnHeaderEndDrag: TNotifyEvent;
    FOnHeaderEndTrack: TNotifyEvent;
    FOnSecondaryColumnHeader: TListViewSecondaryColumnHeaderEvent;

    FDateTimeFormatStr: string;
    FDateFormatStr: string;
    FDateTimeDisplay: TDateTimeDisplay;
    FDragImageList: TDragImageList;
    FHeaderImages: TImageList;

    function SecondaryColumnHeaderOffset(Canvas: TCanvas; Index: Integer): Integer;
    function ColumnHeaderIconWidth: Integer;

  protected
    procedure ColPropertiesChange(Sender: TObject); virtual;

    procedure SetShowColumnIcon(Value: Boolean); virtual;
    procedure SetSortColumn(Value: Integer); virtual;
    procedure SetSortAscending(Value: Boolean); virtual;
    procedure SortItems; virtual;
    procedure SetViewStyle(Value: TViewStyle); override; // CLEAN virtual
    procedure SetDateTimeDisplay(Value: TDateTimeDisplay); virtual;
    procedure SetDateTimeFormatString; virtual;
    procedure HeaderEndDrag(Sender: TObject); virtual;
    procedure SetHeaderImages(Value: TImageList); virtual;
    function SecondaryColumnHeader(Index: Integer; var AliasOnly: Boolean): Integer; virtual;
    function NewColProperties: TCustomListViewColProperties; override;
    function SortAscendingByDefault(Index: Integer): Boolean; virtual;

    procedure CreateWnd; override;
    procedure ColClick(Column: TListColumn); override;
    procedure Loaded; override;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMNotify(var Msg: TWMNotify); message WM_NOTIFY;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetColumnImages; virtual;

    property HeaderImages: TImageList read FHeaderImages write SetHeaderImages;
    property DragImageList: TDragImageList read FDragImageList;
    property ColumnIconPainted: Boolean
      read FColumnIconPainted write FColumnIconPainted stored False;
    property HeaderHandle: HWND read FHeaderHandle;
    property ParentForm: TCustomForm read FParentForm;
    property DateTimeFormatStr: string
      read FDateTimeFormatStr write FDateTimeFormatStr stored False;
    property DateFormatStr: string read FDateFormatStr;
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
    {Display format of the date/time of the files:}
    property DateTimeDisplay: TDateTimeDisplay
      read FDateTimeDisplay write SetDateTimeDisplay default dtdDateTimeSec;
    property OnHeaderEndDrag: TNotifyEvent
      read  FOnHeaderEndDrag write FOnHeaderEndDrag;
    property OnHeaderEndTrack: TNotifyEvent
      read  FOnHeaderEndTrack write FOnHeaderEndTrack;

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
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property ViewStyle;
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
    property OnSelectByMask;
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
    property HeaderImages; // TCustomIEListView
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

const HDM_SETBITMAPMARGIN = (HDM_FIRST + 20);
const HDM_GETBITMAPMARGIN = (HDM_FIRST + 21);
const HDF_SORTUP = $400;
const HDF_SORTDOWN = $200;

procedure Register;
begin
  RegisterComponents('Martin', [TIEListView]);
end;

procedure Header_SetBitmapMargin(Header: HWnd; Margin: Integer);
begin
  SendMessage(Header, HDM_SETBITMAPMARGIN, Margin, 0);
end;

function Header_GetBitmapMargin(Header: HWnd): Integer;
begin
  Result := SendMessage(Header, HDM_GETBITMAPMARGIN, 0, 0);
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
  FHeaderImages := nil;
  FShowColumnIcon := True;
  FSortColumn := 0;
  FSortAscending := True;
  FHeaderCanvas := TCanvas.Create;
  SetDateTimeFormatString;
end; {Create}

procedure TCustomIEListView.SetSortColumn(Value: Integer);
begin
  if Value <> SortColumn then
  begin
    FSortColumn := Value;
    FSortAscending := True;
    if Items.Count > 0 then
      SortItems;
    SetColumnImages;
  end;
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
  if SortAscending <> Value then
  begin
    FSortAscending := Value;
    if Items.Count > 0 then
      SortItems;
    SetColumnImages;
  end;
end; {SetSortAscending}

procedure TCustomIEListView.SetHeaderImages(Value: TImageList);
begin
  if FHeaderImages <> Value then
  begin
    FHeaderImages := Value;
    if FHeaderHandle <> 0 then
      Header_SetImageList(FHeaderHandle, FHeaderImages.Handle);
  end;
end;

procedure TCustomIEListView.SetColumnImages;
var
  HdItem: THdItem;
  Index: Integer;
  SecondaryColumn: Integer;
  Margin, MaxMargin: Integer;
  Caption: string;
  ShowImage: Boolean;
  AliasOnly: Boolean;
begin
  if ShowColumnHeaders and HandleAllocated then
  begin
    for Index := 0 to Columns.Count-1 do
    begin
      HdItem.Mask := HDI_FORMAT;
      Header_GetItem(GetDlgItem(Self.Handle,0), Index, HdItem);

      Caption := TrimRight(Columns[Index].Caption);
      SecondaryColumn := SecondaryColumnHeader(Index, AliasOnly);
      ShowImage := False;
      if (HeaderImages <> nil) and FShowColumnIcon then
      begin
        if Index = SortColumn then ShowImage := True
          else
        if (SecondaryColumn >= 0) and (SecondaryColumn = SortColumn) then
        begin
          if AliasOnly then ShowImage := True
            else
          begin
            Margin := ColumnHeaderIconWidth +
              Canvas.TextWidth(Columns[SecondaryColumn].Caption);
            MaxMargin := Columns[Index].Width -
              SecondaryColumnHeaderOffset(Canvas, Index);
            if Margin <= MaxMargin then
            begin
              Caption := Caption +
                StringOfChar(' ', Margin div Canvas.TextWidth(' '));
              ShowImage := True;
            end;
          end;
        end;
      end;

      if ShowImage then
      begin
        HdItem.Mask := HDI_FORMAT or HDI_IMAGE;
        HdItem.fmt := Hditem.fmt or HDF_IMAGE;
        if SortAscending then
        begin
          HdItem.iImage := 0;
          HdItem.fmt := (Hditem.fmt or HDF_SORTUP) and (not HDF_SORTDOWN);
        end
          else
        begin
          HdItem.iImage := 1;
          HdItem.fmt := (Hditem.fmt or HDF_SORTDOWN) and (not HDF_SORTUP);
        end;
        if Columns[Index].Alignment = taLeftJustify then
          HdItem.fmt := HdItem.fmt or HDF_BITMAP_ON_RIGHT;
      end
        else
      begin
        HdItem.fmt := HdItem.fmt and
          (not (HDF_IMAGE or HDF_SORTUP or HDF_SORTDOWN));
      end;
      Columns[Index].Caption := Caption;

      Header_SetItem(GetDlgItem(Self.Handle, 0), Index, HDItem);
    end;
    FColumnIconPainted := True;
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

function TCustomIEListView.SecondaryColumnHeaderOffset(Canvas: TCanvas; Index: Integer): Integer;
begin
  Result :=
    11 +
    Canvas.TextWidth(TrimRight(Columns[Index].Caption)) +
    ColumnHeaderIconWidth;
end;

function TCustomIEListView.ColumnHeaderIconWidth: Integer;
begin
  Result := 12;
  if Assigned(HeaderImages) then
    Inc(Result, HeaderImages.Width);
end;

function TCustomIEListView.SecondaryColumnHeader(Index: Integer;
  var AliasOnly: Boolean): Integer;
begin
  if Assigned(OnSecondaryColumnHeader) then
  begin
    OnSecondaryColumnHeader(Self, Index, Result);
    AliasOnly := True;
  end
    else
  begin
    Result := -1;
    AliasOnly := False;
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
var
  Index: Integer;
  SecondaryColumn: Integer;
  SecondaryOffset: Integer;
  R: TRect;
  AliasOnly: Boolean;
begin
  Index := Column.Index;
  SecondaryColumn := SecondaryColumnHeader(Index, AliasOnly);
  if SecondaryColumn >= 0 then
  begin
    if AliasOnly then Index := SecondaryColumn
      else
    begin
      Header_GetItemRect(FHeaderHandle, Index, @R);
      // this doesn't take possible vertical scroll into account!
      SecondaryOffset := Mouse.CursorPos.x - ClientToScreen(R.TopLeft).x;
      if SecondaryOffset >= SecondaryColumnHeaderOffset(Canvas, Index) then
        Index := SecondaryColumn;
    end;
  end;

  if Index = SortColumn then FSortAscending := not FSortAscending
    else
  begin
    FSortColumn := Index;
    FSortAscending := SortAscendingByDefault(Index);
  end;

  if Items.Count > 0 then SortItems;

  SetColumnImages;

  inherited;
end; {ColClick}

procedure TCustomIEListView.WMPaint(var Msg: TWMPaint);
begin
  inherited;
  if (ViewStyle = vsReport) and not ColumnIconPainted and
     ShowColumnHeaders then SetColumnImages;
end; {WMPaint}

procedure TCustomIEListView.WMNotify(var Msg: TWMNotify);
var
  SecondaryColumn: Integer;
  AliasOnly: Boolean;
begin
  if (FHeaderHandle <> 0) and (Msg.NMHdr^.hWndFrom = FHeaderHandle) then
    case Msg.NMHdr.code of
      HDN_BEGINDRAG:
        {Due to a bug in D4 (until Update Pack 3) we should eat this message!};
      HDN_ENDDRAG:
        begin
          {Due to a bug in D4 (until Update Pack 3) we should eat this message!}
          HeaderEndDrag(Self);
          Invalidate;
          Exit;
        end;
       HDN_ENDTRACK, HDN_ENDTRACKW:
         begin
           SetColumnImages;
           FColumnIconPainted := False;
           Invalidate;
           inherited;
           if Assigned(FOnHeaderEndTrack) then
             FOnHeaderEndTrack(Self);
           Exit;
         end;
       HDN_DIVIDERDBLCLICK, HDN_DIVIDERDBLCLICKW:
         {Due to a bug in D4 (until Update Pack 3) the column property is
          not updated by this message:}
         begin
           inherited;
           with PHDNotify(Pointer(Msg.NMHdr))^ do
             if Columns.Count > Item then
               Columns[Item].Width := ListView_GetColumnWidth(Self.Handle, Item);
           if Assigned(FOnHeaderEndTrack) then
             FOnHeaderEndTrack(Self);
           SetColumnImages;
           FColumnIconPainted := False;
           Exit;
         end;
       NM_CUSTOMDRAW:
         with PNMCustomDraw(Msg.NMHdr)^ do
         begin
           inherited;
           if dwDrawStage = CDDS_PREPAINT then
           begin
             Msg.Result := Msg.Result or CDRF_NOTIFYITEMDRAW;
           end
             else
           if dwDrawStage = CDDS_ITEMPREPAINT then
           begin
             if (SecondaryColumnHeader(dwItemSpec, AliasOnly) >= 0) and (not AliasOnly) then
               Msg.Result := Msg.Result or CDRF_NOTIFYPOSTPAINT;
           end
             else
           if dwDrawStage = CDDS_ITEMPOSTPAINT then
           begin
             SecondaryColumn := SecondaryColumnHeader(dwItemSpec, AliasOnly);
             if (SecondaryColumn >= 0) and (not AliasOnly) then
             begin
               FHeaderCanvas.Handle := hdc;
               FHeaderCanvas.Font := Font;
               FHeaderCanvas.Brush := Brush;
               FHeaderCanvas.Brush.Style := bsClear;
               if (uItemState and CDIS_SELECTED) <> 0 then
               begin
                 Inc(rc.top);
                 Inc(rc.left);
               end;
               Inc(rc.left, SecondaryColumnHeaderOffset(FHeaderCanvas, dwItemSpec));
               Inc(rc.top,
                 ((rc.bottom - rc.top) -
                    FHeaderCanvas.TextHeight(Columns[SecondaryColumn].Caption)) div 2);
               DrawText(FHeaderCanvas.Handle, PChar(Columns[SecondaryColumn].Caption),
                 Length(Columns[SecondaryColumn].Caption), rc, 0);
             end;
           end;
           Exit;
         end;
    end; {Case}

  inherited;
end; { TCustomIEListView.WMNotify }

procedure TCustomIEListView.HeaderEndDrag(Sender : TObject);
begin
  if Assigned(FOnHeaderEndDrag) then
    FOnHeaderEndDrag(Self);
end; {HeaderEndDrag}

procedure TCustomIEListView.Loaded;
begin
  inherited;
  FHeaderHandle := ListView_GetHeader(Self.Handle);
  if (FHeaderImages <> nil) and (FHeaderHandle <> 0) then
    Header_SetImageList(FHeaderHandle, FHeaderImages.Handle);
end; {Loaded}

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
end; {CreateWnd}

destructor TCustomIEListView.Destroy;
begin
  if Assigned(FDragImageList) then
  begin
    if GlobalDragImageList = FDragImageList then
      GlobalDragImageList := nil;
    FDragImageList.Free;
  end;
  FHeaderCanvas.Free;

  inherited;
end; {Destroy}

procedure TCustomIEListView.SetDateTimeDisplay(Value: TDateTimeDisplay);
begin
  if Value <> FDateTimeDisplay then
  begin
    FDateTimeDisplay := Value;
    SetDateTimeFormatString;
    Invalidate;
  end;
end; {SetDateTimeDisplay}

procedure TCustomIEListView.SetDateTimeFormatString;
var
  ShortDate: string;
begin
  ShortDate := UpperCase(ShortDateFormat);
  {Create DateTime format string:}
  if Pos('YYYY', UpperCase(ShortDate)) = 0 then
  begin
    if Copy(UpperCase(ShortDate), Length(ShortDate) - 1, 2) = 'YY' then
        FDateTimeFormatStr := ShortDateFormat + 'yy'
      else
    if Copy(UpperCase(ShortDate), 1, 2) = 'YY' then
      FDateTimeFormatStr := 'yy' + ShortDateFormat;
  end
    else FDateTimeFormatStr := ShortDateFormat;

  FDateFormatStr := FDateTimeFormatStr;

  if FDateTimeDisplay = dtdDateTimeSec then
      FDateTimeFormatStr := FDateTimeFormatStr + '  ' + LongTimeFormat
    else
  if fDateTimeDisplay = dtdDateTime then
    FDateTimeFormatStr := FDateTimeFormatStr + '  ' + ShortTimeFormat;
end; {SetDateTimeFormatString}

procedure TCustomIEListView.SortItems;
begin
  AlphaSort;
end;

end.
