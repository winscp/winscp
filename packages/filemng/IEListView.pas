unit IEListView;
{==================================================================
 Component TIEListView / Version 1.0, September 1999
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

{Required compiler options for TIEListView:}
{$A+,B-,X+,H+,P+}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, ActiveX, CommCtrl, Extctrls, ImgList, BaseUtils,
  ComCtrls, NortonLikeListView;

type
  TDateTimeDisplay = (dtdDateTimeSec, dtdDateTime, dtdDate);

type
  TIEListView = class(TCustomNortonLikeListView)
  private
    FSortColumn: Integer;
    FSortAscending: Boolean;
    FColumnIconPainted: Boolean;
    FShowColumnIcon: Boolean;
    FHeaderHandle: HWND;
    FParentForm: TCustomForm;
    FMask: string;
    FHeaderCanvas: TCanvas;

    FOnHeaderEndDrag: TNotifyEvent;
    FOnHeaderEndTrack: TNotifyEvent;

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
    procedure SetMask(Value: string); virtual;
    procedure SetHeaderImages(Value: TImageList); virtual;
    function SecondaryColumnHeader(Index: Integer): Integer; virtual;

    procedure CreateWnd; override;
    procedure ColClick(Column: TListColumn); override;
    procedure Loaded; override;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMNotify(var Msg: TWMNotify); message WM_NOTIFY;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetColumnImages; virtual;
    function NormalizeMask(Mask: string): string; dynamic;

    property HeaderImages: TImageList read FHeaderImages write SetHeaderImages;
    property DragImageList: TDragImageList read FDragImageList;
    property ColumnIconPainted: Boolean
      read FColumnIconPainted write FColumnIconPainted stored False;
    property HeaderHandle: HWND read FHeaderHandle;
    property ParentForm: TCustomForm read FParentForm;
    property DateTimeFormatStr: string
      read FDateTimeFormatStr write FDateTimeFormatStr stored False;
    property DateFormatStr: string read FDateFormatStr;
    {filemask, multiple filters are possible: '*.pas;*.dfm'}
    property Mask: string read FMask write SetMask;
    {Set the sort column of the listview}
    property SortColumn: Integer read FSortColumn write SetSortColumn;
    {Show the sorting symbol in the listview's header:}
    property ShowColumnIcon: Boolean
      read FShowColumnIcon write SetShowColumnIcon default True;
    {Sortorder of actual sort column}
    property SortAscending: Boolean
      read FSortAscending write SetSortAscending default True;

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
    Property RowSelect;
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
  end; {Type TIEListView}

var
  GlobalDragImageList: TDragImageList;

implementation

const HDM_SETBITMAPMARGIN = (HDM_FIRST + 20);
const HDM_GETBITMAPMARGIN = (HDM_FIRST + 21);
const HDF_SORTUP = $400;
const HDF_SORTDOWN = $200;

procedure Header_SetBitmapMargin(Header: HWnd; Margin: Integer);
begin
  SendMessage(Header, HDM_SETBITMAPMARGIN, Margin, 0);
end;

function Header_GetBitmapMargin(Header: HWnd): Integer;
begin
  Result := SendMessage(Header, HDM_GETBITMAPMARGIN, 0, 0);
end;

{ TIEListView }

constructor TIEListView.Create(AOwner: TComponent);
begin
  inherited;

  ColProperties.OnChange := ColPropertiesChange;
  FHeaderImages := nil;
  FShowColumnIcon := True;
  FSortColumn := 0;
  FSortAscending := True;
  FMask := '*.*';
  FHeaderCanvas := TCanvas.Create;
  SetDateTimeFormatString;
end; {Create}

procedure TIEListView.SetSortColumn(Value: Integer);
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

procedure TIEListView.SetViewStyle(Value: TViewStyle);
begin
  if Value <> ViewStyle then
  begin
    inherited SetViewStyle(Value);
    if ViewStyle = vsReport then
      SetColumnImages;
  end;
end; {SetViewStyle}

procedure TIEListView.SetSortAscending(Value: Boolean);
begin
  if SortAscending <> Value then
  begin
    FSortAscending := Value;
    if Items.Count > 0 then
      SortItems;
    SetColumnImages;
  end;
end; {SetSortAscending}

procedure TIEListView.SetHeaderImages(Value: TImageList);
begin
  if FHeaderImages <> Value then
  begin
    FHeaderImages := Value;
    if FHeaderHandle <> 0 then
      Header_SetImageList(FHeaderHandle, FHeaderImages.Handle);
  end;
end;

procedure TIEListView.SetColumnImages;
var
  HdItem: THdItem;
  Index: Integer;
  SecondaryColumn: Integer;
  Margin, MaxMargin: Integer;
  Caption: string;
  ShowImage: Boolean;
begin
  if ShowColumnHeaders and HandleAllocated then
  begin
    for Index := 0 to Columns.Count-1 do
    begin
      HdItem.Mask := HDI_FORMAT;
      Header_GetItem(GetDlgItem(Self.Handle,0), Index, HdItem);

      Caption := TrimRight(Columns[Index].Caption);
      SecondaryColumn := SecondaryColumnHeader(Index);
      ShowImage := False;
      if (HeaderImages <> nil) and FShowColumnIcon then
      begin
        if Index = SortColumn then ShowImage := True
          else
        if (SecondaryColumn >= 0) and (SecondaryColumn = SortColumn) then
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

procedure TIEListView.SetShowColumnIcon(Value: Boolean);
begin
  if Value <> ShowColumnIcon then
  begin
    FShowColumnIcon := Value;
    SetColumnImages;
  end;
end; {SetShowColumnIcon}

function TIEListView.SecondaryColumnHeaderOffset(Canvas: TCanvas; Index: Integer): Integer;
begin
  Result :=
    11 +
    Canvas.TextWidth(TrimRight(Columns[Index].Caption)) +
    ColumnHeaderIconWidth;
end;

function TIEListView.ColumnHeaderIconWidth: Integer;
begin
  Result := 12;
  if Assigned(HeaderImages) then
    Inc(Result, HeaderImages.Width);
end;

function TIEListView.SecondaryColumnHeader(Index: Integer): Integer;
begin
  Result := -1;
end;

procedure TIEListView.ColClick(Column: TListColumn);
var
  Index: Integer;
  SecondaryColumn: Integer;
  SecondaryOffset: Integer;
  R: TRect;
begin
  Index := Column.Index;
  SecondaryColumn := SecondaryColumnHeader(Index);
  if SecondaryColumn >= 0 then
  begin
    Header_GetItemRect(FHeaderHandle, Index, @R);
    // this doesn't take possible vertical scroll into account!
    SecondaryOffset := Mouse.CursorPos.x - ClientToScreen(R.TopLeft).x;
    if SecondaryOffset >= SecondaryColumnHeaderOffset(Canvas, Index) then
      Index := SecondaryColumn;
  end;

  if Index = SortColumn then FSortAscending := not FSortAscending
    else
  begin
    FSortColumn := Index;
    FSortAscending := True;
  end;

  if Items.Count > 0 then SortItems;

  SetColumnImages;

  inherited;
end; {ColClick}

procedure TIEListView.WMPaint(var Msg: TWMPaint);
begin
  inherited;
  if (ViewStyle = vsReport) and not ColumnIconPainted and
     ShowColumnHeaders then SetColumnImages;
end; {WMPaint}

procedure TIEListView.WMNotify(var Msg: TWMNotify);
var
  SecondaryColumn: Integer;
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
             if SecondaryColumnHeader(dwItemSpec) >= 0 then
               Msg.Result := Msg.Result or CDRF_NOTIFYPOSTPAINT;
           end
             else
           if dwDrawStage = CDDS_ITEMPOSTPAINT then
           begin
             SecondaryColumn := SecondaryColumnHeader(dwItemSpec);
             if SecondaryColumn >= 0 then
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
end; { TIElistView.WMNotify }

procedure TIEListView.HeaderEndDrag(Sender : TObject);
begin
  if Assigned(FOnHeaderEndDrag) then
    FOnHeaderEndDrag(Self);
end; {HeaderEndDrag}

procedure TIEListView.Loaded;
begin
  inherited;
  FHeaderHandle := ListView_GetHeader(Self.Handle);
  if (FHeaderImages <> nil) and (FHeaderHandle <> 0) then
    Header_SetImageList(FHeaderHandle, FHeaderImages.Handle);
end; {Loaded}

procedure TIEListView.ColPropertiesChange(Sender: TObject);
begin
  SetColumnImages;
end;

procedure TIEListView.CreateWnd;
begin
  inherited;

  FParentForm := GetParentForm(Self);
  if not (csDesigning in ComponentState) then
    FDragImageList := TDragImageList.Create(Self);
  if not Assigned(GlobalDragImageList) then
    GlobalDragImageList := DragImageList;
end; {CreateWnd}

destructor TIEListView.Destroy;
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

procedure TIEListView.SetDateTimeDisplay(Value: TDateTimeDisplay);
begin
  if Value <> FDateTimeDisplay then
  begin
    FDateTimeDisplay := Value;
    SetDateTimeFormatString;
    Invalidate;
  end;
end; {SetDateTimeDisplay}

procedure TIEListView.SetDateTimeFormatString;
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

procedure TIEListView.SetMask(Value: string);
begin
  Value := NormalizeMask(Value);
  FMask := Value;
end;{SetMask}

function TIEListView.NormalizeMask(Mask: string): string;
begin
  Mask := Trim(Mask);
  if Length(Mask) = 0 then Mask := '*.*';

  StrTranslate(Mask, ' ;,;');

  while Pos(';;', Mask) <> 0 do
    System.Delete(Mask, Pos(';;', Mask), 1);

  Result := LowerCase(Mask);
end; {NormalizeMask}

procedure TIEListView.SortItems;
begin
end;

end.