unit NortonLikeListView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ListViewColProperties, CommCtrl;

type
  TCustomNortonLikeListView = class;
  TSelectMode = (smAll, smNone, smInvert); 
  TSelectByMaskEvent = procedure(Control: TCustomNortonLikeListView; Select: Boolean) of object;

  TCustomNortonLikeListView = class(TCustomListView)
  private
    { Private declarations }
    FColProperties: TCustomListViewColProperties;
    FDontSelectItem: Boolean;
    FDontUnSelectItem: Boolean;
    FSelCount: Integer;
    FNortonLike: Boolean;
    FOnSelectByMask: TSelectByMaskEvent;
    FLastDeletedItem: TListItem; // aby sme nepocitali smazany item 2x
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    function GetMarkedCount: Integer;
    function GetMarkedFile: TListItem;
  protected
    { Protected declarations }
    FClearingItems: Boolean;
    FUpdatingSelection: Integer;
    procedure CreateWnd; override;
    procedure BeginSelectionUpdate; virtual;
    procedure EndSelectionUpdate; virtual;
    function CanChangeSelection(Item: TListItem; Select: Boolean): Boolean; virtual;
    procedure ClearItems; virtual;
    procedure ColRightClick(Column: TListColumn; Point: TPoint); override;
    procedure Delete(Item: TListItem); override;
    function DoSelectByMask(Select: Boolean): Boolean; virtual;
    function ExCanChange(Item: TListItem; Change: Integer;
      NewState, OldState: Word): Boolean; dynamic;
    procedure InsertItem(Item: TListItem); override;
    function NewColProperties: TCustomListViewColProperties; virtual;
    procedure FocusSomething;
    function GetItemFromHItem(const Item: TLVItem): TListItem;
    function GetValid: Boolean; virtual;
    function GetSelCount: Integer; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    function ClosestUnselected(Item: TListItem): TListItem;
    procedure SelectAll(Mode: TSelectMode); reintroduce;

    property ColProperties: TCustomListViewColProperties read FColProperties write FColProperties stored False;
    //CLEAN property SelCount: Integer read GetSelCount;

    property MultiSelect default True;
    property NortonLike: Boolean read FNortonLike write FNortonLike default True;
    property OnSelectByMask: TSelectByMaskEvent read FOnSelectByMask write FOnSelectByMask;
    property MarkedCount: Integer read GetMarkedCount;
    property MarkedFile: TListItem read GetMarkedFile;
    property Valid: Boolean read GetValid;
  end;

type
  TNortonLikeListView = class(TCustomNortonLikeListView)
  published
    { Published declarations }
    property Align;
    property AllocBy;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Checkboxes;
    property Color;
    //property Columns;
    property ColumnClick;
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
    property Items;
    property LargeImages;
    property ReadOnly;
    property RowSelect;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowHint;
    property SmallImages;
    property StateImages;
    property TabOrder;
    property TabStop;
    property ViewStyle;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnCustomDraw;
    property OwnerDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OwnerData;
    property OnGetImageIndex;
    property OnCompare;
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
    property OnSelectItem;

    property NortonLike;
    property OnSelectByMask;
    property ColProperties;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Martin', [TNortonLikeListView]);
end;

  { TCustomNortonLikeListView }

constructor TCustomNortonLikeListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSelCount := 0;
  FClearingItems := False;
  MultiSelect := True;
  FDontSelectItem := False;
  FDontUnSelectItem := False;
  FNortonLike := True;
  FColProperties := NewColProperties;
  FLastDeletedItem := nil;
  FUpdatingSelection := 0;
end;

procedure TCustomNortonLikeListView.Delete(Item: TListItem);
begin
  if (FLastDeletedItem <> Item) and Item.Selected then
    Dec(FSelCount);
  FLastDeletedItem := Item;
  inherited;
  FLastDeletedItem := nil;
end;

function TCustomNortonLikeListView.DoSelectByMask(Select: Boolean): Boolean;
begin
  if Assigned(FOnSelectByMask) then
  begin
    FOnSelectByMask(Self, Select);
    Result := True;
  end
    else Result := False;
end;

function TCustomNortonLikeListView.ExCanChange(Item: TListItem; Change: Integer;
  NewState, OldState: Word): Boolean;
begin
  Assert(Assigned(Item));
  Result := True;
  if (Change = LVIF_STATE) and
     ((((OldState and LVIS_SELECTED) < (NewState and LVIS_SELECTED)) and
       ((FDontSelectItem and FNortonLike) or (not CanChangeSelection(Item, True)))) or
      (((OldState and LVIS_SELECTED) > (NewState and LVIS_SELECTED)) and
       ((FDontUnSelectItem and FNortonLike) or (not CanChangeSelection(Item, False))))) then
  begin
    if (OldState or LVIS_SELECTED) <> (NewState or LVIS_SELECTED) then
      ListView_SetItemState(Handle, Item.Index, NewState,
        (NewState or OldState) - LVIS_SELECTED);
    Result := False;
  end;
end;

function TCustomNortonLikeListView.CanChangeSelection(Item: TListItem;
  Select: Boolean): Boolean;
begin
  Result := True;
end;

procedure TCustomNortonLikeListView.ClearItems;
begin
  Items.BeginUpdate;
  try
    FClearingItems := True;
    Items.Clear;
  finally
    FSelCount := 0;
    FClearingItems := False;
    Items.EndUpdate;
  end;
end; { ClearItems }

procedure TCustomNortonLikeListView.ColRightClick(Column: TListColumn; Point: TPoint);
var
  HitInfo: TLVHitTestInfo;
begin
  // Fix: Otherwise we get wrong column when view is horizontally scrolled
  HitInfo.pt := Point;
  if ListView_SubItemHitTest(Handle, @HitInfo) = 0 then
    Column := Columns[HitInfo.iSubItem];
  inherited ColRightClick(Column, Point);
end;

function TCustomNortonLikeListView.ClosestUnselected(Item: TListItem): TListItem;
var
  Index: Integer;
begin
  if Assigned(Item) and (Item.Selected or (NortonLike and (SelCount = 0))) then
  begin
    Index := Item.Index + 1;
    while (Index < Items.Count) and Items[Index].Selected do Inc(Index);
    if (Index >= Items.Count) or Items[Index].Selected then
    begin
      Index := Item.Index - 1;
      while (Index >= 0) and Items[Index].Selected do Dec(Index);
    end;
    if (Index >= 0) and (Index < Items.Count) and (not Items[Index].Selected) then
      Result := Items[Index]
    else
      Result := nil;
  end
    else Result := Item;
end;

procedure TCustomNortonLikeListView.WMNotify(var Message: TWMNotify);
begin
  // disallow resizing of "invisible" (width=0) columns
  with PHDNotify(Message.NMHdr)^ do
    case Hdr.code of
      HDN_BEGINTRACK, HDN_TRACK, HDN_BEGINTRACKW, HDN_TRACKW:
        if not ColProperties.Visible[Item] then
        begin
          Message.Result := 1;
          Exit;
        end;
    end;

  inherited;
end;

procedure TCustomNortonLikeListView.CNNotify(var Message: TWMNotify);
begin
  with Message do
    case NMHdr^.code of
      LVN_ITEMCHANGING:
        with PNMListView(NMHdr)^ do
          if Valid and (not FClearingItems) and (Items[iItem] <> FLastDeletedItem) and
             ((not CanChange(Items[iItem], uChanged)) or
              (not ExCanChange(Items[iItem], uChanged, uNewState, uOldState)))
              then Result := 1;
      LVN_ITEMCHANGED:
        begin
          with PNMListView(NMHdr)^ do
          if Valid and (not FClearingItems) and
             (uChanged = LVIF_STATE) and (Items[iItem] <> FLastDeletedItem) and
             ((uOldState and LVIS_SELECTED) <> (uNewState and LVIS_SELECTED)) then
          begin
            if (uOldState and LVIS_SELECTED) <> 0 then Dec(FSelCount)
              else Inc(FSelCount);
          end;
          inherited;
        end;
      else
        inherited;
    end;
end;

procedure TCustomNortonLikeListView.WMKeyDown(var Message: TWMKeyDown);
var
  PDontUnSelectItem: Boolean;
  PDontSelectItem: Boolean;
  Item: TListItem;
begin
  if NortonLike and (Message.CharCode = VK_INSERT) then
  begin
    if Items.Count > 0 then
    begin
      Item := ItemFocused;
      if Item = nil then Item := Items[0];
      Item.Selected := not Item.Selected;
      SendMessage(Handle, WM_KEYDOWN, VK_DOWN, LongInt(0));
      Message.Result := 1;
    end;
  end
    else
  {if NortonLike and (Message.CharCode = VK_MULTIPLY) then
  begin
    SelectAll(smInvert)
    Message.Result := 1;
  end
    else }
  if (Message.CharCode = VK_ADD) or (Message.CharCode = VK_SUBTRACT) then
  begin
    if DoSelectByMask((Message.CharCode = VK_ADD)) then
      Message.Result := 1;
  end
    else
  if (Message.CharCode in [VK_SPACE, VK_PRIOR, VK_NEXT, VK_END, VK_HOME, VK_LEFT,
    VK_UP, VK_RIGHT, VK_DOWN, VK_SELECT]) then
  begin
    PDontSelectItem := FDontUnSelectItem;
    PDontUnSelectItem := FDontUnSelectItem;
    FDontSelectItem := True;
    FDontUnSelectItem := True;
    try
      inherited;
    finally
      FDontSelectItem := PDontSelectItem;
      FDontUnSelectItem := PDontUnSelectItem;
    end;
  end
    else inherited;
end;

procedure TCustomNortonLikeListView.WMChar(var Message: TWMChar);
var
  PDontUnSelectItem: Boolean;
  PDontSelectItem: Boolean;
begin
  if NortonLike and (Message.CharCode = Byte(' ')) then
  begin
    if (GetKeyState(VK_CONTROL) >= 0) then
    begin
      if Assigned(ItemFocused) then
        ItemFocused.Selected := not ItemFocused.Selected;
    end
      else inherited;
  end
    else
  begin
    PDontSelectItem := FDontUnSelectItem;
    PDontUnSelectItem := FDontUnSelectItem;
    FDontSelectItem := True;
    FDontUnSelectItem := True;
    try
      inherited;
    finally
      FDontSelectItem := PDontSelectItem;
      FDontUnSelectItem := PDontUnSelectItem;
    end;
  end;
end;

procedure TCustomNortonLikeListView.FocusSomething;
begin
  if Valid and (Items.Count > 0) and not Assigned(ItemFocused) then
  begin
    if NortonLike then SendMessage(Handle, WM_KEYDOWN, VK_DOWN, LongInt(0));
    if not Assigned(ItemFocused) then
      ItemFocused := Items[0];
  end;
  if Assigned(ItemFocused) then
    ItemFocused.MakeVisible(False);
end;

procedure TCustomNortonLikeListView.SelectAll(Mode: TSelectMode);
var
  Index: Integer;
  Item: TListItem;
begin
  BeginSelectionUpdate;
  try
    for Index := 0 to Items.Count - 1 do
    begin
      Item := Items[Index];
      case Mode of
        smAll: Item.Selected := True;
        smNone: Item.Selected := False;
        smInvert: Item.Selected := not Item.Selected;
      end;
    end;
  finally
    EndSelectionUpdate;
  end;
end;

procedure TCustomNortonLikeListView.WMLButtonDown(var Message: TWMLButtonDown);
var
  PDontUnSelectItem: Boolean;
  PDontSelectItem: Boolean;
  Shift: TShiftState;
begin
  Shift := KeysToShiftState(Message.Keys);
  PDontSelectItem := FDontUnSelectItem;
  PDontUnSelectItem := FDontUnSelectItem;
  FDontSelectItem := ((Shift * [ssCtrl, ssShift]) = []);
  FDontUnSelectItem := ((Shift * [ssCtrl]) = []);
  try
    inherited;
  finally
    FDontSelectItem := PDontSelectItem;
    FDontUnSelectItem := PDontUnSelectItem;
  end;
end;

procedure TCustomNortonLikeListView.WMRButtonDown(var Message: TWMRButtonDown);
var
  PDontUnSelectItem: Boolean;
  PDontSelectItem: Boolean;
begin
  PDontSelectItem := FDontUnSelectItem;
  PDontUnSelectItem := FDontUnSelectItem;
  FDontSelectItem := True;
  FDontUnSelectItem := True;
  try
    inherited;
  finally
    FDontSelectItem := PDontSelectItem;
    FDontUnSelectItem := PDontUnSelectItem;
  end;
end;

function TCustomNortonLikeListView.GetMarkedFile: TListItem;
begin
  if Assigned(Selected) then Result := Selected
    else
  if Assigned(ItemFocused) and NortonLike then Result := ItemFocused
    else Result := nil;
end;

function TCustomNortonLikeListView.GetSelCount: Integer;
begin
  Result := FSelCount;
end;

procedure TCustomNortonLikeListView.InsertItem(Item: TListItem);
begin
  inherited;
  if Item.Selected then Inc(FSelCount);
end;

function TCustomNortonLikeListView.NewColProperties: TCustomListViewColProperties;
begin
  Result := TListViewColProperties.Create(Self, 5);
end;

function TCustomNortonLikeListView.GetItemFromHItem(const Item: TLVItem): TListItem;
begin
  with Item do
    if (state and LVIF_PARAM) <> 0 then Result := Pointer(lParam)
      else Result := Items[iItem];
end;

function TCustomNortonLikeListView.GetMarkedCount: Integer;
begin
  if (SelCount > 0) or (not NortonLike) then Result := SelCount
    else
  if Assigned(ItemFocused) then Result := 1
    else Result := 0;
end;

function TCustomNortonLikeListView.GetValid: Boolean;
begin
  // Note that TCustomDirView::GetValid don't inherit
  // this method because of optimalization
  Result := (not (csDestroying in ComponentState)) and (not FClearingItems);
end;

procedure TCustomNortonLikeListView.BeginSelectionUpdate;
begin
  // Higher value is probably some nesting error
  Assert(FUpdatingSelection in [0..4]);
  Inc(FUpdatingSelection);
end; { BeginUpdatingSelection }

procedure TCustomNortonLikeListView.EndSelectionUpdate;
begin
  Assert(FUpdatingSelection > 0);
  Dec(FUpdatingSelection);
end; { EndUpdatingSelection }

procedure TCustomNortonLikeListView.CreateWnd;
begin
  inherited;
  
  Assert(ColProperties <> nil);
  ColProperties.ListViewWndCreated;
end;

end.
