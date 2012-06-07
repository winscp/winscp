unit NortonLikeListView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ListViewColProperties, CommCtrl;

type
  TCustomNortonLikeListView = class;
  TSelectMode = (smAll, smNone, smInvert);
  TNortonLikeMode = (nlOn, nlOff, nlKeyboard);
  TSelectByMaskEvent = procedure(Control: TCustomNortonLikeListView; Select: Boolean) of object;

  TCustomNortonLikeListView = class(TCustomListView)
  private
    { Private declarations }
    FColProperties: TCustomListViewColProperties;
    FDontSelectItem: Boolean;
    FDontUnSelectItem: Boolean;
    FSelCount: Integer;
    FNortonLike: TNortonLikeMode;
    FOnSelectByMask: TSelectByMaskEvent;
    FLastDeletedItem: TListItem; // aby sme nepocitali smazany item 2x
    FFocusingItem: Boolean;
    FManageSelection: Boolean;
    FForceUpdateOnItemUnfocus: Boolean;
    FFirstSelected: Integer;
    FLastSelected: Integer;
    FFocused: TDateTime;
    FIgnoreSetFocusFrom: THandle;
    FSelectingImplicitly: Boolean;
    FAnyAndAllSelectedImplicitly: Boolean;
    FLButtonDownShiftState: TShiftState;
    FLButtonDownPos: TPoint;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure LVMEditLabel(var Message: TMessage); message LVM_EDITLABEL;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    function GetMarkedCount: Integer;
    function GetMarkedFile: TListItem;
    procedure ItemSelected(Item: TListItem; Index: Integer);
    procedure ItemUnselected(Item: TListItem; Index: Integer);
    procedure SelectAll(Mode: TSelectMode; Exclude: TListItem); reintroduce; overload;
  protected
    { Protected declarations }
    FClearingItems: Boolean;
    FUpdatingSelection: Integer;
    procedure CreateWnd; override;
    procedure BeginSelectionUpdate; virtual;
    procedure EndSelectionUpdate; virtual;
    function CanChangeSelection(Item: TListItem; Select: Boolean): Boolean; virtual;
    procedure ClearItems; virtual;
    procedure ItemsReordered;
    procedure ColRightClick(Column: TListColumn; Point: TPoint); override;
    procedure Delete(Item: TListItem); override;
    function DoSelectByMask(Select: Boolean): Boolean; virtual;
    function ExCanChange(Item: TListItem; Change: Integer;
      NewState, OldState: Word): Boolean; dynamic;
    procedure InsertItem(Item: TListItem); override;
    function NewColProperties: TCustomListViewColProperties; virtual;
    procedure FocusSomething; virtual;
    function EnableDragOnClick: Boolean; virtual;
    procedure FocusItem(Item: TListItem);
    function GetItemFromHItem(const Item: TLVItem): TListItem;
    function GetValid: Boolean; virtual;
    function GetSelCount: Integer; override;
    procedure DDBeforeDrag;
    function CanEdit(Item: TListItem): Boolean; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ClosestUnselected(Item: TListItem): TListItem;
    procedure SelectAll(Mode: TSelectMode); reintroduce; overload;
    procedure SelectCurrentItem(FocusNext: Boolean);
    function GetNextItem(StartItem: TListItem; Direction: TSearchDirection;
      States: TItemStates): TListItem;

    property ColProperties: TCustomListViewColProperties read FColProperties write FColProperties stored False;

    property MultiSelect default True;
    property NortonLike: TNortonLikeMode read FNortonLike write FNortonLike default nlOn;
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

uses
  PasTools;

procedure Register;
begin
  RegisterComponents('Martin', [TNortonLikeListView]);
end;

  { TCustomNortonLikeListView }

constructor TCustomNortonLikeListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSelCount := 0;
  FFirstSelected := -1;
  FLastSelected := -1;
  FClearingItems := False;
  MultiSelect := True;
  FDontSelectItem := False;
  FDontUnSelectItem := False;
  FNortonLike := nlOn;
  FColProperties := NewColProperties;
  FLastDeletedItem := nil;
  FUpdatingSelection := 0;
  FFocusingItem := False;
  // On Windows Vista, native GetNextItem for selection stops working once we
  // disallow deselecting any item (see ExCanChange).
  // So we need to manage selection state ourselves
  // cannot use Win32MajorVersion as it is affected by compatibility mode and
  // the bug is present even in compatibility mode
  FManageSelection := IsVista;
  FFocused := 0;
  FIgnoreSetFocusFrom := INVALID_HANDLE_VALUE;
  // On Windows 7 we have to force item update when it looses focus,
  // otherwise some remnants of focus rectangle remain
  // Doing the same on WinXP makes list view down from the item flicker,
  // so we avoid this there.
  // Not sure about Vista
  FForceUpdateOnItemUnfocus :=
    (Win32MajorVersion > 6) or
    ((Win32MajorVersion = 6) and (Win32MinorVersion >= 1));
end;

destructor TCustomNortonLikeListView.Destroy;
begin
  FColProperties.Free;
  inherited;
end;

procedure TCustomNortonLikeListView.ItemSelected(Item: TListItem; Index: Integer);
begin
  Inc(FSelCount);

  if FSelectingImplicitly and (FSelCount = 1) then
  begin
    FAnyAndAllSelectedImplicitly := True;
  end
    else
  if not FSelectingImplicitly then
  begin
    FAnyAndAllSelectedImplicitly := False;
  end;

  if FManageSelection then
  begin
    if Index < 0 then
      Index := Item.Index;

    if FSelCount = 1 then
    begin
      Assert(FFirstSelected < 0);
      FFirstSelected := Index;
      Assert(FLastSelected < 0);
      FLastSelected := Index;
    end
      else
    begin
      // if reference is not assigned, do not assign it as we
      // cannot be sure that the item is actually first/last
      if (FFirstSelected >= 0) and (Index < FFirstSelected) then
        FFirstSelected := Index;
      if (FLastSelected >= 0) and (Index > FLastSelected) then
        FLastSelected := Index;
    end;
  end;
end;

procedure TCustomNortonLikeListView.ItemUnselected(Item: TListItem; Index: Integer);
begin
  Dec(FSelCount);
  if (FSelCount = 0) or (not FSelectingImplicitly) then
  begin
    FAnyAndAllSelectedImplicitly := False;
  end;
  if FManageSelection then
  begin
    if Index < 0 then
      Index := Item.Index;

    if FFirstSelected = Index then
    begin
      if FSelCount = 1 then
        FFirstSelected := FLastSelected // may be -1
      else
        FFirstSelected := -1;
    end;
    if FLastSelected = Index then
    begin
      if FSelCount = 1 then
        FLastSelected := FFirstSelected // may be -1
      else
        FLastSelected := -1;
    end;
  end;
end;

procedure TCustomNortonLikeListView.Delete(Item: TListItem);
begin
  if (FLastDeletedItem <> Item) and Item.Selected then
  begin
    ItemUnselected(Item, -1);
  end;
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
       (FDontSelectItem or (not CanChangeSelection(Item, True)))) or
      (((OldState and LVIS_SELECTED) > (NewState and LVIS_SELECTED)) and
       (FDontUnSelectItem or (not CanChangeSelection(Item, False))))) then
  begin
    if (OldState or LVIS_SELECTED) <> (NewState or LVIS_SELECTED) then
    begin
      ListView_SetItemState(Handle, Item.Index, NewState,
        (NewState or OldState) - LVIS_SELECTED);
    end;
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
    if FManageSelection then
    begin
      FFirstSelected := -1;
      FLastSelected := -1;
    end;
    FClearingItems := False;
    Items.EndUpdate;
  end;
end; { ClearItems }

procedure TCustomNortonLikeListView.ItemsReordered;
begin
  if FManageSelection then
  begin
    FFirstSelected := -1;
    FLastSelected := -1;
  end;
end;

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
  if Assigned(Item) and (Item.Selected or ((NortonLike <> nlOff) and (SelCount = 0))) then
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

procedure TCustomNortonLikeListView.DDBeforeDrag;
begin
  FDontSelectItem := False;
  FDontUnSelectItem := False;
end;

procedure TCustomNortonLikeListView.CNNotify(var Message: TWMNotify);
var
  Item: TListItem;
begin
  with Message do
    case NMHdr^.code of
      LVN_ITEMCHANGING:
        with PNMListView(NMHdr)^ do
        begin
          Item := Items[iItem];
          if Valid and (not FClearingItems) and (Item <> FLastDeletedItem) and
             ((not CanChange(Item, uChanged)) or
              (not ExCanChange(Item, uChanged, uNewState, uOldState)))
              then
          begin
            Result := 1;
          end;
        end;
      LVN_ITEMCHANGED:
        begin
          with PNMListView(NMHdr)^ do
          begin
            Item := Items[iItem];
            if Valid and (not FClearingItems) and
               (uChanged = LVIF_STATE) and (Item <> FLastDeletedItem) then
            begin
              if FForceUpdateOnItemUnfocus and
                 (NortonLike <> nlOff) and
                 ((uOldState and LVIS_FOCUSED) > (uNewState and LVIS_FOCUSED)) then
              begin
                // force update, otherwise some remnants of focus rectangle remain
                Item.Update;
              end;
              if (uOldState and LVIS_SELECTED) <> (uNewState and LVIS_SELECTED) then
              begin
                if (uOldState and LVIS_SELECTED) <> 0 then
                begin
                  ItemUnselected(Item, iItem);
                end
                  else
                begin
                  ItemSelected(Item, iItem);
                end;
              end;
            end;
          end;
          inherited;
        end;
      LVN_ENDLABELEDIT:
        begin
          FIgnoreSetFocusFrom := ListView_GetEditControl(Handle);
          inherited;
        end;
      else
        inherited;
    end;
end;

procedure TCustomNortonLikeListView.SelectCurrentItem(FocusNext: Boolean);
var
  Item: TListItem;
begin
  Item := ItemFocused;
  if Item = nil then Item := Items[0];
  Item.Selected := not Item.Selected;
  if FocusNext then
  begin
    SendMessage(Handle, WM_KEYDOWN, VK_DOWN, LongInt(0));
  end;
end;

procedure TCustomNortonLikeListView.WMKeyDown(var Message: TWMKeyDown);
var
  PDontUnSelectItem: Boolean;
  PDontSelectItem: Boolean;
begin
  if (NortonLike <> nlOff) and (Message.CharCode = VK_INSERT) then
  begin
    if Items.Count > 0 then
    begin
      SelectCurrentItem(True);
      Message.Result := 1;
    end;
  end
    else
  {if (NortonLike <> nlOff) and (Message.CharCode = VK_MULTIPLY) then
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
  if (NortonLike <> nlOff) and (Message.CharCode in [VK_LEFT, VK_RIGHT]) and
     (ViewStyle = vsReport) and
     ((GetWindowLong(Handle, GWL_STYLE) and WS_HSCROLL) = 0) then
  begin
    if Items.Count > 0 then
    begin
      // do not focus item directly to make later selecting work
      if Message.CharCode = VK_LEFT then
        SendMessage(Handle, WM_KEYDOWN, VK_HOME, LongInt(0))
      else
        SendMessage(Handle, WM_KEYDOWN, VK_END, LongInt(0));
    end;
    Message.Result := 1;
  end
    else
  if (Message.CharCode in [VK_SPACE, VK_PRIOR, VK_NEXT, VK_END, VK_HOME, VK_LEFT,
    VK_UP, VK_RIGHT, VK_DOWN, VK_SELECT]) then
  begin
    PDontSelectItem := FDontSelectItem;
    PDontUnSelectItem := FDontUnSelectItem;
    FDontSelectItem := FDontSelectItem or
      ((NortonLike <> nlOff) and
       ((KeyDataToShiftState(Message.KeyData) * [ssShift]) = []));
    FDontUnSelectItem :=
      FDontUnSelectItem or
      (NortonLike = nlOn) or
      ((NortonLike = nlKeyboard) and (not FAnyAndAllSelectedImplicitly));
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
  if Message.CharCode in [Word('+'), Word('-'), Word('*')] then
  begin
    // ugly fix to avoid Windows beeping when these keys are processed by
    // WMKeyDown instead of here (WMChar)
    Message.Result := 1;
  end
    else
  if (NortonLike <> nlOff) and (Message.CharCode = Byte(' ')) then
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
    PDontSelectItem := FDontSelectItem;
    PDontUnSelectItem := FDontUnSelectItem;
    FDontSelectItem := FDontSelectItem or (NortonLike <> nlOff);
    FDontUnSelectItem :=
      FDontUnSelectItem or
      (NortonLike = nlOn) or
      ((NortonLike = nlKeyboard) and (not FAnyAndAllSelectedImplicitly));
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
    if (NortonLike <> nlOff) then SendMessage(Handle, WM_KEYDOWN, VK_DOWN, LongInt(0));
    if not Assigned(ItemFocused) then
      ItemFocused := Items[0];
  end;
  if Assigned(ItemFocused) then
    ItemFocused.MakeVisible(False);
end;

function TCustomNortonLikeListView.EnableDragOnClick: Boolean;
begin
  Result := (not FFocusingItem);
end;

procedure TCustomNortonLikeListView.FocusItem(Item: TListItem);
var
  P: TPoint;
  PDontUnSelectItem: Boolean;
  PDontSelectItem: Boolean;
  AParent: TWinControl;
begin
  Item.MakeVisible(False);
  if Focused then
  begin
    P := Item.GetPosition;
    PDontSelectItem := FDontSelectItem;
    PDontUnSelectItem := FDontUnSelectItem;
    FDontSelectItem := True;
    FDontUnSelectItem := True;
    FFocusingItem := True;
    try
      AParent := Parent;
      P := ClientToScreen(P);
      while AParent.Parent <> nil do
        AParent := AParent.Parent;
      P := AParent.ScreenToClient(P);
      SendMessage(AParent.Handle, WM_LBUTTONDOWN, MK_LBUTTON, MAKELPARAM(P.X, P.Y));
      SendMessage(AParent.Handle, WM_LBUTTONUP, MK_LBUTTON, MAKELPARAM(P.X, P.Y));
    finally
      FFocusingItem := False;
      FDontSelectItem := PDontSelectItem;
      FDontUnSelectItem := PDontUnSelectItem;
    end;
  end;
  if ItemFocused <> Item then
    ItemFocused := Item;
end;

procedure TCustomNortonLikeListView.SelectAll(Mode: TSelectMode; Exclude: TListItem);
var
  Index: Integer;
  Item: TListItem;
begin
  BeginSelectionUpdate;
  try
    for Index := 0 to Items.Count - 1 do
    begin
      Item := Items[Index];
      if Item <> Exclude then
      begin
        case Mode of
          smAll: Item.Selected := True;
          smNone: Item.Selected := False;
          smInvert: Item.Selected := not Item.Selected;
        end;
      end;
    end;
  finally
    EndSelectionUpdate;
  end;
end;

procedure TCustomNortonLikeListView.SelectAll(Mode: TSelectMode);
begin
  SelectAll(Mode, nil);
end;

procedure TCustomNortonLikeListView.WMLButtonDown(var Message: TWMLButtonDown);
var
  PDontUnSelectItem: Boolean;
  PDontSelectItem: Boolean;
  PSelectingImplicitly: Boolean;
  SelectingImplicitly: Boolean;
  Shift: TShiftState;
  Item: TListItem;
begin
  Shift := KeysToShiftState(Message.Keys);
  PDontSelectItem := FDontSelectItem;
  PDontUnSelectItem := FDontUnSelectItem;
  PSelectingImplicitly := FSelectingImplicitly;
  FDontSelectItem := FDontSelectItem or ((NortonLike = nlOn) and ((Shift * [ssCtrl, ssShift]) = []));
  FDontUnSelectItem := FDontUnSelectItem or ((NortonLike = nlOn) and ((Shift * [ssCtrl]) = []));
  SelectingImplicitly := ((Shift * [ssCtrl, ssShift]) = []);
  if SelectingImplicitly and (NortonLike = nlKeyboard) then
  begin
    // in general, when clicking, we clear selection only after mouse button is released,
    // from within WMLButtonUp, so we know we are not starting dragging,
    // so we do not want to clear the selection.
    // on the other hand, when clicking outside of the selection,
    // we want to explicitly clear the selection, no matter what
    Item := GetItemAt(Message.XPos, Message.YPos);
    if (Item = nil) or (not Item.Selected) then
      SelectAll(smNone);
  end;
  FSelectingImplicitly := FSelectingImplicitly or SelectingImplicitly;
  FLButtonDownShiftState := Shift;
  FLButtonDownPos := Point(Message.XPos, Message.YPos);
  try
    inherited;
  finally
    FDontSelectItem := PDontSelectItem;
    FDontUnSelectItem := PDontUnSelectItem;
    FSelectingImplicitly := PSelectingImplicitly;
  end;
end;

procedure TCustomNortonLikeListView.WMRButtonDown(var Message: TWMRButtonDown);
var
  PDontUnSelectItem: Boolean;
  PDontSelectItem: Boolean;
  PSelectingImplicitly: Boolean;
  SelectingImplicitly: Boolean;
  Shift: TShiftState;
begin
  Shift := KeysToShiftState(Message.Keys);
  PDontSelectItem := FDontSelectItem;
  PDontUnSelectItem := FDontUnSelectItem;
  PSelectingImplicitly := FSelectingImplicitly;
  FDontSelectItem := FDontSelectItem or (NortonLike = nlOn);
  FDontUnSelectItem := FDontUnSelectItem or (NortonLike = nlOn);
  SelectingImplicitly := ((Shift * [ssCtrl, ssShift]) = []);
  // TODO unselect all when clicking outside of selection
  // (is not done automatically when focused item is not selected)
  FSelectingImplicitly := FSelectingImplicitly or SelectingImplicitly;
  try
    inherited;
  finally
    FDontSelectItem := PDontSelectItem;
    FDontUnSelectItem := PDontUnSelectItem;
    FSelectingImplicitly := PSelectingImplicitly;
  end;
end;

procedure TCustomNortonLikeListView.WMLButtonUp(var Message: TWMLButtonUp);
var
  SelectingImplicitly: Boolean;
  Shift: TShiftState;
begin
  // Workaround
  // For some reason Message.Keys is always 0 here,
  // so we use shift state from the LButtonDown as a workaround
  Shift := KeysToShiftState(Message.Keys);
  SelectingImplicitly :=
    ((Shift * [ssCtrl, ssShift]) = []) and
    ((FLButtonDownShiftState * [ssCtrl, ssShift]) = []);
  if SelectingImplicitly and (csClicked in ControlState) and
     (Abs(FLButtonDownPos.X - Message.XPos) <= 4) and
     (Abs(FLButtonDownPos.Y - Message.YPos) <= 4) then
  begin
    SelectAll(smNone, ItemFocused);
    // Because condition in ItemSelected is not triggered as we first select
    // the new item and then unselect the previous.
    // This probably means that we can get rid of the code in ItemSelected.
    FAnyAndAllSelectedImplicitly := True;
  end;
  inherited;
end;

function TCustomNortonLikeListView.GetMarkedFile: TListItem;
begin
  if Assigned(Selected) then Result := Selected
    else
  if Assigned(ItemFocused) and (NortonLike <> nlOff) then Result := ItemFocused
    else Result := nil;
end;

function TCustomNortonLikeListView.GetNextItem(StartItem: TListItem;
  Direction: TSearchDirection; States: TItemStates): TListItem;
var
  Start, Index, First, Last: Integer;
begin
  if not FManageSelection then
  begin
    Result := inherited GetNextItem(StartItem, Direction, States);
  end
    else
  begin
    Assert(Direction = sdAll);
    if States = [isSelected] then
    begin
      if FSelCount = 0 then
      begin
        Result := nil
      end
        else
      if (not Assigned(StartItem)) and (FFirstSelected >= 0) then
      begin
        Result := Items[FFirstSelected]
      end
        else
      begin
        if Assigned(StartItem) then
          Start := StartItem.Index
        else
          Start := -1;

        if (FFirstSelected >= 0) and (Start < FFirstSelected) then
          First := FFirstSelected
        else
          First := Start + 1;

        if FLastSelected >= 0 then
          Last := FLastSelected
        else
          Last := Items.Count - 1;

        if Start > Last then
        begin
          Result := nil;
        end
          else
        begin
          Index := First;
          while (Index <= Last) and (not (Items[Index].Selected)) do
          begin
            Inc(Index);
          end;

          if Index > Last then
          begin
            Result := nil;

            if Assigned(StartItem) and StartItem.Selected then
            begin
              Assert((FLastSelected < 0) or (FLastSelected = Start));
              FLastSelected := Start;
            end;
          end
            else
          begin
            Result := Items[Index];
            Assert(Result.Selected);

            if not Assigned(StartItem) then
            begin
              Assert((FFirstSelected < 0) or (FFirstSelected = Index));
              FFirstSelected := Index;
            end;
          end;
        end;
      end;
    end
      else
    if States = [isCut] then
    begin
      Result := inherited GetNextItem(StartItem, Direction, States);
    end
      else
    begin
      Assert(False);
      Result := nil;
    end;
  end;
end;

function TCustomNortonLikeListView.GetSelCount: Integer;
begin
  Result := FSelCount;
end;

procedure TCustomNortonLikeListView.InsertItem(Item: TListItem);
begin
  inherited;
  if Item.Selected then
    ItemSelected(Item, -1);
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
  if (SelCount > 0) or (NortonLike = nlOff) then Result := SelCount
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

procedure TCustomNortonLikeListView.LVMEditLabel(var Message: TMessage);
begin
  // explicitly requesting editing (e.g. F2),
  // so we do not care anymore when the view was focused
  FFocused := 0;
  inherited;
end;

function TCustomNortonLikeListView.CanEdit(Item: TListItem): Boolean;
var
  N: TDateTime;
  Delta: Double;
begin
  N := Now;
  Result := inherited CanEdit(Item);
  if Result and (FFocused > 0) then
  begin
    Delta := N - FFocused;
    // it takes little more than 500ms to trigger editing after click
    Result := Delta > (750.0/(24*60*60*1000));
  end;
  FFocused := 0;
end;

procedure TCustomNortonLikeListView.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;

  if Message.FocusedWnd <> FIgnoreSetFocusFrom then
    FFocused := Now;
end;

procedure TCustomNortonLikeListView.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  inherited;

  if IsEditing and (Message.CharCode = VK_TAB) then
    Message.Result := 1;
end;

end.
