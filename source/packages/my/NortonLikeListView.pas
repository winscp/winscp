unit NortonLikeListView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ListViewColProperties, CommCtrl, Menus;

type
  TCustomNortonLikeListView = class;
  TSelectMode = (smAll, smNone, smInvert);
  TNortonLikeMode = (nlOn, nlOff, nlKeyboard);
  TSelectMethod = (smNoneYet, smMouse, smKeyboard);

  TCustomNortonLikeListView = class(TCustomListView)
  private
    { Private declarations }
    FColProperties: TCustomListViewColProperties;
    FDontSelectItem: Boolean;
    FDontUnSelectItem: Boolean;
    FSelCount: Integer;
    FNortonLike: TNortonLikeMode;
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
    FLastSelectMethod: TSelectMethod;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure LVMEditLabel(var Message: TMessage); message LVM_EDITLABEL;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
    function GetMarkedCount: Integer;
    function GetMarkedFile: TListItem;
    procedure ItemSelected(Item: TListItem; Index: Integer);
    procedure ItemUnselected(Item: TListItem; Index: Integer);
    procedure SelectAll(Mode: TSelectMode; Exclude: TListItem); reintroduce; overload;
  protected
    { Protected declarations }
    FClearingItems: Boolean;
    FInsertingNewUnselectedItem: Boolean;
    FUpdatingSelection: Integer;
    FNextCharToIgnore: Word;
    FHeaderHandle: HWND;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure BeginSelectionUpdate; virtual;
    procedure EndSelectionUpdate; virtual;
    function CanChangeSelection(Item: TListItem; Select: Boolean): Boolean; virtual;
    procedure ClearItems; virtual;
    procedure ItemsReordered;
    procedure Delete(Item: TListItem); override;
    function ExCanChange(Item: TListItem; Change: Integer;
      NewState, OldState: Word): Boolean; dynamic;
    procedure InsertItem(Item: TListItem); override;
    function NewColProperties: TCustomListViewColProperties; virtual; abstract;
    procedure FocusSomething(ForceMakeVisible: Boolean); virtual;
    function EnableDragOnClick: Boolean; virtual;
    function GetItemFromHItem(const Item: TLVItem): TListItem;
    function GetValid: Boolean; virtual;
    function GetSelCount: Integer; override;
    procedure DDBeforeDrag;
    function CanEdit(Item: TListItem): Boolean; override;
    function GetPopupMenu: TPopupMenu; override;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    procedure SetItemSelectedByIndex(Index: Integer; Select: Boolean);
    function GetItemSelectedByIndex(Index: Integer): Boolean;
    procedure MakeTopItem(Item: TListItem);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ClosestUnselected(Item: TListItem): TListItem;
    procedure SelectAll(Mode: TSelectMode); reintroduce; overload;
    procedure SelectCurrentItem(FocusNext: Boolean);
    function GetNextItem(StartItem: TListItem; Direction: TSearchDirection;
      States: TItemStates): TListItem;
    procedure MakeProgressVisible(Item: TListItem);
    procedure FocusItem(Item: TListItem);
    function IsItemVisible(Item: TListItem): Boolean;

    property ColProperties: TCustomListViewColProperties read FColProperties write FColProperties stored False;

    property MultiSelect default True;
    property NortonLike: TNortonLikeMode read FNortonLike write FNortonLike default nlOn;
    property MarkedCount: Integer read GetMarkedCount;
    property MarkedFile: TListItem read GetMarkedFile;
    property Valid: Boolean read GetValid;
    property LastSelectMethod: TSelectMethod read FLastSelectMethod;
  end;

implementation

uses
  PasTools, Types;

  { TCustomNortonLikeListView }

constructor TCustomNortonLikeListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSelCount := 0;
  FFirstSelected := -1;
  FLastSelected := -1;
  FClearingItems := False;
  FInsertingNewUnselectedItem := False;
  MultiSelect := True;
  FDontSelectItem := False;
  FDontUnSelectItem := False;
  FNortonLike := nlOn;
  FColProperties := NewColProperties;
  FLastDeletedItem := nil;
  FUpdatingSelection := 0;
  FFocusingItem := False;
  FLastSelectMethod := smNoneYet;
  // Since Windows Vista, native GetNextItem for selection stops working
  // once we disallow deselecting any item (see ExCanChange).
  // So we need to manage selection state ourselves
  // All supported Windows versions have the bug (last time tested on Windows 11 23H2 22631),
  // keeping the variable only as a way to tag all related code
  FManageSelection := True;
  FFocused := 0;
  FIgnoreSetFocusFrom := INVALID_HANDLE_VALUE;
  // On Windows 7 we have to force item update when it looses focus,
  // otherwise some remnants of focus rectangle remain
  // Doing the same on WinXP makes list view down from the item flicker,
  // so we avoid this there.
  // Not sure about Vista
  FForceUpdateOnItemUnfocus := IsWin7;
  FNextCharToIgnore := 0;
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
var
  Index: Integer;
begin
  if (FLastDeletedItem <> Item) and (not FClearingItems) then
  begin
    Index := Item.Index;

    if GetItemSelectedByIndex(Index) then
      ItemUnselected(Item, Index);

    if FManageSelection then
    begin
      if (FLastSelected >= 0) and (Index <= FLastSelected) then
        Dec(FLastSelected);

      if (FFirstSelected >= 0) and (Index <= FFirstSelected) then
        Dec(FFirstSelected);
    end;
  end;
  FLastDeletedItem := Item;
  inherited;
  FLastDeletedItem := nil;
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

function TCustomNortonLikeListView.ClosestUnselected(Item: TListItem): TListItem;
var
  Index: Integer;
begin
  if Assigned(Item) and (Item.Selected or ((NortonLike <> nlOff) and (SelCount = 0))) then
  begin
    Index := Item.Index + 1;
    while (Index < Items.Count) and GetItemSelectedByIndex(Index) do Inc(Index);
    if (Index >= Items.Count) or GetItemSelectedByIndex(Index) then
    begin
      Index := Item.Index - 1;
      while (Index >= 0) and GetItemSelectedByIndex(Index) do Dec(Index);
    end;
    if (Index >= 0) and (Index < Items.Count) and (not GetItemSelectedByIndex(Index)) then
      Result := Items[Index]
    else
      Result := nil;
  end
    else Result := Item;
end;

function TCustomNortonLikeListView.GetPopupMenu: TPopupMenu;
begin
  // While editing pretend that we do not have a popup menu.
  // Otherwise Ctrl+V is swallowed by the TWinControl.CNKeyDown,
  // when it finds out (TWinControl.IsMenuKey) that there's a command with Ctrl+V shortcut in the list view context menu
  // (the "paste" file action)
  if IsEditing then
  begin
    Result := nil;
  end
    else
  begin
    Result := inherited;
  end;
end;

procedure TCustomNortonLikeListView.WMNotify(var Message: TWMNotify);
var
  HDNotify: PHDNotify;
begin
  if (FHeaderHandle <> 0) and (Message.NMHdr^.hWndFrom = FHeaderHandle) then
  begin
    HDNotify := PHDNotify(Message.NMHdr);
    // Disallow resizing of "invisible" (width=0) columns.
    // (We probably get only Unicode versions of the messages here as
    // controls are created as Unicode by VCL)
    case HDNotify.Hdr.code of
      HDN_BEGINTRACKA, HDN_TRACKA, HDN_BEGINTRACKW, HDN_TRACKW:
        if not ColProperties.Visible[HDNotify.Item] then
        begin
          Message.Result := 1;
          Exit;
        end;
      // We won't get here when user tries to resize the column by mouse,
      // as that's prevented above.
      // But we get here when other methods are used
      // (the only we know about atm is Ctrl-+ shortcut)
      // We are getting this notification also when control is being setup,
      // with mask including also other fields, not just HDI_WIDTH.
      // While it does not seem to hurt to swallow even those messages,
      // not sure it's good thing to do, so we swallow width-only messages only.
      // That's why there's "= HDI_WIDTH" not "and HDI_WIDTH <> 0".
      HDN_ITEMCHANGINGA, HDN_ITEMCHANGINGW:
        if (HDNotify.PItem.Mask = HDI_WIDTH) and
           (HDNotify.PItem.cxy <> 0) and
           (not ColProperties.Visible[HDNotify.Item]) then
        begin
          Message.Result := 1;
          Exit;
        end;
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
        begin
          inherited;
        end;
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
  PLastSelectMethod: TSelectMethod;
  PDontUnSelectItem: Boolean;
  PDontSelectItem: Boolean;
begin
  FNextCharToIgnore := 0;
  if (NortonLike <> nlOff) and (Message.CharCode = VK_INSERT) then
  begin
    if Items.Count > 0 then
    begin
      PLastSelectMethod := FLastSelectMethod;
      FLastSelectMethod := smKeyboard;
      try
        SelectCurrentItem(True);
      finally
        FLastSelectMethod := PLastSelectMethod;
      end;
      Message.Result := 1;
    end;
  end
    else
  if Message.CharCode = VK_ADD then
  begin
    FNextCharToIgnore := Word('+');
    inherited;
  end
    else
  if Message.CharCode = VK_SUBTRACT then
  begin
    FNextCharToIgnore := Word('-');
    inherited;
  end
    else
  if Message.CharCode = VK_MULTIPLY then
  begin
    FNextCharToIgnore := Word('*');
    inherited;
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
  if (NortonLike <> nlOff) and (Message.CharCode = VK_SPACE) and
     ((KeyDataToShiftState(Message.KeyData) * [ssCtrl]) <> []) then
  begin
    // prevent Ctrl+Space landing in else branch below,
    // this can safely get processed by default handler as Ctrl+Space
    // toggles only focused item, not affecting others
    PLastSelectMethod := FLastSelectMethod;
    FLastSelectMethod := smKeyboard;
    try
      inherited;
    finally
      FLastSelectMethod := PLastSelectMethod;
    end;
  end
    else
  if (Message.CharCode in [VK_SPACE, VK_PRIOR, VK_NEXT, VK_END, VK_HOME, VK_LEFT,
    VK_UP, VK_RIGHT, VK_DOWN, VK_SELECT]) then
  begin
    PLastSelectMethod := FLastSelectMethod;
    PDontSelectItem := FDontSelectItem;
    PDontUnSelectItem := FDontUnSelectItem;
    FLastSelectMethod := smKeyboard;
    FDontSelectItem := FDontSelectItem or
      ((NortonLike <> nlOff) and
       ((KeyDataToShiftState(Message.KeyData) * [ssShift]) = []));
    // Note that Space (selecting toggling) is processed by default handler for WM_CHAR,
    // otherwise the below condition would prevent unselection
    FDontUnSelectItem :=
      FDontUnSelectItem or
      (NortonLike = nlOn) or
      ((NortonLike = nlKeyboard) and (not FAnyAndAllSelectedImplicitly));
    try
      inherited;
    finally
      FDontSelectItem := PDontSelectItem;
      FDontUnSelectItem := PDontUnSelectItem;
      FLastSelectMethod := PLastSelectMethod;
    end;
  end
    else inherited;
end;

procedure TCustomNortonLikeListView.WMSysCommand(var Message: TWMSysCommand);
begin
  // Ugly workaround to avoid Windows beeping when Alt+Grey +/- are pressed
  // (for (Us)Select File with Same Ext commands)
  // The same for Alt+Enter (for Properties)
  if (Message.CmdType = SC_KEYMENU) and
     ((Message.Key = Word('+')) or (Message.Key = Word('-')) or (Message.Key = VK_RETURN)) then
  begin
    Message.Result := 1;
  end
    else inherited;
end;

procedure TCustomNortonLikeListView.WMChar(var Message: TWMChar);
var
  PLastSelectMethod: TSelectMethod;
  PDontUnSelectItem: Boolean;
  PDontSelectItem: Boolean;
begin
  if Message.CharCode = FNextCharToIgnore then
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
      // If not handled in TCustomScpExplorerForm::DirViewKeyPress
      if not DoKeyPress(Message) then
      begin
        if Assigned(ItemFocused) then
          ItemFocused.Selected := not ItemFocused.Selected;
      end;
    end
      else inherited;
  end
    else
  begin
    PLastSelectMethod := FLastSelectMethod;
    PDontSelectItem := FDontSelectItem;
    PDontUnSelectItem := FDontUnSelectItem;
    FDontSelectItem := FDontSelectItem or (NortonLike <> nlOff);
    FLastSelectMethod := smKeyboard;
    FDontUnSelectItem :=
      FDontUnSelectItem or
      (NortonLike = nlOn) or
      ((NortonLike = nlKeyboard) and (not FAnyAndAllSelectedImplicitly));
    try
      inherited;
    finally
      FLastSelectMethod := PLastSelectMethod;
      FDontSelectItem := PDontSelectItem;
      FDontUnSelectItem := PDontUnSelectItem;
    end;
  end;
  FNextCharToIgnore := 0;
end;

procedure TCustomNortonLikeListView.FocusSomething(ForceMakeVisible: Boolean);
var
  MakeVisible: Boolean;
begin
  MakeVisible := ForceMakeVisible;
  if Valid and (Items.Count > 0) and not Assigned(ItemFocused) then
  begin
    MakeVisible := True;

    if (NortonLike <> nlOff) then
    begin
      SendMessage(Handle, WM_KEYDOWN, VK_DOWN, LongInt(0));
    end;

    if not Assigned(ItemFocused) then
    begin
      ItemFocused := Items[0];
    end;
  end;
  if MakeVisible and Assigned(ItemFocused) then
  begin
    ItemFocused.MakeVisible(False);
  end;
end;

function TCustomNortonLikeListView.EnableDragOnClick: Boolean;
begin
  Result := (not FFocusingItem);
end;

procedure TCustomNortonLikeListView.FocusItem(Item: TListItem);
var
  P: TPoint;
  PLastSelectMethod: TSelectMethod;
  PDontUnSelectItem: Boolean;
  PDontSelectItem: Boolean;
  WParam: UINT_PTR;
  LParam: INT_PTR;
begin
  // This whole is replacement for mere ItemFocused := Item
  // because that does not reset some internal focused pointer,
  // causing subsequent Shift-Click selects range from the first item,
  // not from focused item.
  Item.MakeVisible(False);
  Assert(Focused);
  if Focused then
  begin
    P := Item.GetPosition;
    PLastSelectMethod := FLastSelectMethod;
    PDontSelectItem := FDontSelectItem;
    PDontUnSelectItem := FDontUnSelectItem;
    FLastSelectMethod := smNoneYet;
    FDontSelectItem := True;
    FDontUnSelectItem := True;
    FFocusingItem := True;
    try
      // HACK
      // WM_LBUTTONDOWN enters loop, waiting for WM_LBUTTONUP,
      // so we have to post it in advance to break the loop immediately

      // Without MK_CONTROL, if there are more items selected,
      // they won't get unselected on subsequent focus change
      // (with explorer-style selection).
      // And it also makes the click the least obtrusive, affecting the focused
      // file only.
      WParam := MK_LBUTTON or MK_CONTROL;
      LParam := MAKELPARAM(P.X, P.Y);
      PostMessage(Handle, WM_LBUTTONUP, WParam, LParam);
      SendMessage(Handle, WM_LBUTTONDOWN, WParam, LParam);
    finally
      FFocusingItem := False;
      FLastSelectMethod := PLastSelectMethod;
      FDontSelectItem := PDontSelectItem;
      FDontUnSelectItem := PDontUnSelectItem;
    end;
  end;
  if ItemFocused <> Item then
    ItemFocused := Item;
end;

// TListItem.Selected needs an index, which is expensively looked up.
// If we know it already, avoid that loop up.
procedure TCustomNortonLikeListView.SetItemSelectedByIndex(Index: Integer; Select: Boolean);
var
  State: Integer;
begin
  if Select then State := LVIS_SELECTED
    else State := 0;
  ListView_SetItemState(Handle, Index, State, LVIS_SELECTED);
end;

function TCustomNortonLikeListView.GetItemSelectedByIndex(Index: Integer): Boolean;
begin
  Result := (ListView_GetItemState(Handle, Index, LVIS_SELECTED) and LVIS_SELECTED) <> 0;
end;

procedure TCustomNortonLikeListView.SelectAll(Mode: TSelectMode; Exclude: TListItem);
var
  Index: Integer;
  Item: TListItem;
  NewState: Boolean;
begin
  BeginSelectionUpdate;
  try
    // Setting/Querying selected state is expensive.
    // This optimization is important for call from TCustomNortonLikeListView.WMLButtonUp in nlKeyboard mode.
    if (Mode = smNone) and
       // If there are too many, plain iteration is more effective then using GetNextItem
       // (though that can be optimized too, by passing index in and out instead of an item pointer)
       (FSelCount < Items.Count div 4) then
    begin
      Item := GetNextItem(nil, sdAll, [isSelected]);
      while Assigned(Item) do
      begin
        if Item <> Exclude then
          Item.Selected := False;
        Item := GetNextItem(Item, sdAll, [isSelected]);
      end;
    end
      else
    begin
      for Index := 0 to Items.Count - 1 do
      begin
        Item := Items[Index];
        if Item <> Exclude then
        begin
          case Mode of
            smAll: NewState := True;
            smNone: NewState := False;
            smInvert: NewState := not GetItemSelectedByIndex(Index);
              else
            begin
              Assert(False);
              NewState := False;
            end;
          end;

          SetItemSelectedByIndex(Index, NewState);
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
  PLastSelectMethod: TSelectMethod;
  PDontUnSelectItem: Boolean;
  PDontSelectItem: Boolean;
  PSelectingImplicitly: Boolean;
  SelectingImplicitly: Boolean;
  Shift: TShiftState;
  Item: TListItem;
begin
  Shift := KeysToShiftState(Message.Keys);
  PLastSelectMethod := FLastSelectMethod;
  PDontSelectItem := FDontSelectItem;
  PDontUnSelectItem := FDontUnSelectItem;
  PSelectingImplicitly := FSelectingImplicitly;
  FLastSelectMethod := smMouse;
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
    FLastSelectMethod := PLastSelectMethod;
    FDontSelectItem := PDontSelectItem;
    FDontUnSelectItem := PDontUnSelectItem;
    FSelectingImplicitly := PSelectingImplicitly;
  end;
end;

procedure TCustomNortonLikeListView.WMRButtonDown(var Message: TWMRButtonDown);
var
  PLastSelectMethod: TSelectMethod;
  PDontUnSelectItem: Boolean;
  PDontSelectItem: Boolean;
  PSelectingImplicitly: Boolean;
  SelectingImplicitly: Boolean;
  Shift: TShiftState;
begin
  Shift := KeysToShiftState(Message.Keys);
  PLastSelectMethod := FLastSelectMethod;
  PDontSelectItem := FDontSelectItem;
  PDontUnSelectItem := FDontUnSelectItem;
  PSelectingImplicitly := FSelectingImplicitly;
  FLastSelectMethod := smMouse;
  FDontSelectItem := FDontSelectItem or (NortonLike = nlOn);
  FDontUnSelectItem := FDontUnSelectItem or (NortonLike = nlOn);
  SelectingImplicitly := ((Shift * [ssCtrl, ssShift]) = []);
  // TODO unselect all when clicking outside of selection
  // (is not done automatically when focused item is not selected)
  FSelectingImplicitly := FSelectingImplicitly or SelectingImplicitly;
  try
    inherited;
  finally
    FLastSelectMethod := PLastSelectMethod;
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
          while (Index <= Last) and (not GetItemSelectedByIndex(Index)) do
          begin
            Inc(Index);
          end;

          if Index > Last then
          begin
            Result := nil;

            if (Start >= 0) and GetItemSelectedByIndex(Start) then
            begin
              Assert((FLastSelected < 0) or (FLastSelected = Start));
              FLastSelected := Start;
            end;
          end
            else
          begin
            Result := Items[Index];
            Assert(GetItemSelectedByIndex(Index));

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
    if States = [] then
    begin
      if Assigned(StartItem) then
        Start := StartItem.Index
      else
        Start := -1;
      Inc(Start);
      if Start < Items.Count then
        Result := Items[Start]
      else
        Result := nil;
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

  if (not FInsertingNewUnselectedItem) and // Optimization to avoid expensive Item.Selected
     Item.Selected then
  begin
    ItemSelected(Item, -1);
  end;
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

procedure TCustomNortonLikeListView.WMNCDestroy(var Message: TWMNCDestroy);
begin
  // VCLCOPY
  FHeaderHandle := 0;
  inherited;
end;

procedure TCustomNortonLikeListView.CreateWnd;
begin
  try
    Assert(ColProperties <> nil);
    inherited;
    // VCL gets the handle from WM_CREATE
    FHeaderHandle := ListView_GetHeader(Handle);

    ColProperties.ListViewWndCreated;
  finally
  end;
end;

procedure TCustomNortonLikeListView.DestroyWnd;
begin
  ColProperties.ListViewWndDestroying;
  try
    inherited;
  finally
    ColProperties.ListViewWndDestroyed;
  end;
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
    Result := Delta > (750.0/MSecsPerDay);
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

procedure TCustomNortonLikeListView.MakeTopItem(Item: TListItem);
begin
  Scroll(0, Item.Top - TopItem.Top);
end;

procedure TCustomNortonLikeListView.MakeProgressVisible(Item: TListItem);
var
  DisplayRect: TRect;
begin
  if ViewStyle = vsReport then
  begin
    DisplayRect := Item.DisplayRect(drBounds);

    if DisplayRect.Bottom > ClientHeight then
    begin
      MakeTopItem(Item);
    end;
  end;

  Item.MakeVisible(False);
end;

function TCustomNortonLikeListView.IsItemVisible(Item: TListItem): Boolean;
begin
  Result := (ListView_IsItemVisible(Handle, Item.Index) <> 0);
end;

procedure TCustomNortonLikeListView.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  if M <> D then
  begin
    // When font is scaled, while the control is being re-created, previous font is restored once
    // read from the persistence data in TCustomListView.CreateWnd.
    // Requiring handle, makes sure the re-create phase is closed.
    // We could limit impact by checking ControlHasRecreationPersistenceData,
    // but for now, we actually prefer larger impact to test this change better.
    HandleNeeded;
  end;
  inherited;
  ColProperties.ChangeScale(M, D);
end;

end.
