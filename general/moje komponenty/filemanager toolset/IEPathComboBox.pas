unit IEPathComboBox;

interface

{$WARN UNIT_PLATFORM OFF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CommCtrl, ShellAPI, ImgList,
  CustomPathComboBox, IEDriveInfo, IEComboBox, DirView;

type
  TDirectoryToSelect = (dsCurrent, dsRoot);
  TDriveType = (dtUnknown, dtNoRootDrive, dtFloppy, dtFixed, dtRemote, dtCDROM, dtRAM);
  TDriveTypes = set of TDriveType;

const
  DefaultDriveTypes = [dtFloppy, dtFixed, dtRemote, dtCDROM, dtRAM];

type
  TIEPathComboBox = class(TCustomPathComboBox)
  private
    FDirectoryToSelect: TDirectoryToSelect;
    FDrive: TDrive;
    FDisplayStyle: TVolumeDisplayStyle;
    FDriveTypes: TDriveTypes;
    FDontNotifyPathChange: Boolean;
    FInternalWindowHandle: HWND;

    procedure SetDisplayStyle(Value: TVolumeDisplayStyle);
    procedure SetDrive(Value: TDrive);

    function DriveStored: Boolean;
    function GetFocusedDrive: Char;
    function GetItemDrive(Index: Integer): TDrive;
  protected
    procedure CreateWnd; override;
    procedure DropDown; override;
    procedure SetDriveTypes(Value: TDriveTypes); virtual;
    function GetItemImage(Index: Integer): Integer; override;
    function GetItemTextEx(Index: Integer; ForList: Boolean): string; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure PathChanged; override;
    procedure SetPath(Value: string); override;
    procedure InternalWndProc(var Msg: TMessage);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDriveIndex(Drive: TDrive; Closest: Boolean): Integer;
    procedure ResetItems;
    property FocusedDrive: Char read GetFocusedDrive;
    property ItemDrive[Index: Integer]: TDrive read GetItemDrive;

  published
    property Drive: TDrive read FDrive write SetDrive
      stored DriveStored;
    property DirectoryToSelect: TDirectoryToSelect
      read FDirectoryToSelect write FDirectoryToSelect default dsRoot;
    property DisplayStyle: TVolumeDisplayStyle read fDisplayStyle
      write SetDisplayStyle default doPrettyName;
    property DriveTypes: TDriveTypes
      read FDriveTypes write SetDriveTypes default DefaultDriveTypes;

    property DropDownFixedWidth;
    property OnCloseUp;
    property ShowFullPath;

    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses
  Math;

constructor TIEPathComboBox.Create(AOwner : TComponent);
begin
  inherited;

  UseSystemImageList := True;
  FDirectoryToSelect := dsRoot;
  FDrive := FirstFixedDrive;
  FDriveTypes := DefaultDriveTypes;
  FDontNotifyPathChange := False;
  ResetItemHeight;
  FInternalWindowHandle := Classes.AllocateHWnd(InternalWndProc);
end; {TIEPathComboBox.Create}

destructor TIEPathComboBox.Destroy;
begin
  Classes.DeallocateHWnd(FInternalWindowHandle);
  inherited;
end;

procedure TIEPathComboBox.InternalWndProc(var Msg: TMessage);
begin
  with Msg do
  begin
    if (Msg = WM_DEVICECHANGE) and
       ((wParam = {DBT_CONFIGCHANGED} $0018) or (wParam = {DBT_DEVICEARRIVAL} $8000) or
          (wParam = {DBT_DEVICEREMOVECOMPLETE} $8004)) then
    begin
      try
        DriveInfo.Load;
        ResetItems;
      except
        Application.HandleException(Self);
      end
    end;

    Result := DefWindowProc(FInternalWindowHandle, Msg, wParam, lParam);
  end;
end;

procedure TIEPathComboBox.CreateWnd;
begin
  inherited;
  ResetItems;
end;

function TIEPathComboBox.DriveStored: Boolean;
begin
  Result := (not Assigned(DirView)) and (Drive <> FirstFixedDrive);
end; { DriveStored }

procedure TIEPathComboBox.DropDown;
begin
  inherited;
end;

function TIEPathComboBox.GetDriveIndex(Drive: TDrive; Closest: Boolean): Integer;
var
  Index: Integer;
begin
  Result := -1;
  Drive := UpCase(Drive);
  for Index := Items.Count - 1 downto 0 do
  begin
    if Items[Index][1] = Drive then
    begin
      Result := Index;
      Break;
    end
      else
    if Closest and (Items[Index][1] > Drive) then
    begin
      Result := Index;
    end;
  end;
end; {TIEPathComboBox.GetDriveIndex}

function TIEPathComboBox.GetItemImage(Index: Integer): Integer;
begin
  Result := DriveInfo.GetImageIndex(ItemDrive[Index])
end;

function TIEPathComboBox.GetItemTextEx(Index: Integer; ForList: Boolean): string;
var
  ADrive: TDrive;
begin
  ADrive := ItemDrive[Index];
  case DisplayStyle of
    doPrettyName: Result := DriveInfo.GetPrettyName(ADrive);
    doDisplayName: Result := DriveInfo.GetDisplayName(ADrive);
    doLongPrettyName: Result := DriveInfo.GetLongPrettyName(ADrive);
  end;
end;

procedure TIEPathComboBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  Index: Integer;
begin
  if DroppedDown and
     (Key in [Word('A')..Word('Z'), Word('a')..Word('z')]) then
  begin
    Index := GetDriveIndex(Char(Key), False);
    if Index >= 0 then
    begin
      DroppedDown := False;
      ItemIndex := Index;
      if not FDontNotifyPathChange then PathChanged;
    end
      else Beep;
  end
    else inherited;
end; { KeyDown }

procedure TIEPathComboBox.ResetItems;
var
  Drive: TDrive;
  Index: Integer;
begin
  if Items.Count > 0 then Items.Clear;
  
  for Drive := FirstDrive to LastDrive do
  begin
    if DriveInfo[Drive].Valid and
      (TDriveType(DriveInfo[Drive].DriveType) in FDriveTypes) then
        Items.Add(Drive {+ GetDisplayName(Drive)});
  end;

  Index := GetDriveIndex(FDrive, False);
  if Index >= 0 then
  begin
    ItemIndex := Index;
  end
    else
  if Items.Count > 0 then
  begin
    Index := GetDriveIndex(FDrive, True);
    if Index < 0 then
    begin
      Index := Items.Count - 1;
    end;
    ItemIndex := Index;
    PathChanged;
  end;
end; {TIEPathComboBox.ResetItems }

procedure TIEPathComboBox.SetDrive(Value: TDrive);
var
  NewIndex: Integer;
begin
  Value := Upcase(Value);
  if Value <> FDrive then
  begin
    DriveInfo.ReadDriveStatus(Value, dsValid);
    if DriveInfo[Drive].Valid and
       (TDriveType(DriveInfo[Drive].DriveType) in FDriveTypes) then
    begin
      FDrive := Value;
      NewIndex := GetDriveIndex(Drive, False);
      if NewIndex >= 0 then
      begin
        ItemIndex := NewIndex;
        if not FDontNotifyPathChange then PathChanged;
      end;
    end;
  end;
end; {TIEPathComboBox.SetDrive}

procedure TIEPathComboBox.SetDriveTypes(Value: TDriveTypes);
begin
  if FDriveTypes <> Value then
  begin
    FDriveTypes := DriveTypes;
    ResetItems;
  end;
end; {TIEPathComboBox.SetDriveTypes}

procedure TIEPathComboBox.SetDisplayStyle(Value: TVolumeDisplayStyle);
begin
  if FDisplayStyle <> Value then
  begin
    FDisplayStyle := Value;
    ResetItems;
    Invalidate;
  end;
end; {TIEPathComboBox.SetDisplayStyle}

procedure TIEPathComboBox.PathChanged;
begin
  FDrive := FocusedDrive;
  if DirectoryToSelect = dsRoot then FPath := Format('%s:', [FDrive])
    else
  begin
    GetDir(Integer(FDrive) - Integer('A') + 1, FPath);
    FPath := ExcludeTrailingPathDelimiter(FPath);
  end;
  inherited;
end;

function TIEPathComboBox.GetFocusedDrive: Char;
begin
  Result := Upcase(ItemDrive[ItemIndex]);
end; { GetFocusedDrive }

function TIEPathComboBox.GetItemDrive(Index: Integer): TDrive;
begin
  Result := UpCase(Items[Index][1]);
end;

procedure TIEPathComboBox.SetPath(Value: string);
var
  Expanded: string;
begin
  if Value <> '' then
  begin
    Value := IncludeTrailingPathDelimiter(Value);
    if Value <> FPath then
    begin
      FPath := Value;
      Expanded := ExpandFileName(Value);
      if Pos(':', Expanded) <> 2 then
        raise Exception.CreateFmt('"%s" is not valid local path.', [Value]);
      FDontNotifyPathChange := True;
      try
        Drive := Expanded[1];
      finally
        FDontNotifyPathChange := False;
      end;
    end;
  end;
end; { SetPath }

procedure Register;
begin
  RegisterComponents('DriveDir', [TIEPathComboBox]);
end;

end.
