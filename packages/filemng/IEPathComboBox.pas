unit IEPathComboBox;

interface

{$WARN UNIT_PLATFORM OFF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CommCtrl, ShellAPI, ImgList, ShlObj, 
  CustomPathComboBox, IEDriveInfo, IEComboBox, DirView;

type
  TDirectoryToSelect = (dsCurrent, dsRoot, dsLast);
  TDriveType = (dtUnknown, dtNoRootDrive, dtFloppy, dtFixed, dtRemote, dtCDROM, dtRAM);
  TDriveTypes = set of TDriveType;
  TSpecialFolder = (sfDesktop, sfMyDocuments);
  TDriveLetter = 'A'..'Z';
  TSpecialFolders = set of TSpecialFolder;

const
  DefaultDriveTypes = [dtFloppy, dtFixed, dtRemote, dtCDROM, dtRAM];
  DefaultSpecialFolders = [sfDesktop, sfMyDocuments];

type
  TFolderInfo = record
    Valid: Boolean;
    Path: string;
    DisplayName: string;
    ImageIndex: Integer;
    Text: string;
    PIDL: PItemIDList;
  end;

type
  TIEPathComboBox = class(TCustomPathComboBox)
  private
    FDirectoryToSelect: TDirectoryToSelect;
    FDrive: TDrive;
    FDisplayStyle: TVolumeDisplayStyle;
    FDriveTypes: TDriveTypes;
    FDontNotifyPathChange: Boolean;
    FShowSpecialFolders: TSpecialFolders;
    FSpecialFolders: array[TSpecialFolder] of TFolderInfo;
    FLastPath: array[TDriveLetter] of string;

    procedure SetDisplayStyle(Value: TVolumeDisplayStyle);
    procedure SetDrive(Value: TDrive);

    function DriveStored: Boolean;
    function GetFocusedDrive: Char;
    function GetItemDrive(Index: Integer): TDrive;
    procedure SetShowSpecialFolders(Value: TSpecialFolders);

  protected
    procedure CreateWnd; override;
    procedure DropDown; override;
    procedure SetDriveTypes(Value: TDriveTypes); virtual;
    function GetItemImage(Index: Integer): Integer; override;
    function GetItemTextEx(Index: Integer; ForList: Boolean): string; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure PathChanged; override;
    procedure SetPath(Value: string); override;
    function SpecialItems: Integer;
    procedure LoadFolderInfo(var Info: TFolderInfo);
    function GetItemSpecialFolder(Index: Integer): TSpecialFolder;
    property ItemDrive[Index: Integer]: TDrive read GetItemDrive;

  public
    constructor Create(AOwner: TComponent); override;
    function GetDriveIndex(Drive: TDrive; Closest: Boolean): Integer;
    procedure ResetItems;
    property FocusedDrive: Char read GetFocusedDrive;

  published
    property Drive: TDrive read FDrive write SetDrive
      stored DriveStored;
    property DirectoryToSelect: TDirectoryToSelect
      read FDirectoryToSelect write FDirectoryToSelect default dsRoot;
    property DisplayStyle: TVolumeDisplayStyle read fDisplayStyle
      write SetDisplayStyle default doPrettyName;
    property DriveTypes: TDriveTypes
      read FDriveTypes write SetDriveTypes default DefaultDriveTypes;
    property ShowSpecialFolders: TSpecialFolders
      read FShowSpecialFolders write SetShowSpecialFolders default DefaultSpecialFolders;

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

resourcestring
  SSpecialFolderMyDocuments = 'My documents';
  SSpecialFolderDesktop = 'Desktop';

implementation

uses
  Math, CustomDirView, BaseUtils;

constructor TIEPathComboBox.Create(AOwner : TComponent);
var
  InitPath: string;
  F: TSpecialFolder;
  D: Char;
begin
  inherited;

  UseSystemImageList := True;
  FDirectoryToSelect := dsRoot;
  for D := Low(FLastPath) to High(FLastPath) do
    FLastPath[D] := '';
  FShowSpecialFolders := DefaultSpecialFolders;
  for F := Low(FSpecialFolders) to High(FSpecialFolders) do
  begin
    FSpecialFolders[F].Valid := False;
    FSpecialFolders[F].PIDL := nil; 
  end;
  SpecialFolderLocation(CSIDL_PERSONAL, FSpecialFolders[sfMyDocuments].Path,
    FSpecialFolders[sfMyDocuments].PIDL);
  FSpecialFolders[sfMyDocuments].Text := SSpecialFolderMyDocuments;
  SpecialFolderLocation(CSIDL_DESKTOP, FSpecialFolders[sfDesktop].Path,
    FSpecialFolders[sfDesktop].PIDL);
  FSpecialFolders[sfDesktop].Text := SSpecialFolderDesktop;

  InitPath := GetCurrentDir;
  if IsUNCPath(InitPath) then
  begin
    InitPath := UserDocumentDirectory;
    if IsUNCPath(InitPath) then
    begin
      InitPath := AnyValidPath;
    end;
  end;

  InitPath := ExtractFileDrive(InitPath);
  if (Length(InitPath) <> 2) or (InitPath[2] <> ':') then FDrive := FirstFixedDrive
    else FDrive := InitPath[1];

  FDriveTypes := DefaultDriveTypes;
  FDontNotifyPathChange := False;
  ResetItemHeight;
end; {TIEPathComboBox.Create}

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

function TIEPathComboBox.SpecialItems: Integer;
begin
  Result := 0;
  if sfDesktop in ShowSpecialFolders then Inc(Result);
  if sfMyDocuments in ShowSpecialFolders then Inc(Result);
end;

function TIEPathComboBox.GetItemSpecialFolder(Index: Integer): TSpecialFolder;
begin
  if (Index = 0) and (sfDesktop in ShowSpecialFolders) then Result := sfDesktop
    else
  if ((Index = 0) or (Index = 1)) and (sfMyDocuments in ShowSpecialFolders) then Result := sfMyDocuments
    else
  begin
    Assert(False);
    Result := sfDesktop; // to satisfy compiler
  end;
end;

procedure TIEPathComboBox.LoadFolderInfo(var Info: TFolderInfo);
var
  FileInfo: TShFileInfo;
  Path: PChar;
  Flags: Word;
begin
  if not Info.Valid then
  begin
    if Info.PIDL <> nil then
    begin
      Path := PChar(Info.PIDL);
      Flags := SHGFI_PIDL;
    end
      else
    begin
      Path := PChar(Info.Path);
      Flags := 0;
    end;

    SHGetFileInfo(Path, 0, FileInfo, SizeOf(FileInfo),
      SHGFI_DISPLAYNAME or SHGFI_SYSICONINDEX or SHGFI_SMALLICON or Flags);
    Info.ImageIndex := FileInfo.iIcon;
    Info.DisplayName := FileInfo.szDisplayName;
  end;
end;

procedure TIEPathComboBox.SetShowSpecialFolders(Value: TSpecialFolders);
begin
  if ShowSpecialFolders <> Value then
  begin
    FShowSpecialFolders := Value;
    ResetItems;
  end;
end;

function TIEPathComboBox.GetDriveIndex(Drive: TDrive; Closest: Boolean): Integer;
var
  Index: Integer;
begin
  Result := -1;
  Drive := UpCase(Drive);
  for Index := Items.Count - 1 downto SpecialItems do
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
var
  SpecialFolder: TSpecialFolder;
begin
  if Index < SpecialItems then
  begin
    SpecialFolder := GetItemSpecialFolder(Index);
    LoadFolderInfo(FSpecialFolders[SpecialFolder]);
    Result := FSpecialFolders[SpecialFolder].ImageIndex;
  end
    else
  begin
    Result := DriveInfo.GetImageIndex(ItemDrive[Index])
  end;
end;

function TIEPathComboBox.GetItemTextEx(Index: Integer; ForList: Boolean): string;
var
  ADrive: TDrive;
  SpecialFolder: TSpecialFolder;
begin
  if Index < SpecialItems then
  begin
    SpecialFolder := GetItemSpecialFolder(Index);
    LoadFolderInfo(FSpecialFolders[SpecialFolder]);
    Result := FSpecialFolders[SpecialFolder].Text;
  end
    else
  begin
    ADrive := ItemDrive[Index];
    case DisplayStyle of
      doPrettyName: Result := DriveInfo.GetPrettyName(ADrive);
      doDisplayName: Result := DriveInfo.GetDisplayName(ADrive);
      doLongPrettyName: Result := DriveInfo.GetLongPrettyName(ADrive);
    end;
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

  for Index := 0 to SpecialItems - 1 do Items.Add('');

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
var
  Index: Integer;
  SpecialFolder: TSpecialFolder;
begin
  if ItemIndex < SpecialItems then
  begin
    SpecialFolder := GetItemSpecialFolder(ItemIndex);
    FPath := FSpecialFolders[SpecialFolder].Path;
    FDrive := Upcase(FPath[1]);
    Index := GetDriveIndex(FDrive, False);
    if Index >= 0 then
    begin
      ItemIndex := Index;
    end
  end
    else
  begin
    FDrive := FocusedDrive;
    case DirectoryToSelect of
      dsRoot:
        FPath := Format('%s:', [FDrive]);

      dsCurrent: 
        begin
          GetDir(Integer(FDrive) - Integer('A') + 1, FPath);
          FPath := ExcludeTrailingPathDelimiter(FPath);
        end;

      dsLast:
        if FLastPath[FDrive] <> '' then FPath := FLastPath[FDrive]
          else
        begin
          GetDir(Integer(FDrive) - Integer('A') + 1, FPath);
          FPath := ExcludeTrailingPathDelimiter(FPath);
        end;
    end;

    FLastPath[FDrive] := FPath;
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

      FLastPath[Drive] := Expanded;
    end;
  end;
end; { SetPath }

procedure Register;
begin
  RegisterComponents('DriveDir', [TIEPathComboBox]);
end;

end.
