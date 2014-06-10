unit DriveView;

{==================================================================
 Component TDriveView / Version 2.6, January 2000
 ==================================================================


    Description:
    ============
    Displays the the directory structure of all drives as treeview
    with shell icons. Complete drag&Drop support for files and
    directories.


    Author:
    =======
    (c) Ingo Eckel 1998, 1999
    Sodener Weg 38
    65812 Bad Soden
    Germany

    Modifications (for WinSCP):
    ===========================
    (c) Martin Prikryl 2004

    V2.6:
    - Shows "shared"-symbol with directories
    - Delphi5 compatible

    For detailed documentation and history see TDriveView.htm.


{==================================================================}
interface

{ Define ENHVALIDATE to scan all existing directories on a detected filesystem change:}
{.$DEFINE ENHVALIDATE}

{Required compiler options for TDriveView:}
{$A+,B-,X+,H+,P+}
{$WARN SYMBOL_PLATFORM OFF}

uses
  Windows, Messages, SysUtils, Classes,  Graphics, Controls, Forms, ComObj,
  Dialogs, ComCtrls, ShellApi, CommCtrl, ExtCtrls, ActiveX,  ShlObj,
  DirView, ShellDialogs, DragDrop, DragDropFilesEx, FileChanges, FileOperator,
  DiscMon, IEDriveInfo, IEListView, PIDL, BaseUtils, ListExt, CustomDirView,
  CustomDriveView;

{$I ResStrings.pas}

const
{$IFNDEF NO_THREADS}
  msThreadChangeDelay = 50;
{$ENDIF}
  CInvalidSize = $FFFFFFFF;

  ErrorNodeNA = '%s: Node not assigned';

  {Flags used by TDriveView.RefreshRootNodes:}
  dvdsFloppy          = 8;  {Include floppy drives}
  dvdsRereadAllways   = 16; {Refresh drivestatus in any case}

type
  ECreateShortCut  = class(Exception);
  EInvalidDirName  = class(Exception);
  EInvalidPath     = class(Exception);
  ENodeNotAssigned = class(Exception);

  TDriveStatus = record
    Scanned: Boolean;          {Drive allready scanned?}
    Verified: Boolean;         {Drive completly scanned?}
    RootNode: TTreeNode;       {Rootnode to drive}
    RootNodeIndex: Integer;
    {$IFNDEF NO_THREADS}
    DiscMonitor: TDiscMonitor; {Monitor thread}
    {$ENDIF}
    ChangeTimer: TTimer;       {Change timer for the monitor thread}
    DefaultDir: string;        {Current directory}
  end;

  TScanDirInfo = record
    SearchNewDirs: Boolean;
    StartNode: TTreeNode;
    DriveType: Integer;
  end;

  PScanDirInfo = ^TScanDirInfo;

  TDriveViewScanDirEvent = procedure(Sender: TObject; Node: TTreeNode;
    var DoScanDir: Boolean) of object;
  TDriveViewDiskChangeEvent = procedure(Sender: TObject; Drive: TDrive) of object;

  TDriveView = class;

  TNodeData = class
  private
    FDirName: string;
    FShortName: string;
    FAttr: Integer;
    FScanned: Boolean;
    FData: Pointer;
    FExpanded: Boolean;
    FDirSize: Cardinal;
    FIsRecycleBin: Boolean;
    FIconEmpty: Boolean;

  public
    shAttr: ULONG;
    PIDL: PItemIDList;
    ShellFolder: IShellFolder;

    constructor Create;
    destructor Destroy; override;

    property DirName: string read FDirName write FDirName;
    property ShortName: string read FShortName write FShortName;
    property Attr: Integer read FAttr write FAttr;
    property Scanned: Boolean read FScanned write FScanned;
    property Data: Pointer read FData write FData;
    property Expanded: Boolean read FExpanded write FExpanded;
    property DirSize: Cardinal read FDirSize write FDirSize;
    property IsRecycleBin: Boolean read FIsRecycleBin;
    property IconEmpty: Boolean read FIconEmpty write FIconEmpty;
  end;

  TDriveTreeNode = class(TTreeNode)
    procedure Assign(Source: TPersistent); override;
  end;

  TDriveView = class(TCustomDriveView)
  private
    DriveStatus: array[FirstDrive .. LastDrive] of TDriveStatus;

    FConfirmDelete: Boolean;
    FConfirmOverwrite: Boolean;
    FWatchDirectory: Boolean;
    FDirectory: string;
    FFullDriveScan: Boolean;
    FShowDirSize: Boolean;
    FShowVolLabel: Boolean;
    FVolDisplayStyle: TVolumeDisplayStyle;
    FShowAnimation: Boolean;
    FChangeFlag: Boolean;
    FLastDir: string;
    FValidateFlag: Boolean;
    FCreating: Boolean;
    FForceRename: Boolean;
    FRenameNode: TTreeNode;
    FLastRenameName: string;
    FInternalWindowHandle: HWND;
    FPrevSelected: TTreeNode;
    FPrevSelectedIndex: Integer;
    FChangeTimerSuspended: Integer;

    FDesktop: IShellFolder;
    FWorkPlace: IShellFolder;

    {Additional events:}
    FOnStartScan: TNotifyEvent;
    FOnEndScan: TNotifyEvent;
    FOnScanDir: TDriveViewScanDirEvent;
    FOnDiskChange: TDriveViewDiskChangeEvent;
    FOnInsertedDiskChange: TDriveViewDiskChangeEvent;
    FOnChangeDetected: TDriveViewDiskChangeEvent;
    FOnChangeInvalid: TDriveViewDiskChangeEvent;
    FOnDisplayContextMenu: TNotifyEvent;
    FOnRefreshDrives: TNotifyEvent;

    {used components:}
    FDirView: TDirView;
    FFileOperator: TFileOperator;

    FChangeInterval: Cardinal;

    FNoCheckDrives: string;
    FCompressedColor: TColor;
    FFileNameDisplay: TFileNameDisplay;

    {Drag&drop:}
    FLastPathCut: string;

    {Drag&drop helper functions:}
    procedure SignalDirDelete(Sender: TObject; Files: TStringList);

    function CheckForSubDirs(Path: string): Boolean;
    function ReadSubDirs(Node: TTreeNode; DriveType: Integer): Boolean;

    {Callback-functions used by iteratesubtree:}
    function CallBackValidateDir(var Node: TTreeNode; Data: Pointer): Boolean;
    function CallBackSaveNodeState(var Node: TTreeNode; Data: Pointer): Boolean;
    function CallBackRestoreNodeState(var Node: TTreeNode; Data: Pointer): Boolean;
    function CallBackDisplayName(var Node: TTreeNode; Data: Pointer): Boolean;
    function CallBackSetDirSize(var Node: TTreeNode; Data: Pointer): Boolean;
    function CallBackExpandLevel(var Node: TTreeNode; Data: Pointer): Boolean;

    { Notification procedures used by component TDiscMonitor: }
    procedure ChangeDetected(Sender: TObject; const Directory: string;
      var SubdirsChanged: Boolean);
    procedure ChangeInvalid(Sender: TObject; const Directory: string; const ErrorStr: string);

    {Notification procedure used by component TTimer:}
    procedure ChangeTimerOnTimer(Sender: TObject);

  protected
    procedure SetSelected(Node: TTreeNode);
    procedure SetFullDriveScan(DoFullDriveScan: Boolean);
    procedure SetWatchDirectory(Value: Boolean);
    procedure SetShowDirSize(ShowIt: Boolean);
    procedure SetShowVolLabel(ShowIt: Boolean);
    procedure SetVolDisplayStyle(DoStyle: TVolumeDisplayStyle);
    procedure SetDirView(Value: TDirView);
    procedure SetChangeInterval(Value: Cardinal);
    procedure SetNoCheckDrives(Value: string);
    procedure SetCompressedColor(Value: TColor);
    procedure SetFileNameDisplay(Value: TFileNameDisplay);
    procedure SetDirectory(Value: string); override;
    procedure SetDrive(Drive: TDrive);
    function  GetDrive: TDrive;
    procedure GetNodeShellAttr(ParentFolder: IShellFolder; NodeData: TNodeData;
      Path: string; ContentMask: Boolean = True);
    function  DoScanDir(FromNode: TTreeNode): Boolean; virtual;
    function  AddChildNode(ParentNode: TTreeNode; SRec: TSearchRec): TTreeNode; virtual;
{$IFNDEF NO_THREADS}
    procedure CreateWatchThread(Drive: TDrive); virtual;
{$ENDIF}
    procedure InternalWndProc(var Msg: TMessage);

    function DirAttrMask: Integer;

    procedure ValidateDirectoryEx(Node: TTreeNode; Recurse: TRecursiveScan;
      NewDirs: Boolean); override;
    procedure ValidateDirectoryEasy(Node: TTreeNode);
    procedure RebuildTree; override;

    procedure SetLastPathCut(Path: string);
    function GetCanUndoCopyMove: Boolean; virtual;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Edit(const Item: TTVItem); override;

    procedure WMUserRename(var Message: TMessage); message WM_USER_RENAME;

    function GetCustomDirView: TCustomDirView; override;
    procedure SetCustomDirView(Value: TCustomDirView); override;

    function NodePath(Node: TTreeNode): string; override;
    function NodeIsRecycleBin(Node: TTreeNode): Boolean; override;
    function NodePathExists(Node: TTreeNode): Boolean; override;
    function NodeColor(Node: TTreeNode): TColor; override;
    function FindPathNode(Path: string): TTreeNode; override;
    function CreateNode: TTreeNode; override;

    function DDSourceEffects: TDropEffectSet; override;
    procedure DDChooseEffect(KeyState: Integer; var Effect: Integer); override;
    function DragCompleteFileList: Boolean; override;
    function DDExecute: TDragResult; override;

  public
    property Images;
    property StateImages;
    property Items stored False;
    property Selected Write SetSelected stored False;

    property WorkPlace: IShellFolder read FWorkPlace;

    property DragImageList: TDragImageList read FDragImageList;

    property Drive: TDrive read GetDrive write SetDrive stored False;

    property DragDrive: TDrive read FDragDrive;
    property CanUndoCopyMove: Boolean read GetCanUndoCopyMove;
    property DDFileOperator: TFileOperator read FFileOperator;
    property LastPathCut: string read FLastPathCut write SetLastPathCut;

    function UndoCopyMove: Boolean; dynamic;
    procedure EmptyClipboard; dynamic;
    function CopyToClipBoard(Node: TTreeNode): Boolean; dynamic;
    function CutToClipBoard(Node: TTreeNode): Boolean; dynamic;
    function CanPasteFromClipBoard: Boolean; dynamic;
    function PasteFromClipBoard(TargetPath: string = ''): Boolean; dynamic;
    procedure PerformDragDropFileOperation(Node: TTreeNode; Effect: Integer); override;

    {Drive handling:}
    function GetDriveStatus(Drive: TDrive): TDriveStatus;
    function GetDriveTypetoNode(Node: TTreeNode): Integer;  {Returns DRIVE_CDROM etc..}
    function GetDriveType(Drive: TDrive): Integer;           {Returns DRIVE_CDROM etc..}
    function GetDriveToNode(Node: TTreeNode): Char;
    function GetDriveText(Drive: TDrive): string;
    procedure ScanDrive(Drive: TDrive);
    procedure RefreshRootNodes(ScanDirectory: Boolean; dsFlags: Integer);
    function GetValidDrivesStr: string;
    procedure RefreshDirSize(Node: TTreeNode);
    procedure RefreshDriveDirSize(Drive: TDrive);

    {Node handling:}
    procedure SetImageIndex(Node: TTreeNode); virtual;
    function FindNodeToPath(Path: string): TTreeNode;
    function NodeVerified(Node: TTreeNode): Boolean;
    function NodeAttr(Node: TTreeNode): Integer;
    function RootNode(Node: TTreeNode): TTreeNode;
    function GetDirName(Node: TTreeNode): string;
    function GetDirSize(Node: TTreeNode): Cardinal; virtual;
    procedure SetDirSize(Node: TTreeNode); virtual;
    function GetDisplayName(Node: TTreeNode): string;
    function NodeUpdateAble(Node: TTreeNode): Boolean; virtual;
    procedure ExpandLevel(Node: TTreeNode; Level: Integer); virtual;
    function NodePathName(Node: TTreeNode): string; override;

    function GetFQPIDL(Node: TTreeNode): PItemIDList;

    function GetSubTreeSize(Node: TTreeNode): Integer; dynamic;

    {Directory update:}
    function  CreateDirectory(ParentNode: TTreeNode; NewName: string): TTreeNode; dynamic;
    function  DeleteDirectory(Node: TTreeNode; AllowUndo: Boolean): Boolean; dynamic;

    procedure DeleteSubNodes(Node: TTreeNode); dynamic;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {Save and restore the subnodes expanded state:}
    procedure SaveNodesState(Node: TTreeNode);
    procedure RestoreNodesState(Node: TTreeNode);

    {Menu-handling:}
    procedure DisplayContextMenu(Node: TTreeNode; Point: TPoint); override;

    procedure DisplayPropertiesMenu(Node: TTreeNode); override;

{$IFNDEF NO_THREADS}
    {Watchthread handling:}
    procedure StartWatchThread; virtual;
    procedure StopWatchThread; virtual;
    procedure SuspendChangeTimer;
    procedure ResumeChangeTimer;
    procedure TerminateWatchThread(Drive: TDrive); virtual;
    procedure StartAllWatchThreads; virtual;
    procedure StopAllWatchThreads; virtual;
    function WatchThreadActive: Boolean; overload;
    function WatchThreadActive(Drive: TDrive): Boolean; overload;
    function NodeWatched(Node: TTreeNode): Boolean; virtual;
{$ENDIF}
    procedure ValidateCurrentDirectoryIfNotMonitoring;

    (* Modified Events: *)
    procedure GetImageIndex(Node: TTreeNode); override;
    function CanEdit(Node: TTreeNode): Boolean; override;
    function CanChange(Node: TTreeNode): Boolean; override;
    function CanExpand(Node: TTreeNode): Boolean; override;
    procedure Delete(Node: TTreeNode); override;
    procedure Loaded; override;
    procedure KeyPress(var Key: Char); override;
    procedure Change(Node: TTreeNode); override;

  published
    {Additional properties:}

    {Current selected directory:}
    property Directory;

    {Confirm deleting directories:}
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    {Confirm overwriting directories:}
    property ConfirmOverwrite: Boolean read FConfirmOverwrite write FConfirmOverwrite default True;
    {Scan all directories in method ScanDrive:}
    property FullDriveScan: Boolean read FFullDriveScan write SetFullDriveScan default False;
    {Enable automatic update on filesystem changes:}
    property WatchDirectory: Boolean read FWatchDirectory write SetWatchDirectory default False;
    {Peform automatic update after ChangeInterval milliseconds:}
    property ChangeInterval: Cardinal read FChangeInterval write SetChangeInterval default MSecsPerSec;
    {Linked component TDirView:}
    property DirView: TDirView read FDirView write SetDirView;
    property ShowDirSize: Boolean read FShowDirSize write SetShowDirSize default False;
    {Show the volume labels of drives:}
    property ShowVolLabel: Boolean read FShowVolLabel write SetShowVolLabel default True;
    {How to display the drives volume labels:}
    property VolDisplayStyle: TVolumeDisplayStyle read FVolDisplayStyle write SetVolDisplayStyle default doPrettyName;
    {Show AVI-animation when performing a full drive scan:}
    property ShowAnimation: Boolean read FShowAnimation write FShowAnimation default False;
    {Don't watch these drives for changes:}
    property NoCheckDrives: string read  FNoCheckDrives write SetNoCheckDrives;
    property CompressedColor: TColor read FCompressedColor write SetCompressedColor default clBlue;
    property FileNameDisplay: TFileNameDisplay read FFileNameDisplay write SetFileNameDisplay default fndStored;
    {Additional events:}
    property OnStartScan: TNotifyEvent read FOnStartScan write FOnStartScan;
    property OnEndScan: TNotifyEvent read FOnEndScan write FOnEndScan;
    property OnScanDir: TDriveViewScanDirEvent read FOnScanDir write FOnScanDir;
    property OnDiskChange: TDriveViewDiskChangeEvent read FOnDiskChange write FOnDiskChange;
    property OnInsertedDiskChange: TDriveViewDiskChangeEvent read FOnInsertedDiskChange
      write FOnInsertedDiskChange;
    property OnChangeDetected: TDriveViewDiskChangeEvent read FOnChangeDetected
      write FOnChangeDetected;
    property OnChangeInvalid: TDriveViewDiskChangeEvent read FOnChangeInvalid
      write FOnChangeInvalid;
    property OnDisplayContextMenu: TNotifyEvent read FOnDisplayContextMenu
      write FOnDisplayContextMenu;
    property OnRefreshDrives: TNotifyEvent read FOnRefreshDrives
      write FOnRefreshDrives;

    property DDLinkOnExeDrag;

    property TargetPopUpMenu;

    property OnDDDragEnter;
    property OnDDDragLeave;
    property OnDDDragOver;
    property OnDDDrop;
    property OnDDQueryContinueDrag;
    property OnDDGiveFeedback;
    property OnDDDragDetect;
    property OnDDProcessDropped;
    property OnDDError;
    property OnDDExecuted;
    property OnDDFileOperation;
    property OnDDFileOperationExecuted;
    property OnDDMenuPopup;

    property Align;
    property Anchors;
    property AutoExpand;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ChangeDelay;
    property Color;
    property Ctl3D;
    property Constraints;
    property DoubleBuffered;
    {Delphi's drag&drop is not compatible with the OLE windows drag&drop:}
    property DragKind;
    property DragCursor;
    property DragMode Default dmAutomatic;
    property OnDragDrop;
    property OnDragOver;
    property Enabled;
    property Font;
    property HideSelection;
    property HotTrack;
    property Indent;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property RowSelect;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property TabOrder;
    property TabStop default True;
    property ToolTips;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsing;
    property OnCollapsed;
    property OnCompare;
    property OnDblClick;
    property OnDeletion;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanding;
    property OnExpanded;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses
  CompThread;

resourcestring
   SErrorInvalidDirName = 'New name contains invalid characters %s';

type
  PInt = ^Integer;

procedure Register;
begin
  RegisterComponents('DriveDir', [TDriveView]);
end; {Register}

constructor TNodeData.Create;
begin
  inherited;

  FAttr := 0;
  FExpanded := False;
  FScanned := False;
  FDirName := '';
  FShortName := '';
  FDirSize := CInvalidSize;
  FIsRecycleBin := False;
  FIconEmpty := True;
  shAttr := 0;
  PIDL := nil;
  ShellFolder := nil;
end; {TNodeData.Create}

destructor TNodeData.Destroy;
begin
  SetLength(FDirName, 0);

  if Assigned(PIDL) then
    FreePIDL(PIDL);

  inherited;
end; {TNodeData.Destroy}

  { TDriveTreeNode }

procedure TDriveTreeNode.Assign(Source: TPersistent);
var
  SourceData: TNodeData;
  NewData: TNodeData;
begin
  inherited Assign(Source);

  if not Deleting and (Source is TTreeNode) then
  begin
    SourceData := TNodeData(TTreeNode(Source).Data);
    NewData := TNodeData.Create();
    NewData.DirName := SourceData.DirName;
    NewData.ShortName := SourceData.ShortName;
    NewData.Attr := SourceData.Attr;
    NewData.Scanned := SourceData.Scanned;
    NewData.Data := SourceData.Data;
    NewData.Expanded := SourceData.Expanded;
    NewData.FIsRecycleBin := SourceData.FIsRecycleBin;
    NewData.IconEmpty := SourceData.IconEmpty;
    TTreeNode(Source).Data := NewData;
  end;
end;

  { TDriveView }

constructor TDriveView.Create(AOwner: TComponent);
var
  Drive: TDrive;
begin
  inherited;

  FCreating := True;

  if FChangeInterval = 0 then
    FChangeInterval := MSecsPerSec;

  for Drive := FirstDrive to LastDrive do
    with DriveStatus[Drive] do
    begin
      Scanned := False;
      Verified := False;
      RootNode := nil;
      RootNodeIndex := -1;
      DiscMonitor := nil;
      DefaultDir := EmptyStr;
      {ChangeTimer: }
      ChangeTimer := TTimer.Create(Self);
      ChangeTimer.Interval := 0;
      ChangeTimer.Enabled := False;
      ChangeTimer.OnTimer := ChangeTimerOnTimer;
      ChangeTimer.Tag := Ord(Drive);
    end;

  FFileOperator := TFileOperator.Create(Self);
  FFileOperator.ProgressTitle := coFileOperatorTitle;
  FFileOperator.Flags := [foAllowUndo, foNoConfirmMkDir];

  FCompressedColor := clBlue;
  FShowVolLabel := True;
  FChangeFlag := False;
  FLastDir := EmptyStr;
  FValidateFlag := False;
  FConfirmDelete := True;
  FShowAnimation := False;
  FDirectory := EmptyStr;
  FFileNameDisplay   := fndStored;
  FForceRename := False;
  FLastRenameName := '';
  FRenameNode := nil;
  FPrevSelected := nil;
  FPrevSelectedIndex := -1;
  FChangeTimerSuspended := 0;

  FConfirmOverwrite := True;
  FLastPathCut := '';
  FStartPos.X := -1;
  FStartPos.Y := -1;
  FDragPos := FStartPos;

  FInternalWindowHandle := Classes.AllocateHWnd(InternalWndProc);

  with FDragDropFilesEx do
  begin
    ShellExtensions.DragDropHandler := True;
  end;
end; {Create}

destructor TDriveView.Destroy;
var
  Drive: TDrive;
begin
  Classes.DeallocateHWnd(FInternalWindowHandle);

  for Drive := FirstDrive to LastDrive do
    with DriveStatus[Drive] do
    begin
      if Assigned(DiscMonitor) then
        DiscMonitor.Free;
      if Assigned(ChangeTimer) then
        ChangeTimer.Free;
    end;

  if Assigned(FFileOperator) then
    FFileOperator.Free;

  inherited Destroy;
end; {Destroy}

procedure TDriveView.InternalWndProc(var Msg: TMessage);
begin
  with Msg do
  begin
    if (Msg = WM_DEVICECHANGE) and
       ((wParam = {DBT_CONFIGCHANGED} $0018) or (wParam = {DBT_DEVICEARRIVAL} $8000) or
          (wParam = {DBT_DEVICEREMOVECOMPLETE} $8004)) then
    begin
      try
        //DriveInfo.Load;
        RefreshRootNodes(False, dsAll);
        if Assigned(OnRefreshDrives) then
          OnRefreshDrives(Self);
      except
        Application.HandleException(Self);
      end
    end;

    Result := DefWindowProc(FInternalWindowHandle, Msg, wParam, lParam);
  end;
end;

procedure TDriveView.CreateWnd;
var
  PIDLWorkPlace: PItemIDList;
  Drive: TDrive;
begin
  inherited;

  if Assigned(PopupMenu) then
    PopupMenu.Autopopup := False;

  OLECheck(shGetDesktopFolder(FDesktop));
  OLECheck(shGetSpecialFolderLocation(Self.Handle, CSIDL_DRIVES, PIDLWorkPlace));
  FDesktop.BindToObject(PIDLWorkPlace, nil, IID_IShellFolder, Pointer(FWorkPlace));
  FreePIDL(PIDLWorkPlace);

  FDragDropFilesEx.SourceEffects := [deCopy, deMove, deLink];
  FDragDropFilesEx.TargetEffects := [deCopy, deMove, deLink];

  if FPrevSelectedIndex >= 0 then
  begin
    FPrevSelected := Items[FPrevSelectedIndex];
    FPrevSelectedIndex := -1;
  end;

  for Drive := FirstDrive to LastDrive do
    with DriveStatus[Drive] do
    begin
      if RootNodeIndex >= 0 then
      begin
        RootNode := Items[RootNodeIndex];
        RootNodeIndex := -1;
      end;
    end;
end; {CreateWnd}

procedure TDriveView.DestroyWnd;
var
  Drive: TDrive;
begin
  if CreateWndRestores and (Items.Count > 0) and (csRecreating in ControlState) then
  begin
    FPrevSelectedIndex := -1;
    if Assigned(FPrevSelected) then
    begin
      FPrevSelectedIndex := FPrevSelected.AbsoluteIndex;
      FPrevSelected := nil;
    end;

    for Drive := FirstDrive to LastDrive do
      with DriveStatus[Drive] do
      begin
        RootNodeIndex := -1;
        if Assigned(RootNode) then
        begin
          RootNodeIndex := RootNode.AbsoluteIndex;
          RootNode := nil;
        end;
      end;
  end;
  inherited;
end;

function TDriveView.GetFQPIDL(Node: TTreeNode): PItemIDList;
var
  Eaten: ULONG;
  shAttr: ULONG;
begin
  Result := nil;
  if Assigned(Node) then
  begin
    FDesktop.ParseDisplayName(FParentForm.Handle, nil, PChar(NodePathName(Node)), Eaten,
      Result, shAttr);
  end;
end; {GetFQPIDL}

function TDriveView.NodeColor(Node: TTreeNode): TColor;
begin
  Result := clDefaultItemColor;
  with TNodeData(Node.Data) do
    if not Node.Selected then
    begin
      {Colored display of compressed directories:}
      if (Attr and FILE_ATTRIBUTE_COMPRESSED) <> 0 then
          Result := FCompressedColor
        else
      {Dimmed display, if hidden-atrribut set:}
      if FDimmHiddenDirs and ((Attr and FILE_ATTRIBUTE_HIDDEN) <> 0) then
          Result := clGrayText
    end;
end;

function TDriveView.GetCustomDirView: TCustomDirView;
begin
  Result := DirView;
end;

procedure TDriveView.SetCustomDirView(Value: TCustomDirView);
begin
  DirView := Value as TDirView;
end;

function TDriveView.NodePath(Node: TTreeNode): string;
var
  ParentNode: TTreeNode;
begin
  if not Assigned(Node) then
    raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['GetDirPath']));

  Result := GetDirName(Node);
  ParentNode := Node.Parent;

  while (ParentNode <> nil) and (ParentNode.Level >= 0) do
  begin
    if ParentNode.Level > 0 then
      Result := GetDirName(ParentNode) + '\' + Result
    else
      Result := GetDirName(ParentNode) + Result;

    ParentNode := ParentNode.Parent;
  end;

  if Length(Result) = 3 then
    SetLength(Result, 2);
end;

{NodePathName: Returns the complete path to Node with trailing backslash on rootnodes:
 C:\ ,C:\WINDOWS, C:\WINDOWS\SYSTEM  }
function TDriveView.NodePathName(Node: TTreeNode): string;
begin
  Result := NodePath(Node);
  if Length(Result) = 2 then
    Result := Result + '\';
end; {NodePathName}

function TDriveView.NodeIsRecycleBin(Node: TTreeNode): Boolean;
begin
  Result := TNodeData(Node.Data).IsRecycleBin;
end;

function TDriveView.NodePathExists(Node: TTreeNode): Boolean;
begin
  Result := DirExists(NodePathName(Node));
end;

function TDriveView.CanEdit(Node: TTreeNode): Boolean;
begin
  Result := inherited CanEdit(Node) or FForceRename;
  if Result then
  begin
    Result := Assigned(Node.Parent) and
      (not TNodeData(Node.Data).IsRecycleBin) and
      (not ReadOnly) and
      (FDragDropFilesEx.DragDetectStatus <> ddsDrag) and
      ((TNodeData(Node.Data).Attr and (faReadOnly or faSysFile)) = 0) and
      (UpperCase(Node.Text) = UpperCase(GetDirName(Node)));
  end;
  FForceRename := False;
end; {CanEdit}

procedure TDriveView.Edit(const Item: TTVItem);
var
  NewDirName: string;
  SRec: TSearchRec;
  Node: TTreeNode;
  Info: string;
  i: Integer;
begin
  Node := GetNodeFromHItem(Item);
  if (Length(Item.pszText) > 0) and (Item.pszText <> Node.Text) then
  begin
    if StrContains(coInvalidDosChars, Item.pszText) then
    begin
      Info := coInvalidDosChars;
      for i := Length(Info) downto 1 do
        System.Insert(Space, Info, i);

      if Assigned(OnEdited) then
      begin
        NewDirName := Node.Text;
        OnEdited(Self, Node, NewDirName);
      end;
      if Length(Item.pszText) > 0 then
        raise EInvalidDirName.CreateFmt(SErrorInvalidDirName, [Info]);
      Exit;
    end;

{$IFNDEF NO_THREADS}
    StopWatchThread;
    if Assigned(DirView) then
      DirView.StopWatchThread;
{$ENDIF}

    with FFileOperator do
    begin
      Flags := [foAllowUndo, foNoConfirmation];
      Operation := foRename;
      OperandFrom.Clear;
      OperandTo.Clear;
      OperandFrom.Add(NodePath(Node));
      OperandTo.Add(IncludeTrailingBackslash(NodePath(Node.Parent)) + Item.pszText);
    end;

    try
      if FFileOperator.Execute then
      begin
        Node.Text := Item.pszText;
        TNodeData(Node.Data).DirName := Item.pszText;
        if FindFirst(IncludeTrailingBackslash(NodePath(Node.Parent)) + Item.pszText,
             faAnyFile, SRec) = 0 then
        begin
          TNodeData(Node.Data).ShortName := string(SRec.FindData.cAlternateFileName);
        end;
        FindClose(SRec);
        SortChildren(Node.Parent, False);

        inherited;
      end
        else
      begin
        if FileOrDirExists(IncludeTrailingBackslash(NodePath(Node.Parent)) + Item.pszText) then
          Info := SErrorRenameFileExists + Item.pszText
        else
          Info := SErrorRenameFile + Item.pszText;

        MessageBeep(MB_ICONHAND);
        if MessageDlg(FormatLastOSError(Info), mtError, [mbOK, mbAbort], 0) = mrOK then
        begin
          FLastRenameName := Item.pszText;
          FRenameNode := Node;
          PostMessage(Self.Handle, WM_USER_RENAME, 0, 0);
        end;
      end;
    finally
{$IFNDEF NO_THREADS}
      StartWatchThread;
{$ENDIF}
      if Assigned(DirView) then
      begin
        DirView.Reload2;
{$IFNDEF NO_THREADS}
        DirView.StartWatchThread;
{$ENDIF}
      end;
    end;
  end;
end; {Edit}

procedure TDriveView.WMUserRename(var Message: TMessage);
begin
  if Assigned(FRenameNode) then
  begin
    FForceRename := True;
    TreeView_EditLabel(Handle, FRenameNode.ItemID);
    SetWindowText(TreeView_GetEditControl(Self.Handle), PChar(FLastRenameName));
    FRenameNode := nil;
  end;
end; {WMUserRename}

function TDriveView.CanExpand(Node: TTreeNode): Boolean;
var
  SubNode: TTreeNode;
  Drive: TDrive;
  SaveCursor: TCursor;
begin
  Result := inherited CanExpand(Node);
  Drive := GetDriveToNode(Node);
  if Node.HasChildren then
  begin
    if (Node.Level = 0) and
       (not DriveStatus[Drive].Scanned) and
       (Drive >= FirstFixedDrive) then
    begin
      SubNode := Node.GetFirstChild;
      if not Assigned(SubNode) then
      begin
        ScanDrive(Drive);
        SubNode := Node.GetFirstChild;
        Node.HasChildren := Assigned(SubNode);
        Result := Node.HasChildren;
{$IFNDEF NO_THREADS}
        if not Assigned(DriveStatus[Drive].DiscMonitor) then
          CreateWatchThread(Drive);
{$ENDIF}
      end;
    end
      else
    begin
      SaveCursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
      try
        if (not TNodeData(Node.Data).Scanned) and DoScanDir(Node) then
        begin
          ReadSubDirs(Node, DriveInfo[Drive].DriveType);
        end;
      finally
        Screen.Cursor := SaveCursor;
      end;
    end;
  end;
end; {CanExpand}

procedure TDriveView.GetImageIndex(Node: TTreeNode);
begin
  if TNodeData(Node.Data).IconEmpty then
    SetImageIndex(Node);
  inherited;
end; {GetImageIndex}

procedure TDriveView.Loaded;
begin
  inherited;
  {Create the drive nodes:}
  RefreshRootNodes(False, dsDisplayName or dvdsFloppy);
  {Set the initial directory:}
  if (Length(FDirectory) > 0) and DirExists(FDirectory) then
    Directory := FDirectory;

  FCreating := False;
end; {Loaded}

function TDriveView.CreateNode: TTreeNode;
begin
  Result := TDriveTreeNode.Create(Items);
end;

procedure TDriveView.Delete(Node: TTreeNode);
var
  NodeData: TNodeData;
begin
  if Node = FPrevSelected then
    FPrevSelected := nil;

  NodeData := nil;

  if Assigned(Node) and Assigned(Node.Data) then
    NodeData := TNodeData(Node.Data);
  Node.Data := nil;

  inherited;

  if Assigned(NodeData) and not (csRecreating in ControlState) then
  begin
    NodeData.Destroy;
  end;
end; {OnDelete}

procedure TDriveView.KeyPress(var Key: Char);
begin
  inherited;

  if Assigned(Selected) then
  begin
    if Pos(Key, coInvalidDosChars) <> 0 then
    begin
      Beep;
      Key := #0;
    end;
  end;
end; {KeyPress}

function TDriveView.CanChange(Node: TTreeNode): Boolean;
var
  Path: string;
  Drive: TDrive;
begin
  Result := inherited CanChange(Node);

  if not Reading and not (csRecreating in ControlState) then
  begin
    if Result and Assigned(Node) then
    begin
      Path := NodePathName(Node);
      if Path <> FLastDir then
      begin
        Drive := Path[1];

        DriveInfo.ReadDriveStatus(Drive, dsSize or dsImageIndex);
        if not DriveInfo[Drive].DriveReady then
        begin
          MessageDlg(Format(SDriveNotReady, [Drive]), mtError, [mbOK], 0);
          Result := False;
        end
          else
        if not DirectoryExists(Path) then
        begin
          MessageDlg(Format(SDirNotExists, [Path]), mtError, [mbOK], 0);
          Result := False;
        end;
      end;
    end;

    if Result and (csDestroying in ComponentState) then
      Result := False;

    if Result and
       (not FCanChange) and
       Assigned(Node) and
       Assigned(Node.Data) and
       Assigned(Selected) and
       Assigned(Selected.Data) then
    begin
      DropTarget := Node;
      Result := False;
    end
      else
    DropTarget := nil;
  end;
end; {CanChange}

procedure TDriveView.Change(Node: TTreeNode);
var
  Drive: TDrive;
  OldSerial: DWORD;
  NewDir: string;
  LastDrive: TDrive;
begin
  if not Reading and not (csRecreating in ControlState) then
  begin
    if Assigned(Node) then
    begin
      NewDir := NodePathName(Node);
      if NewDir <> FLastDir then
      begin
        Drive := NewDir[1];
        if Length(FLastDir) > 0 then
          LastDrive := FLastDir[1]
        else
          LastDrive := #0;

        FChangeFlag := True;
        FLastDir := NewDir;

        OldSerial := DriveInfo[Drive].DriveSerial;
        DriveInfo.ReadDriveStatus(Drive, dsSize or dsImageIndex);
        with DriveInfo[Drive]^ do
        begin
          if Assigned(FDirView) and (FDirView.Path <> NewDir) then
            FDirView.Path := NewDir;

          if DriveReady then
          begin
            if not DirExists(NewDir) then
            begin
              ValidateDirectory(DriveStatus[Upcase(NewDir[1])].RootNode);
              Exit;
            end;

            DriveStatus[Drive].DefaultDir := IncludeTrailingBackslash(NewDir);

            if LastDrive <> Drive then
            begin
{$IFNDEF NO_THREADS}
              if (LastDrive >= FirstDrive) and
                 (DriveInfo[LastDrive].DriveType = DRIVE_REMOVABLE) then
                    TerminateWatchThread(LastDrive);
{$ENDIF}

              {Drive serial has changed or is missing: allways reread the drive:}
              if (DriveSerial <> OldSerial) or (DriveSerial = 0) then
              begin
                if TNodeData(DriveStatus[Drive].RootNode.Data).Scanned then
                  ScanDrive(Drive);
                if Assigned(FOnInsertedDiskChange) then
                  FOnInsertedDiskChange(Self, Drive);
              end;

              if Assigned(FOnDiskChange) then
                FOnDiskChange(Self, Drive);
            end;
{$IFNDEF NO_THREADS}
            StartWatchThread;
{$ENDIF}
          end
            else  {Drive not ready:}
          begin
            DriveStatus[Drive].RootNode.DeleteChildren;
            DriveStatus[Drive].DefaultDir := EmptyStr;
            if LastDrive <> Drive then
            begin
              if Assigned(FOnInsertedDiskChange) then
                FOnInsertedDiskChange(Self, Drive);
              if Assigned(FOnDiskChange) then
                FOnDiskChange(Self, Drive);
            end;
          end;
        end;
      end;

      if (not Assigned(FPrevSelected)) or (not FPrevSelected.HasAsParent(Node)) then
        Node.Expand(False);
      FPrevSelected := Node;

      ValidateCurrentDirectoryIfNotMonitoring;
    end;
  end;

  inherited;
end; {Change}

procedure TDriveView.SetImageIndex(Node: TTreeNode);
var
  FileInfo: TShFileInfo;
  NodePath: string;
begin
  if Assigned(Node) and TNodeData(Node.Data).IconEmpty then
  begin
    NodePath := NodePathName(Node);
    if Node.Level = 0 then
    begin
      with DriveInfo[NodePath[1]]^ do
      begin
        if ImageIndex = 0 then
        begin
          DriveInfo.ReadDriveStatus(NodePath[1], dsImageIndex);
          Node.ImageIndex := DriveInfo[NodePath[1]].ImageIndex;
        end
          else Node.ImageIndex := ImageIndex;
        Node.SelectedIndex := Node.ImageIndex;
      end;
    end
      else
    begin
      if DriveInfo[NodePath[1]].DriveType = DRIVE_REMOTE then
      begin
        Node.ImageIndex := StdDirIcon;
        Node.SelectedIndex := StdDirSelIcon;
      end
        else
      begin
        try
          SHGetFileInfo(PChar(NodePath), 0, FileInfo, SizeOf(FileInfo),
            SHGFI_SYSICONINDEX or SHGFI_SMALLICON);

          if (FileInfo.iIcon < Images.Count) and (FileInfo.iIcon > 0) then
          begin
            Node.ImageIndex := FileInfo.iIcon;
            SHGetFileInfo(PChar(NodePath), 0, FileInfo, SizeOf(FileInfo),
              SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_OPENICON);
            Node.SelectedIndex := FileInfo.iIcon;
          end
            else
          begin
            Node.ImageIndex := StdDirIcon;
            Node.SelectedIndex := StdDirSelIcon;
          end;
        except
          Node.ImageIndex := StdDirIcon;
          Node.SelectedIndex := StdDirSelIcon;
        end;
      end;
    end;
  end; {IconEmpty}
  TNodeData(Node.Data).IconEmpty := False;
end; {SetImageIndex}

function TDriveView.GetDriveText(Drive: TDrive): string;
begin
  if FShowVolLabel and (Length(DriveInfo.GetPrettyName(Drive)) > 0) then
  begin
    case FVolDisplayStyle of
      doPrettyName:     Result := DriveInfo.GetPrettyName(Drive);
      doDisplayName:    Result := DriveInfo.GetDisplayName(Drive);
      doLongPrettyName: Result := DriveInfo.GetLongPrettyName(Drive);
    end; {Case}
  end
    else Result := Drive + ':';
end; {GetDriveText}

function TDriveView.GetValidDrivesStr: String;
var
  Drive: TDrive;
begin
  Result := '';
  for Drive := FirstDrive to LastDrive do
    if DriveInfo[Drive].Valid then
      Result := Result + Drive;
end; {GetValidDriveStr}

type
  TFolderAttributesGetterThread = class(TCompThread)
  private
    FParentFolder: iShellFolder;
    FPIDL: PItemIDList;
    FshAttr: PUINT;

  protected
    procedure Execute; override;

  public
    constructor Create(ParentFolder: iShellFolder; PIDL: PItemIDList; shAttr: PUINT);

    class procedure GetFolderAttributes(ParentFolder: iShellFolder; PIDL: PItemIDList; shAttr: PUINT);
  end;

class procedure TFolderAttributesGetterThread.GetFolderAttributes(
  ParentFolder: iShellFolder; PIDL: PItemIDList; shAttr: PUINT);
var
  NotResult: Boolean;
  ErrorMode: Word;
begin
  ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
  try
    try
      NotResult := not Succeeded(ParentFolder.GetAttributesOf(1, PIDL, shAttr^));
    finally
      SetErrorMode(ErrorMode);
    end;
    if NotResult then shAttr^ := 0;
  except
    shAttr^ := 0;
  end;
end;

constructor TFolderAttributesGetterThread.Create(ParentFolder: iShellFolder; PIDL: PItemIDList; shAttr: PUINT);
begin
  inherited Create(True);
  FParentFolder := ParentFolder;
  FPIDL := PIDL;
  FshAttr := shAttr;
end;

procedure TFolderAttributesGetterThread.Execute;
begin
  GetFolderAttributes(FParentFolder, FPIDL, FshAttr);
end;

procedure TDriveView.GetNodeShellAttr(ParentFolder: IShellFolder;
  NodeData: TNodeData; Path: string; ContentMask: Boolean = True);
var
{$IFNDEF IDE}
  Thread: TFolderAttributesGetterThread;
{$ENDIF}
  shAttr: ULONG;
begin
  if (not Assigned(ParentFolder)) or (not Assigned(NodeData)) then
    Exit;

  if not Assigned(NodeData.PIDL) then
    NodeData.PIDL := PIDL_GetFromParentFolder(ParentFolder, PChar(Path));
  if Assigned(NodeData.PIDL) then
  begin
    if ContentMask then
      shAttr := SFGAO_DISPLAYATTRMASK or SFGAO_CONTENTSMASK
    else
      shAttr := SFGAO_DISPLAYATTRMASK;

    // Resoving attributes make take ages, so we run it from a separate thread
    // and timeout waiting for the thread after a second.
    // But when running from IDE, it triggers starting/exiting the thread,
    // again taking ages. So in IDE we revert to single-thread approach
    {$IFDEF IDE}
    TFolderAttributesGetterThread.GetFolderAttributes(ParentFolder, NodeData.PIDL, @shAttr);
    NodeData.shAttr := shAttr;
    {$ELSE}
    Thread := TFolderAttributesGetterThread.Create(ParentFolder, NodeData.PIDL, @shAttr);
    Thread.FreeOnTerminate := True;
    Thread.Resume;
    if Thread.WaitFor(MSecsPerSec) then
    begin
      NodeData.shAttr := shAttr;
    end
      else
    begin
      NodeData.shAttr := 0;
    end;
    {$ENDIF}

    if not ContentMask then
      NodeData.shAttr := NodeData.shAttr or SFGAO_HASSUBFOLDER;

    if not Assigned(NodeData.ShellFolder) then
    begin
      ParentFolder.BindToObject(NodeData.PIDL, nil, IID_IShellFolder,
        Pointer(NodeData.ShellFolder));
    end;
  end;
end; {GetNodeAttr}

procedure TDriveView.RefreshRootNodes(ScanDirectory: Boolean; dsFlags: Integer);
var
  Drive: Char;
  NewText: string;
  NextDrive: TDrive;
  D: TDrive;
  SaveCursor: TCursor;
  WasValid: Boolean;
  OldSerial: DWORD;
  WFirstDrive: TDrive;
  NodeData: TNodeData;
begin
  {Fetch disabled drives from the registry:}

  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if (dsFlags and dvdsFloppy) <> 0 then
      WFirstDrive := FirstDrive
    else
      WFirstDrive := FirstFixedDrive;

    for Drive := WFirstDrive to LastDrive do
    begin
      with DriveInfo[Drive]^ do
      begin
        WasValid  := Assigned(DriveStatus[Drive].RootNode);
        OldSerial := DriveSerial;
      end;
      if ((dsFlags and dvdsReReadAllways) = 0) and
         (Length(DriveInfo[Drive].DisplayName) > 0) then
            dsFlags := dsFlags and (not dsDisplayName);

      DriveInfo.ReadDriveStatus(Drive, dsFlags);

      with DriveInfo[Drive]^, DriveStatus[Drive] do
      begin
        if Valid then
        begin
          if not WasValid then
          {New drive has arrived: insert new rootnode:}
          begin
            NextDrive := LastDrive;
            if not FCreating then
            begin
              for D := Drive to LastDrive do
              begin
                if Assigned(DriveStatus[D].RootNode) then
                begin
                  NextDrive := D;
                  Break;
                end;
              end;
            end;

            { Create root directory node }
            NodeData := TNodeData.Create;
            NodeData.DirName := Drive + ':\';
            NodeData.ShortName := Drive + ':\';

            {Get the shared attributes:}
            if (Drive >= FirstFixedDrive) and (DriveType <> DRIVE_REMOVABLE) and
               ((DriveType <> DRIVE_REMOTE) or GetNetWorkConnected(Drive)) then
            begin
              GetNodeShellAttr(FWorkPlace, NodeData, NodeData.DirName);
            end;

            if Assigned(DriveStatus[NextDrive].RootNode) then
              RootNode := Items.InsertObject(DriveStatus[NextDrive].RootNode, '', NodeData)
            else
              RootNode := Items.AddObject(nil, '', NodeData);

            if (NodeData.shAttr and SFGAO_SHARE) <> 0 then
              RootNode.OverlayIndex := 0;

            RootNode.Text := GetDisplayName(RootNode);
            RootNode.HasChildren := True;

            Scanned := False;
            Verified := False;
          end
            else
          if RootNode.ImageIndex <> DriveInfo[Drive].ImageIndex then
          begin {WasValid = True}
            RootNode.ImageIndex    := DriveInfo[Drive].ImageIndex;
            RootNode.SelectedIndex := DriveInfo[Drive].ImageIndex;
          end;

          if (Drive >= FirstFixedDrive) and Scanned then
          begin
            if ScanDirectory and (DriveSerial <> OldSerial) then
            begin
              ScanDrive(Drive);
            end;
          end;

          if Assigned(RootNode) then
          begin
            NewText := GetDisplayName(RootNode);
            if RootNode.Text <> NewText then
              RootNode.Text := NewText;
          end;
        end
          else
        if WasValid then
        {Drive has been removed => delete rootnode:}
        begin
          if Directory[1] = Drive then
          begin
            Directory := NodePathName(DriveStatus[Drive].RootNode.GetPrevSibling);
            if not Assigned(Selected) then
            begin
              Directory := NodePathName(DriveStatus[FirstFixedDrive].RootNode);
            end;
          end;
          Scanned := False;
          Verified := False;
          RootNode.Delete;
          RootNode := nil;
        end;
      end;
    end;
  finally
    Screen.Cursor := SaveCursor;
  end;
end; {RefreshRootNodes}

function TDriveView.AddChildNode(ParentNode: TTreeNode; SRec: TSearchRec): TTreeNode;
var
  NewNode: TTreeNode;
  NodeData: TNodeData;
begin
  NodeData := TNodeData.Create;
  NodeData.Attr := SRec.Attr;
  NodeData.DirName := SRec.Name;
  NodeData.ShortName := SRec.FindData.cAlternateFileName;
  NodeData.FIsRecycleBin :=
    (SRec.Attr and faSysFile <> 0) and
    (ParentNode.Level = 0) and
    ((UpperCase(SRec.Name) = 'RECYCLED') or
     (UpperCase(SRec.Name) = 'RECYCLER'));

  { query content attributes ("has subfolder") only if tree view is visible }
  { to avoid unnecessary scan of subfolders (which may take some time) }
  { if tree view is not visible anyway }
  if not Assigned(TNodeData(ParentNode.Data).ShellFolder) then
    GetNodeShellAttr(FWorkPlace, TNodeData(ParentNode.Data), NodePathName(ParentNode), Visible);

  GetNodeShellAttr(TNodeData(ParentNode.Data).ShellFolder, NodeData, SRec.Name, Visible);

  NewNode := Self.Items.AddChildObject(ParentNode, '', NodeData);
  NewNode.Text := GetDisplayName(NewNode);

  if (NodeData.shAttr and SFGAO_SHARE) <> 0 then
    NewNode.OverlayIndex := 0;

  Result := NewNode;
end; {AddChildNode}

function TDriveView.GetDriveStatus(Drive: TDrive): TDriveStatus;
begin
  Result := DriveStatus[Upcase(Drive)];
end; {GetDriveStatus}

function TDriveView.DoScanDir(FromNode: TTreeNode): Boolean;
begin
  with TNodeData(FromNode.Data) do
    Result := not IsRecycleBin;
  if Assigned(FOnScanDir) then
    FOnScanDir(Self, FromNode, Result);
end; {DoScanDir}

function TDriveView.DirAttrMask: Integer;
begin
  Result := faDirectory or faSysFile;
  if ShowHiddenDirs then
    Result := Result or faHidden;
end;

procedure TDriveView.ScanDrive(Drive: TDrive);
var
  DosError: Integer;
  RootNode: TTreeNode;
  SaveCursor: TCursor;
  FAnimate: TAnimate;

  procedure ScanPath(const Path: string; ParentNode: TTreeNode);
  var
    SRec: TSearchRec;
    SubNode: TTreeNode;
  begin
    if not DoScanDir(ParentNode) then
      Exit;

    DosError := FindFirst(Path, DirAttrMask, Srec);
    while DosError = 0 do
    begin
      if (SRec.Name <> '.') and
         (SRec.Name <> '..') and
         (SRec.Attr and faDirectory <> 0) then
      begin
        if (SRec.Attr And faDirectory) <> 0 then
        begin { Scan subdirectory }
          SubNode := AddChildNode(ParentNode, SRec);
          TNodeData(SubNode.Data).Scanned := True;
          ScanPath(ExtractFilePath(Path) + SRec.Name + '\*.*', SubNode);
          if not FContinue then
            Break;
        end;
      end;
      DosError := FindNext(SRec);
    end;
    FindClose(Srec);
    if (Items.Count mod 10) = 0 then
      Application.ProcessMessages;
    if not FContinue then
      Exit;
  end; {ScanPath}

begin {ScanDrive}
  with Self.Items do
  begin
    FContinue := True;
    if not FFullDriveScan then
    begin
      ValidateDirectory(FindNodeToPath(Drive + ':\'));
      DriveStatus[Drive].Scanned := True;
      DriveStatus[Drive].Verified := False;
    end
      else
    begin
      FAnimate := nil;
      SaveCursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
      Items.BeginUpdate;

      if FShowAnimation then
      begin
        FAnimate := TAnimate.Create(Self);
        FAnimate.Top := (Height - FAnimate.Height) div 2;
        FAnimate.Left := ((Width - FAnimate.Width) * 2) div 3;
        FAnimate.Parent := Self;
        FAnimate.CommonAVI := aviFindFolder;
        FAnimate.Active := True;
      end;

      if Assigned(FOnStartScan) then
        FOnStartScan(Self);

      try
        RootNode := DriveStatus[Drive].RootNode;
        if not Assigned(RootNode) then Exit;

        iF RootNode.HasChildren then
          RootNode.DeleteChildren;

        ScanPath(Drive + ':\*.*', RootNode);      { scan subdirectories of rootdir}
        TNodeData(RootNode.Data).Scanned := True;

        DriveStatus[Drive].Scanned := True;
        DriveStatus[Drive].Verified := True;
      finally
        SortChildren(DriveStatus[Drive].RootNode, True);
        EndUpdate;
        if Assigned(FAnimate) then
          FAnimate.Free;
      end;
      RootNode.Expand(False);

      Screen.Cursor := SaveCursor;
      if Assigned(FOnEndScan) then
      FOnEndScan(Self);
    end;
  end;
end; {ScanDrive}

function TDriveView.FindNodeToPath(Path: string): TTreeNode;
var
  Drive: Char;

  function SearchSubDirs(ParentNode: TTreeNode; Path: string): TTreeNode; forward;

  function DoSearchSubDirs(ParentNode: TTreeNode; Path: string): TTreeNode;
  var
    i: Integer;
    Node: TTreeNode;
    Dir: string;
  begin
    {Extract first directory from path:}
    i := Pos('\', Path);

    if i = 0 then
      i := Length(Path);

    Dir := System.Copy(Path, 1, i);
    System.Delete(Path, 1, i);

    if Dir[Length(Dir)] = '\' then
      SetLength(Dir, Pred(Length(Dir)));

    Node := ParentNode.GetFirstChild;
    if not Assigned(Node) then
    begin
      ValidateDirectoryEx(ParentNode, rsRecursiveExisting, True);
      Node := ParentNode.GetFirstChild;
    end;

    Result := nil;
    while Assigned(Node) do
    begin
      if (UpperCase(GetDirName(Node)) = Dir) or (TNodeData(Node.Data).ShortName = Dir) then
      begin
        if Length(Path) > 0 then
          Result := SearchSubDirs(Node, Path)
        else
          Result := Node;
        Exit;
      end;
      Node := ParentNode.GetNextChild(Node);
    end;
  end;

  function SearchSubDirs(ParentNode: TTreeNode; Path: string): TTreeNode;
  var
    Read: Boolean;
  begin
    Result := nil;
    if Length(Path) > 0 then
    begin
      Read := False;

      if not TNodeData(ParentNode.Data).Scanned then
      begin
        ReadSubDirs(ParentNode, GetDriveTypetoNode(ParentNode));
        Read := True;
      end;

      Result := DoSearchSubDirs(ParentNode, Path);

      // reread subfolders, just in case the directory we look for was just created
      // (as can happen when navigating to new remote directory with synchronized
      // browsing enabled and opting to create the non-existing local directory)
      if (not Assigned(Result)) and (not Read) then
      begin
        ValidateDirectoryEx(ParentNode, rsNoRecursive, True);
        Result := DoSearchSubDirs(ParentNode, Path);
      end;
    end;
  end; {SearchSubDirs}

begin {FindNodeToPath}
  Result := nil;
  if Length(Path) < 3 then
    Exit;

  // Particularly when used by TDirView to delegate browsing to
  // hidden drive view, the handle may not be created
  HandleNeeded;

  Drive := Upcase(Path[1]);
  if (Drive < FirstDrive) or (Drive > LastDrive) then
    EConvertError.Create(Format(ErrorInvalidDrive, [Drive]))
  else
    if Assigned(DriveStatus[Drive].RootNode) then
    begin
      System.Delete(Path, 1, 3);
      if Length(Path) > 0 then
      begin
        if not DriveStatus[Drive].Scanned then
          ScanDrive(Drive);
        Result := SearchSubDirs(DriveStatus[Drive].RootNode, UpperCase(Path));
      end
        else Result := DriveStatus[Drive].RootNode;
    end;
end; {FindNodetoPath}

function TDriveView.CheckForSubDirs(Path: string): Boolean;
var
  DosError: Integer;
  SRec: TSearchRec;
begin
  Result := False;

  DosError := FindFirst(IncludeTrailingBackslash(Path) + '*.', DirAttrMask, SRec);
  while DosError = 0 do
  begin
    if (SRec.Name <> '.' ) and
       (SRec.Name <> '..') and
       (SRec.Attr and faDirectory <> 0) then
    begin
      Result := True;
      Break;
    end;
    DosError := FindNext(SRec);
  end;
  FindClose(SRec);
end; {CheckForSubDirs}

function TDriveView.ReadSubDirs(Node: TTreeNode; DriveType: Integer): Boolean;
var
  DosError: Integer;
  SRec: TSearchRec;
  NewNode: TTreeNode;
begin
  Result := False;
  DosError := FindFirst(IncludeTrailingBackslash(NodePath(Node)) + '*.*', DirAttrMask, SRec);
  while DosError = 0 do
  begin
    if (SRec.Name <> '.' ) and
       (SRec.Name <> '..') and
       (SRec.Attr and faDirectory <> 0) then
    begin
      NewNode := AddChildNode(Node, SRec);
      if DoScanDir(NewNode) then
      begin
        NewNode.HasChildren := Bool(TNodeData(NewNode.Data).shAttr and SFGAO_HASSUBFOLDER);

        TNodeData(NewNode.Data).Scanned := not NewNode.HasChildren;
      end
        else
      begin
        NewNode.HasChildren := False;
        TNodeData(NewNode.Data).Scanned := True;
      end;
      Result := True;
    end;
    DosError := FindNext(SRec);
  end; {While DosError = 0}
  FindClose(Srec);
  TNodeData(Node.Data).Scanned := True;

  if Result then SortChildren(Node, False)
    else Node.HasChildren := False;
  Application.ProcessMessages;
end; {ReadSubDirs}

function TDriveView.CallBackValidateDir(Var Node: TTreeNode; Data: Pointer): Boolean;

type
  PSearchRec = ^TSearchRec;

var
  WorkNode: TTreeNode;
  DelNode: TTreeNode;
  NewNode: TTreeNode;
  SRec: TSearchRec;
  SrecList: TStringList;
  SubDirList: TStringList;
  DosError: Integer;
  Index: Integer;
  NewDirFound: Boolean;
  ParentDir: string;

begin {CallBackValidateDir}
  Result := True;
  if (not Assigned(Node)) or (not Assigned(Node.Data)) then
    Exit;

  NewDirFound := False;

  {Check, if directory still exists: (but not with root directory) }
  if Assigned(Node.Parent) and (PScanDirInfo(Data)^.StartNode = Node) then
    if not DirExists(NodePathName(Node)) then
    begin
      WorkNode := Node.Parent;
      if Selected = Node then
        Selected := WorkNode;
      if DropTarget = Node then
        DropTarget := nil;
      Node.Delete;
      Node := nil;
      Exit;
    end;

  WorkNode := Node.GetFirstChild;

  if TNodeData(Node.Data).Scanned and Assigned(WorkNode) then
  {if node was already scanned: check wether the existing subnodes are still alive
   and add all new subdirectories as subnodes:}
  begin
    if DoScanDir(Node) then
    begin
      ParentDir := IncludeTrailingBackslash(NodePath(Node));
      {Build list of existing subnodes:}
      SubDirList := TStringList.Create;
      while Assigned(WorkNode) do
      begin
        SubDirList.Add(TNodeData(WorkNode.Data).DirName);
        WorkNode := Node.GetNextChild(WorkNode);
      end;
      {Sorting not required, because the subnodes are already sorted!}

      SRecList := TStringList.Create;
      DosError := FindFirst(ParentDir + '*.*', DirAttrMask, SRec);
      while DosError = 0 do
      begin
        if (Srec.Name <> '.' ) and
           (Srec.Name <> '..') and
           (Srec.Attr and faDirectory <> 0) then
        begin
          SrecList.Add(Srec.Name);
          if not SubDirList.Find(Srec.Name, Index) then
          {Subnode does not exists: add it:}
          begin
            NewNode := AddChildNode(Node, SRec);
            NewNode.HasChildren := CheckForSubDirs(ParentDir + Srec.Name);
            TNodeData(NewNode.Data).Scanned := Not NewNode.HasChildren;
            NewDirFound  := True;
          end;
        end;
        DosError := FindNext(Srec);
      end;
      FindClose(Srec);
      Sreclist.Sort;

      {Remove not existing subnodes:}
      WorkNode := Node.GetFirstChild;
      while Assigned(WorkNode) do
      begin
        if not Assigned(WorkNode.Data) or
           not SrecList.Find(TNodeData(WorkNode.Data).DirName, Index) then
        begin
          DelNode := WorkNode;
          WorkNode := Node.GetNextChild(WorkNode);
          DelNode.Delete;
        end
          else
        begin
          if (SrecList[Index] <> TNodeData(WorkNode.Data).DirName) then
          begin
            {Case of directory letters has changed:}
            TNodeData(WorkNode.Data).DirName := SrecList[Index];
            TNodeData(WorkNode.Data).ShortName := ExtractShortPathName(NodePathName(WorkNode));
            WorkNode.Text := SrecList[Index];
          end;
          SrecList.Delete(Index);
          WorkNode := Node.GetNextChild(WorkNode);
        end;
      end;
      SrecList.Free;
      SubDirList.Free;
      {Sort subnodes:}
      if NewDirFound then
        SortChildren(Node, False);
    end;
  end
    else
  {Node was not already scanned:}
  if (PScanDirInfo(Data)^.SearchNewDirs or
     TNodeData(Node.Data).Scanned or
     (Node = PScanDirInfo(Data)^.StartNode)) and
     DoScanDir(Node) then
        ReadSubDirs(Node, PScanDirInfo(Data)^.DriveType);
end; {CallBackValidateDir}

procedure TDriveView.RebuildTree;
var
  Drive: TDrive;
begin
  for Drive := FirstDrive to LastDrive do
    with DriveStatus[Drive] do
      if Assigned(RootNode) and DriveStatus[Drive].Scanned then
        ValidateDirectory(RootNode);
end;

procedure TDriveView.ValidateCurrentDirectoryIfNotMonitoring;
begin
  if Assigned(Selected) and
     not Assigned(DriveStatus[GetDriveToNode(Selected)].DiscMonitor) then
  begin
    ValidateDirectory(Selected);
  end;
end;

procedure TDriveView.ValidateDirectoryEx(Node: TTreeNode; Recurse: TRecursiveScan;
  NewDirs: Boolean);
var
  Info: PScanDirInfo;
  SelDir: string;
  SaveCursor: TCursor;
{$IFNDEF NO_THREADS}
  RestartWatchThread: Boolean;
{$ENDIF}
  SaveCanChange: Boolean;
  CurrentPath: string;
begin
  if Assigned(Node) and Assigned(Node.Data) and
     (not FValidateFlag) and DoScanDir(Node) then
  begin
    SelDir := Directory;
    SaveCursor := Screen.Cursor;
    if Self.Focused and (Screen.Cursor <> crHourGlass) then
      Screen.Cursor := crHourGlass;

    CurrentPath := NodePath(Node);

    if Node.Level = 0 then
      DriveStatus[CurrentPath[1]].ChangeTimer.Enabled := False;

    {$IFNDEF NO_THREADS}
    RestartWatchThread := WatchThreadActive;
    {$ENDIF}
    try
      {$IFNDEF NO_THREADS}
      if WatchThreadActive then
        StopWatchThread;
      {$ENDIF}

      FValidateFlag := True;

      New(Info);
      Info^.StartNode := Node;
      Info^.SearchNewDirs := NewDirs;
      Info^.DriveType := DriveInfo[CurrentPath[1]].DriveType;

      SaveCanChange := FCanChange;
      FCanChange := True;
      FChangeFlag := False;
      IterateSubTree(Node, CallBackValidateDir, Recurse, coScanStartNode, Info);
      FValidateFlag := False;
      if (not Assigned(Selected)) and (Length(SelDir) > 0) then
        Directory := Copy(SelDir, 1, 3);
      if (SelDir <> Directory) and (not FChangeFlag) then
        Change(Selected);
      FCanChange := SaveCanChange;

      Dispose(Info);
    finally
      {$IFNDEF NO_THREADS}
      if RestartWatchThread and FWatchDirectory and not WatchThreadActive then
        StartWatchThread;
      {$ENDIF}

      if Screen.Cursor <> SaveCursor then
        Screen.Cursor := SaveCursor;
    end;
  end;
end; {ValidateDirectoryEx}

procedure TDriveView.ValidateDirectoryEasy(Node: TTreeNode);
begin
  if Assigned(Node) then
  begin
    if not Assigned(Node.Data) or (not TNodeData(Node.Data).Scanned) then
      ValidateDirectoryEx(Node, rsRecursiveExpanded, False);
  end;
end; {ValidateDirectoryEasy}

function  TDriveView.GetSubTreeSize(Node: TTreeNode): Integer;
var
  PSubSize: PInt;
  SaveCursor: TCursor;
begin
  Assert(Assigned(Node));

  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  ValidateAllDirectories(Node);
  RefreshDirSize(Node);
  New(PSubSize);
  PSubSize^ := 0;
  IterateSubTree(Node, CallBackSetDirSize, rsRecursive, coScanStartNode, PSubSize);
  Result := PSubSize^;
  Dispose(PSubSize);
  Screen.Cursor := SaveCursor;
end; {GetSubTreeSize}

function TDriveView.GetDriveTypeToNode(Node: TTreeNode): Integer;
begin
  Assert(Assigned(Node));

  Result := DriveInfo[NodePath(Node)[1]].DriveType
end; {GetDriveTypeToNode}

function TDriveView.GetDriveType(Drive: TDrive): Integer;           {Returns DRIVE_CDROM etc..}
begin
  Result := DriveInfo[UpCase(Drive)].DriveType;
end; {GetDriveType}

function TDriveView.NodeUpdateAble(Node: TTreeNode): Boolean;
begin
  Result := Assigned(Node) and Assigned(Node.Data) and (Node.Level > 0);
end; {NodeUpdateAble}

function  TDriveView.CallBackSaveNodeState(var Node: TTreeNode; Data: Pointer): Boolean;
begin
  Result := True;
  TNodeData(Node.Data).Expanded := Node.Expanded;
end; {CallBackSaveNodeState}

function  TDriveView.CallBackRestoreNodeState(Var Node: TTreeNode; Data: Pointer): Boolean;
begin
  Result := True;
  Node.Expanded := TNodeData(Node.Data).Expanded;
end; {CallBackRestoreNodeState}

procedure TDriveView.SaveNodesState(Node: TTreeNode);
begin
  IterateSubTree(Node, CallbackSaveNodeState, rsRecursive, coScanStartNode, nil);
end; {SaveNodesState}

procedure TDriveView.RestoreNodesState(Node: TTreeNode);
begin
  Items.BeginUpdate;
  IterateSubTree(Node, CallbackRestoreNodeState, rsRecursive, coScanStartNode, nil);
  Items.EndUpdate;
end; {RestoreNodesState}

function TDriveView.CreateDirectory(ParentNode: TTreeNode; NewName: string): TTreeNode;
var
  SRec: TSearchRec;
begin
  Assert(Assigned(ParentNode));

  Result := nil;
  if not TNodeData(ParentNode.Data).Scanned then
    ValidateDirectory(ParentNode);

{$IFNDEF NO_THREADS}
  StopWatchThread;
{$ENDIF}

  try
{$IFNDEF NO_THREADS}
    if Assigned(FDirView) then
      FDirView.StopWatchThread;
{$ENDIF}

    {create phyical directory:}
    LastIOResult := 0;
    if not Windows.CreateDirectory(PChar(NodePath(ParentNode) + '\' + NewName), nil) then
      LastIOResult := GetLastError;
    if LastIOResult = 0 then
    begin
      {Create treenode:}
      FindFirst(NodePath(ParentNode) + '\' + NewName, faAnyFile, SRec);
      Result := AddChildNode(ParentNode, Srec);
      FindClose(Srec);
      TNodeData(Result.Data).Scanned := True;
      SortChildren(ParentNode, False);
      ParentNode.Expand(False);
    end;
  finally
{$IFNDEF NO_THREADS}
    StartWatchThread;
{$ENDIF}
    if Assigned(FDirView) then
    begin
{$IFNDEF NO_THREADS}
      FDirView.StartWatchThread;
{$ENDIF}
      FDirView.Reload2;
    end;

  end;
end; {CreateDirectory}

function TDriveView.DeleteDirectory(Node: TTreeNode; AllowUndo: Boolean): Boolean;
var
  DelDir: string;
  OperatorResult: Boolean;
  FileOperator: TFileOperator;
  SaveCursor: TCursor;
begin
  Assert(Assigned(Node));

  Result := False;
  if Assigned(Node) and (Node.Level > 0) then
  begin
    SaveCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    FileOperator := TFileOperator.Create(Self);
    DelDir := NodePathName(Node);
    FileOperator.OperandFrom.Add(DelDir);
    FileOperator.Operation := foDelete;
    if AllowUndo then
      FileOperator.Flags := FileOperator.Flags + [foAllowUndo]
    else
      FileOperator.Flags := FileOperator.Flags - [foAllowUndo];

    if not ConfirmDelete then
      FileOperator.Flags := FileOperator.Flags + [foNoConfirmation];

    try
      if DirExists(DelDir) then
      begin
{$IFNDEF NO_THREADS}
        StopWatchThread;
{$ENDIF}
        OperatorResult := FileOperator.Execute;

        if OperatorResult and (not FileOperator.OperationAborted) and
           (not DirExists(DelDir)) then
        begin
          Node.Delete
        end
          else
        begin
          Result := False;
          if not AllowUndo then
          begin
            {WinNT4-Bug: FindFirst still returns the directories search record, even if the
             directory was deleted:}
            ChDir(DelDir);
            if IOResult <> 0 then
              Node.Delete;
          end;
        end;
      end
        else
      begin
        Node.Delete;
        Result := True;
      end;
    finally
{$IFNDEF NO_THREADS}
      StartWatchThread;
{$ENDIF}
      if Assigned(DirView) and Assigned(Selected) then
        DirView.Path := NodePathName(Selected);
      FileOperator.Free;
      Screen.Cursor := SaveCursor;
    end;
  end;
end; {DeleteDirectory}

{$IFNDEF NO_THREADS}
procedure TDriveView.CreateWatchThread(Drive: TDrive);
begin
  if csDesigning in ComponentState then
    Exit;

  if (not Assigned(DriveStatus[Drive].DiscMonitor)) and
     FWatchDirectory and
     (DriveInfo[Drive].DriveType <> DRIVE_REMOTE) and
     (Pos(Drive, FNoCheckDrives) = 0) then
  begin
    with DriveStatus[Drive] do
    begin
      DiscMonitor := TDiscMonitor.Create(Self);
      DiscMonitor.ChangeDelay := msThreadChangeDelay;
      DiscMonitor.SubTree := True;
      DiscMonitor.Filters := [moDirName];
      DiscMonitor.OnChange := ChangeDetected;
      DiscMonitor.OnInvalid := ChangeInvalid;
      DiscMonitor.SetDirectory(Drive + ':\');
      DiscMonitor.Open;
    end;
  end;
end; {CreateWatchThread}
{$ENDIF}

procedure TDriveView.SetWatchDirectory(Value: Boolean);
begin
  if FWatchDirectory <> Value then
  begin
    FWatchDirectory := Value;
{$IFNDEF NO_THREADS}
    if (not (csDesigning in ComponentState)) and Value then
      StartAllWatchThreads
    else
      StopAllWatchThreads;
{$ENDIF}
  end;
end; {SetAutoScan}

procedure TDriveView.SetDirView(Value: TDirView);
begin
  if Assigned(FDirView) then
    FDirView.DriveView := nil;

  FDirView := Value;

  if Assigned(FDirView) then
    FDirView.DriveView := Self;
end; {SetDirView}

procedure TDriveView.SetChangeInterval(Value: Cardinal);
var
  Drive: TDrive;
begin
  if Value > 0 then
  begin
    FChangeInterval := Value;
    for Drive := FirstDrive to LastDrive do
      with DriveStatus[Drive] do
        if Assigned(ChangeTimer) then
          ChangeTimer.Interval := Value;
  end;
end; {SetChangeInterval}

procedure TDriveView.SetNoCheckDrives(Value: string);
begin
  FNoCheckDrives := UpperCase(Value);
end; {SetNoCheckDrives}

procedure TDriveView.DeleteSubNodes(Node: TTreeNode);
begin
  if Assigned(Node) then
  begin
    Node.DeleteChildren;
    if Node.Level = 0 then
      DriveStatus[GetDriveToNode(Node)].Scanned := False;
    Node.HasChildren := False;
  end;
end; {DeleteSubNodes}

function TDriveView.NodeWatched(Node: TTreeNode): Boolean;
var
  Drive: TDrive;
begin
  Drive := GetDriveToNode(Node);
  Result := WatchThreadActive(Drive);
end; {NodeWatched}

procedure TDriveView.ChangeInvalid(Sender: TObject; const Directory: string;
  const ErrorStr: string);
var
  Dir: string;
begin
  Dir := (Sender as TDiscMonitor).Directories[0];
  with DriveStatus[Dir[1]] do
  begin
    DiscMonitor.Close;
    if Assigned(FOnChangeInvalid) then
      FOnChangeInvalid(Self, Dir[1]);
  end;
end; {DirWatchChangeInvalid}

procedure TDriveView.ChangeDetected(Sender: TObject; const Directory: string;
  var SubdirsChanged: Boolean);
var
  DirChanged: string;
begin
  if Sender is TDiscMonitor then
  begin
    DirChanged := (Sender as TDiscMonitor).Directories[0];
    if Length(DirChanged) > 0 then
    begin
      with DriveStatus[DirChanged[1]] do
      begin
        ChangeTimer.Interval := 0;
        ChangeTimer.Interval := FChangeInterval;
        ChangeTimer.Enabled  := True;
      end;
    end;
  end;
end; {DirWatchChangeDetected}

procedure TDriveView.ChangeTimerOnTimer(Sender: TObject);
var
  Node: TTreeNode;
  Drive: TDrive;
begin
  if (FChangeTimerSuspended = 0) and (Sender is TTimer) then
    with TTimer(Sender) do
    begin
      Drive := Chr(Tag);
      Node := FindNodeToPath(Drive + ':\');
      Interval := 0;
      Enabled := False;

      if Assigned(Node) then
      begin
        {Check also collapsed (invisible) subdirectories:}
        ValidateDirectory(Node);
        if Assigned(FOnChangeDetected) then
          FOnChangeDetected(Self, Drive);
      end;
    end;
end; {ChangeTimerOnTimer}

{$IFNDEF NO_THREADS}
procedure TDriveView.StartWatchThread;
var
  NewWatchedDir: string;
  Drive: TDrive;
begin
  if (csDesigning in ComponentState) or
     not Assigned(Selected) or
     not fWatchDirectory then Exit;

  NewWatchedDir := NodePathName(RootNode(Selected));
  Drive := Upcase(NewWatchedDir[1]);

  with DriveStatus[Drive] do
  begin
    if not Assigned(DiscMonitor) then
      CreateWatchThread(Drive);
    if Assigned(DiscMonitor) and not DiscMonitor.Enabled then
      DiscMonitor.Enabled := True;
  end;
end; {StartWatchThread}

procedure TDriveView.StopWatchThread;
begin
  if Assigned(Selected) then
    with DriveStatus[GetDriveToNode(Selected)] do
      if Assigned(DiscMonitor) then
        DiscMonitor.Enabled := False;
end; {StopWatchThread}

procedure TDriveView.SuspendChangeTimer;
begin
  Inc(FChangeTimerSuspended);
end;

procedure TDriveView.ResumeChangeTimer;
begin
  Assert(FChangeTimerSuspended > 0);
  Dec(FChangeTimerSuspended);
end;

procedure TDriveView.TerminateWatchThread(Drive: TDrive);
begin
  if Drive >= FirstDrive then
    with DriveStatus[Drive] do
      if Assigned(DiscMonitor) then
      begin
        DiscMonitor.Free;
        DiscMonitor := nil;
      end;
end; {StopWatchThread}

procedure TDriveView.StartAllWatchThreads;
var
  Drive: TDrive;
begin
  if (csDesigning in ComponentState) or (not FWatchDirectory) then
     Exit;

  for Drive := FirstFixedDrive to LastDrive do
    with DriveStatus[Drive] do
      if Scanned then
      begin
        if not Assigned(DiscMonitor) then
          CreateWatchThread(Drive);
        if Assigned(DiscMonitor) and (not DiscMonitor.Active) then
          DiscMonitor.Open;
      end;

  if Assigned(Selected) and (GetDriveToNode(Selected) < FirstFixedDrive) then
    StartWatchThread;
end; {StartAllWatchThreads}

procedure TDriveView.StopAllWatchThreads;
var
  Drive: TDrive;
begin
  for Drive := FirstDrive to LastDrive do
    with DriveStatus[Drive] do
    begin
      if Assigned(DiscMonitor) then
        DiscMonitor.Close;
    end;
end; {StopAllWatchThreads}

function TDriveView.WatchThreadActive(Drive: TDrive): Boolean;
begin
  Result := FWatchDirectory and
    Assigned(DriveStatus[Drive].DiscMonitor) and
    DriveStatus[Drive].DiscMonitor.Active and
    DriveStatus[Drive].DiscMonitor.Enabled;
end; {WatchThreadActive}

function TDriveView.WatchThreadActive: Boolean;
var
  Drive: TDrive;
begin
  if not Assigned(Selected) then
  begin
    Result := False;
    Exit;
  end;

  Drive := GetDriveToNode(Selected);

  Result := WatchThreadActive(Drive);
end; {WatchThreadActive}
{$ENDIF}

procedure TDriveView.SetFullDriveScan(DoFullDriveScan: Boolean);
begin
  FFullDriveScan := DoFullDriveScan;
end; {SetAutoScan}

function TDriveView.FindPathNode(Path: string): TTreeNode;
begin
  {Find existing path or parent path of not existing path:}
  repeat
    Result := FindNodeToPath(Path);
    if not Assigned(Result) then
      Path := ExtractFilePath(ExcludeTrailingBackslash(Path));
  until Assigned(Result) or (Length(Path) < 3);
end;

procedure TDriveView.SetDirectory(Value: string);
begin
  Value := IncludeTrailingBackslash(Value);
  FDirectory := Value;

  inherited;

  if Assigned(Selected) and (Selected.Level = 0) then
  begin
    if not DriveStatus[GetDriveToNode(Selected)].Scanned then
      ScanDrive(GetDriveToNode(Selected));
  end;
end; {SetDirectory}

procedure TDriveView.SetDrive(Drive: TDrive);
begin
  if GetDrive <> Drive then
    with DriveStatus[Drive] do
      if Assigned(RootNode) then
      begin
        if DefaultDir = EmptyStr then
          DefaultDir := Drive + ':\';
        if not Scanned then
          RootNode.Expand(False);
        TopItem := RootNode;
        Directory := IncludeTrailingBackslash(DefaultDir);
      end;
end; {SetDrive}

function  TDriveView.GetDrive: TDrive;
begin
  if Assigned(Selected) then
    Result := GetDriveToNode(Selected)
  else
    Result := #0;
end; {GetDrive}

function TDriveView.GetDirName(Node: TTreeNode): string;
begin
  if Assigned(Node) and Assigned(Node.Data) then
    Result := TNodeData(Node.Data).DirName
  else
    Result := '';
end; {GetDirName}

{GetDrive: returns the driveletter of the Node.}
function TDriveView.GetDriveToNode(Node: TTreeNode): Char;
var
  Path: string;
begin
  if (not Assigned (Node)) or (not Assigned(Node.Data)) then
    raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['GetDrive']));
  Path := NodePath(Node);
  if Length(Path) > 0 then
    Result := Upcase(Path[1])
  else
    Result := #0;
end; {GetDrive}

{RootNode: returns the rootnode to the Node:}
function TDriveView.RootNode(Node: TTreeNode): TTreeNode;
begin
  Result := Node;
  if not Assigned(Node) then
    raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['RootNode']));
  while Assigned(Result.Parent) do
    Result := Result.Parent;
end; {RootNode}

{NodeAttr: Returns the directory attributes to the node:}
function TDriveView.NodeAttr(Node: TTreeNode): Integer;
begin
  if not Assigned(Node) then
    raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['NodeAttr']));
  Result := TNodeData(Node.Data).Attr;
end; {NodeAttr}

function TDriveView.NodeVerified(Node: TTreeNode): Boolean;
begin
  if (not Assigned(Node)) or (not Assigned(Node.Data)) then
    raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['NodeVerified']));
  Result := TNodeData(Node.Data).Scanned;
end; {NodeVerified}

function  TDriveView.CallBackExpandLevel(var Node: TTreeNode; Data: Pointer): Boolean;
begin
  Result := True;
  if not Assigned(Node) then
    raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['CallBackExpandLevel']));
  if (Node.Level <= Integer(Data)) and (not Node.Expanded) then
    Node.Expand(False)
  else if (Node.Level > Integer(Data)) and Node.Expanded then
    Node.Collapse(True);
end; {CallBackExpandLevel}

procedure TDriveView.ExpandLevel(Node: TTreeNode; Level: Integer);
{Purpose: Expands all subnodes of node up to the given level}
begin
  if (not Assigned(Node)) or (not Assigned(Node.Data)) then
    raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['ExpandLevel']));
  Items.BeginUpdate;
  IterateSubTree(Node, CallBackExpandLevel, rsRecursive, coScanStartNode, Pointer(Level));
  Items.EndUpdate;
end; {ExpandLevel}

function  TDriveView.CallBackDisplayName(var Node: TTreeNode; Data: Pointer): Boolean;
begin
  Result := True;
  if not Assigned(Node) then
    raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['CallBackDisplayName']));
  Node.Text := GetDisplayName(Node);
end; {CallBackDisplayName}

function  TDriveView.CallBackSetDirSize(var Node: TTreeNode; Data: Pointer): Boolean;
begin
  Result := True;
  if Assigned(Node) then
  begin
    SetDirSize(Node);
    if FShowDirSize then
      Node.Text := GetDisplayName(Node);
    if Assigned(Data) then
      Inc(PInt(Data)^, TNodeData(Node.Data).DirSize);
  end;
  Application.ProcessMessages;
  if not FContinue then
    Exit;
end; {CallBackSetDirSize}

procedure TDriveView.SetShowDirSize(ShowIt: Boolean);
var
  Drive: Char;
  RootNode: TTreeNode;
  SaveCursor: TCursor;
begin
  if ShowIt = FShowDirSize then
    Exit;
  FShowDirSize := ShowIt;
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Items.BeginUpdate;
  for Drive := FirstFixedDrive to LastDrive do
  begin
    if DriveInfo[Drive].Valid then
    begin
      RootNode := DriveStatus[Drive].RootNode;
      if Assigned(RootNode) then
        IterateSubTree(RootNode, CallBackDisplayName, rsRecursive, coScanStartNode, nil);
    end;
  end;
  Items.EndUpdate;
  Screen.Cursor := SaveCursor;
end; {SetShowDirSize}

procedure TDriveView.RefreshDirSize(Node: TTreeNode);
begin
  if not Assigned(Node) then
    raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['RefreshDirSize']));
  CallBackSetDirSize(Node, nil);
end; {RefreshDirSize}

procedure TDriveView.RefreshDriveDirSize(Drive: TDrive);
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Items.BeginUpdate;
  with DriveStatus[Drive] do
  begin
    if Assigned(RootNode) then
      IterateSubTree(RootNode, CallBackSetDirSize, rsRecursive, coScanStartNode, nil);
  end;
  Items.EndUpdate;
  Screen.Cursor := SaveCursor;
end; {RefreshDriveDirSize}

function TDriveView.GetDirSize(Node: TTreeNode): Cardinal;
begin
  if (not Assigned(Node)) or (not Assigned(Node.Data)) then
    raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['GetDirSize']));
  if TNodeData(Node.Data).DirSize = CInvalidSize then
    SetDirSize(Node);
  Result := TNodeData(Node.Data).DirSize;
end; {GetDirSize}

procedure TDriveView.SetDirSize(Node: TTreeNode);
var
  SRec: TSearchRec;
  Size: Cardinal;
begin
  if (not Assigned(Node)) or (not Assigned(Node.Data)) then
    raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['SetDirSize']));
  Size := 0;
  if FindFirst(IncludeTrailingBackslash(NodePath(Node)) + '*.*', faAnyFile, SRec) = 0 then
  begin
    repeat
      if (SRec.Attr and faDirectory) = 0 then
        Inc(Size, SRec.Size);
    until FindNext(SRec) <> 0;
  end;
  FindClose(Srec);
  TNodeData(Node.Data).DirSize := Size;
end; {SetDirSize}

function TDriveView.GetDisplayName(Node: TTreeNode): string;
var
  DirName: string;
begin
  Result := '';
  if (not Assigned(Node)) or (not Assigned(Node.Data)) then
    raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['GetDisplayName']));

  if Node.Level = 0 then Result := GetDriveText(GetDriveToNode(Node))
    else
  begin
    DirName := GetDirName(Node);
    case FFileNameDisplay of
      fndCap: Result := UpperCase(DirName);
      fndNoCap: Result := LowerCase(DirName);
      fndNice: if Length(DirName) <= 8 then
               begin
                  Result := LowerCase(DirName);
                 Result[1] := Upcase(Result[1]);
               end
                 else Result := DirName;
      else
        Result := DirName;
    end; {Case}
  end;

  if FShowDirSize then
    Result := Result + ' = ' + FormatBytes(GetDirSize(Node), True, False);
end; {GetDisplayName}

procedure TDriveView.SetShowVolLabel(ShowIt: Boolean);
begin
  IF ShowIt = fShowVolLabel Then
  Exit;
  fShowVolLabel := ShowIt;
  RefreshRootNodes(False, dvdsFloppy);
End; {SetShowVolLabel}


procedure TDriveView.SetVolDisplayStyle(DoStyle: TVolumeDisplayStyle);
var
  Drive: TDrive;
begin
  if DoStyle <> fVolDisplayStyle then
  begin
    FVolDisplayStyle := DoStyle;
    if not FCreating then
      for Drive := FirstDrive to LastDrive do
        begin
          if DriveInfo[Drive].Valid then
            DriveStatus[Drive].RootNode.Text := GetDisplayName(DriveStatus[Drive].RootNode);
        end;
  end;
end; {SetVolDisplayStyle}

procedure TDriveView.SetCompressedColor(Value: TColor);
begin
  if Value <> FCompressedColor then
  begin
    FCompressedColor := Value;
    Invalidate;
  end;
end; {SetCompressedColor}

procedure TDriveView.SetFileNameDisplay(Value: TFileNameDisplay);
var
  Drive: TDrive;
begin
  if Value <> FFileNameDisplay then
  begin
    FFileNameDisplay := Value;
    for Drive := FirstDrive to LastDrive do
      with DriveStatus[Drive] do
        if Assigned(RootNode) and DriveStatus[Drive].Scanned then
          IterateSubTree(RootNode, CallBackDisplayName, rsRecursive, coNoScanStartNode, nil);
  end;
end; {SetFileNameDisplay}

procedure TDriveView.DisplayContextMenu(Node: TTreeNode; Point: TPoint);
var
  Verb: string;
  DirWatched: Boolean;
begin
{$IFNDEF NO_THREADS}
  DirWatched := NodeWatched(Node) and WatchThreadActive;
{$ELSE}
  DirWatched := False;
{$ENDIF}

  Assert(Node <> nil);

  if Node <> Selected then
    DropTarget := Node;

  Verb := EmptyStr;
  if Assigned(FOnDisplayContextMenu) then
    FOnDisplayContextMenu(Self);

  ShellDisplayContextMenu(FParentForm.Handle, Point, NodePathName(Node),
    CanEdit(Node), Verb, False);

  if Verb = shcRename then Node.EditText
    else
  if Verb = shcCut then
  begin
    LastClipBoardOperation := cboCut;
    LastPathCut := NodePathName(Node);
  end
    else
  if Verb = shcCopy then LastClipBoardOperation := cboCopy
    else
  if Verb = shcPaste then
    PasteFromClipBoard(NodePathName(Node));

  DropTarget := nil;

  if not DirWatched then
    ValidateDirectory(Node);
end; {DisplayContextMenu (2)}

procedure TDriveView.DisplayPropertiesMenu(Node: TTreeNode);
begin
  Assert(Assigned(Node));
  ShellExecuteContextCommand(FParentForm.Handle, shcProperties, NodePathName(Node));
end; {ContextMenu}

procedure TDriveView.SetSelected(Node: TTreeNode);
begin
  if Node <> Selected then
  begin
    FChangeFlag := False;
    FCanChange := True;
    inherited Selected := Node;
    if not FChangeFlag then
      Change(Selected);
  end;
end; {SetSelected}

{Called by TFileDeleteThread, when a file deletion was detected by the D&D receiving application:}
procedure TDriveView.SignalDirDelete(Sender: TObject; Files: TStringList);
begin
  if Files.Count > 0 then
    ValidateDirectory(FindNodeToPath(Files[0]));
end; {SignalDirDelete}

function TDriveView.DDSourceEffects: TDropEffectSet;
begin
  if FDragNode.Level = 0 then
    Result := [deLink]
  else
    Result := [deLink, deCopy, deMove];
end;

procedure TDriveView.DDChooseEffect(KeyState: Integer; var Effect: Integer);
var
  TargetDrive: Char;
begin
  if DropTarget = nil then Effect := DropEffect_None
    else
  if (KeyState and (MK_CONTROL or MK_SHIFT) = 0) then
  begin
    TargetDrive := NodePath(DropTarget)[1];

    if FExeDrag and (TargetDrive >= FirstFixedDrive) and (FDragDrive >= FirstFixedDrive) then
    begin
      Effect := DropEffect_Link;
    end
      else
    if (Effect = DropEffect_Copy) and
       ((DragDrive = GetDriveToNode(DropTarget)) and
         (FDragDropFilesEx.AvailableDropEffects and DropEffect_Move <> 0)) then
    begin
      Effect := DropEffect_Move;
    end;
  end;

  inherited;
end;

function TDriveView.DragCompleteFileList: Boolean;
begin
  Result := (GetDriveType(NodePathName(FDragNode)[1]) <> DRIVE_REMOVABLE);
end;

function TDriveView.DDExecute: TDragResult;
{$IFNDEF NO_THREADS}
var
  WatchThreadOK: Boolean;
  DragParentPath: string;
  DragPath: string;
{$ENDIF}
begin
{$IFNDEF NO_THREADS}
  WatchThreadOK := WatchThreadActive;
{$ENDIF}

  Result := FDragDropFilesEx.Execute(nil);

  {$IFNDEF NO_THREADS}
  if (Result = drMove) and (not WatchThreadOK) then
  begin
    DragPath := NodePathName(FDragNode);
    if Assigned(FDragNode.Parent) then
      DragParentPath := NodePathName(FDragNode.Parent)
    else
      DragParentPath := DragPath;

    if (FDragNode.Level > 0) or (DragParentPath <> NodePathName(Selected.Parent)) then
    begin
      FDragNode := FindNodeToPath(DragPath);
      if Assigned(FDragNode) then
      begin
        FDragFileList.Clear;
        FDragFileList.Add(DragPath);
        TFileDeleteThread.Create(FDragFileList, MaxWaitTimeOut, SignalDirDelete);
      end;
    end;
  end;
{$ENDIF}
end;

procedure TDriveView.PerformDragDropFileOperation(Node: TTreeNode; Effect: Integer);
var
  i: Integer;
  SourcePath: string;
  SourceParentPath: string;
  SourceFile: string;
  SaveCursor: TCursor;
  DoFileOperation: Boolean;
  TargetNode: TTreeNode;
  FileNamesAreMapped: Boolean;
  TargetPath: string;
  IsRecycleBin: Boolean;
begin
  TargetPath := NodePathName(Node);
  IsRecycleBin := NodeIsRecycleBin(Node);

  if FDragDropFilesEx.FileList.Count = 0 then
    Exit;

  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  SourcePath := EmptyStr;

  try
    if (Effect = DropEffect_Copy) or (Effect = DropEffect_Move) then
    begin
{$IFNDEF NO_THREADS}
      StopAllWatchThreads;
      if Assigned(FDirView) then
        FDirView.StopWatchThread;

      if Assigned(DropSourceControl) and
         (DropSourceControl is TDirView) and
         (DropSourceControl <> FDirView) then
      begin
        TDirView(DropSourceControl).StopWatchThread;
      end;
{$ENDIF}

      FileNamesAreMapped := (TFDDListItem(FDragDropFilesEx.FileList[0]^).MappedName <> '');

      {Set the source directory:}
      for i := 0 to FDragDropFilesEx.FileList.Count - 1 do
      begin
        FFileOperator.OperandFrom.Add(
          TFDDListItem(FDragDropFilesEx.FileList[i]^).Name);

        if FileNamesAreMapped then
          FFileOperator.OperandTo.Add(IncludeTrailingBackslash(TargetPath) +
            TFDDListItem(FDragDropFilesEx.FileList[i]^).MappedName);
      end;

      SourcePath := TFDDListItem(FDragDropFilesEx.FileList[0]^).Name;
      SourceParentPath := ExtractFilePath(ExcludeTrailingBackslash(SourcePath));

      ClearDragFileList(FDragDropFilesEx.FileList);

      FFileOperator.Flags := [foAllowUndo, foNoConfirmMkDir];

      {Set the target directory or target files:}
      if FileNamesAreMapped and (not IsRecycleBin) then
      begin
        FFileOperator.Flags := FFileOperator.Flags + [foMultiDestFiles]
      end
        else
      begin
        FFileOperator.Flags := FFileOperator.Flags - [foMultiDestFiles];
        FFileOperator.OperandTo.Clear;
        FFileOperator.OperandTo.Add(TargetPath);
      end;

      if IsRecycleBin then FFileOperator.Operation := foDelete
        else
      case Effect of
        DropEffect_Copy: FFileOperator.Operation := foCopy;
        DropEffect_Move: FFileOperator.Operation := foMove;
      end;  {Case}

      if IsRecycleBin then
      begin
        if not ConfirmDelete then
          FFileOperator.Flags := FFileOperator.Flags + [foNoConfirmation];
      end
        else
      if not ConfirmOverwrite then
        FFileOperator.Flags := FFileOperator.Flags + [foNoConfirmation];

      DoFileOperation := True;
      if Assigned(FOnDDFileOperation) then
        FOnDDFileOperation(Self, Effect, SourcePath, TargetPath, DoFileOperation);

      if DoFileOperation and (FFileOperator.OperandFrom.Count > 0) then
      begin
        FFileOperator.Execute;
        if Assigned(FOnDDFileOperationExecuted) then
          FOnDDFileOperationExecuted(Self, Effect, SourcePath, TargetPath);
        if FileNamesAreMapped then
          FFileOperator.ClearUndo;
      end;
    end
      else
    if Effect = DropEffect_Link then
    { Create Link requested: }
    begin
      for i := 0 to FDragDropFilesEx.FileList.Count - 1 do
      begin
        SourceFile := TFDDListItem(FDragDropFilesEx.FileList[i]^).Name;
        if Length(SourceFile) = 3 then
          SourcePath := Copy(DriveInfo[SourceFile[1]].PrettyName, 4, 255) + '(' + SourceFile[1] + ')'
        else
          SourcePath := ExtractFileName(SourceFile);

        if not CreateFileShortCut(SourceFile,
            IncludeTrailingBackslash(TargetPath) + ChangeFileExt(SourcePath, '.lnk'),
            ExtractFileNameOnly(SourceFile)) then
        begin
          DDError(DDCreateShortCutError);
        end;
      end;
    end;

    if Effect = DropEffect_Move then
      Items.BeginUpdate;

    {Update source directory, if move-operation was performed:}
    if ((Effect = DropEffect_Move) or IsRecycleBin) then
      ValidateDirectory(FindNodeToPath(SourceParentPath));

    {Update subdirectories of target directory:}
    TargetNode := FindNodeToPath(TargetPath);
    if Assigned(TargetNode) then
      ValidateDirectory(TargetNode)
    else
      ValidateDirectory(DriveStatus[TargetPath[1]].RootNode);

    if Effect = DropEffect_Move then
      Items.EndUpdate;

    {Update linked component TDirView:}
    if Assigned(FDirView)
{$IFNDEF NO_THREADS}
        and not FDirView.WatchThreadActive
{$ENDIF}
        then
    begin
      case Effect of
        DropEffect_Copy,
        DropEffect_Link:
          if (IncludeTrailingBackslash(TargetPath) = IncludeTrailingBackslash(DirView.Path)) then
            FDirView.Reload2;

        DropEffect_Move:
          if (IncludeTrailingBackslash(TargetPath) = IncludeTrailingBackslash(DirView.Path)) or
             (IncludeTrailingBackslash(SourceParentPath) = IncludeTrailingBackslash(DirView.Path)) then
          begin
            if FDirView <> DropSourceControl then FDirView.Reload2;
          end;
      end; {Case}
    end;

    {Update the DropSource control, if files are moved and it is a TDirView:}
    if (Effect = DropEffect_Move) and (DropSourceControl is TDirView) then
      TDirView(DropSourceControl).ValidateSelectedFiles;

  finally
    FFileOperator.OperandFrom.Clear;
    FFileOperator.OperandTo.Clear;
{$IFNDEF NO_THREADS}
    StartAllWatchThreads;

    if Assigned(FDirView) and (not FDirView.WatchThreadActive) then
      FDirView.StartWatchThread;
    if Assigned(DropSourceControl) and (DropSourceControl is TDirView) and
       (not TDirView(DropSourceControl).WatchThreadActive) then
      TDirView(DropSourceControl).StartWatchThread;
{$ENDIF}
    Screen.Cursor := SaveCursor;
  end;
end; {PerformDragDropFileOperation}

function TDriveView.GetCanUndoCopyMove: Boolean;
begin
  Result := Assigned(FFileOperator) and FFileOperator.CanUndo;
end; {CanUndoCopyMove}

function TDriveView.UndoCopyMove: Boolean;
var
  LastTarget: string;
  LastSource: string;
begin
  Result := False;
  if FFileOperator.CanUndo then
  begin
    Lasttarget := FFileOperator.LastOperandTo[0];
    LastSource := FFileOperator.LastOperandFrom[0];
{$IFNDEF NO_THREADS}
    StopAllWatchThreads;
{$ENDIF}
    Result := FFileOperator.UndoExecute;

    ValidateDirectory(FindNodeToPath(ExtractFilePath(LastTarget)));
    ValidateDirectory(FindNodeToPath(ExtractFilePath(LastSource)));
{$IFNDEF NO_THREADS}
    StartAllWatchThreads;
{$ENDIF}

    if Assigned(FDirView) then
      with FDirView do
{$IFNDEF NO_THREADS}
        if not WatchThreadActive then
{$ENDIF}
        begin
          if (IncludeTrailingBackslash(ExtractFilePath(LastTarget)) = IncludeTrailingBackslash(Path)) or
             (IncludeTrailingBackslash(ExtractFilePath(LastSource)) = IncludeTrailingBackslash(Path)) then
            Reload2;
        end;
      end;
end; {UndoCopyMove}

{Clipboard operations:}
procedure TDriveView.SetLastPathCut(Path: string);
var
  Node: TTreeNode;
begin
  if FLastPathCut <> Path then
  begin
    Node := FindNodeToPath(FLastPathCut);
    if Assigned(Node) then
    begin
      FLastPathCut := Path;
      Node.Cut := False;
    end;
    Node := FindNodeToPath(Path);
    if Assigned(Node) then
    begin
      FLastPathCut := Path;
      Node.Cut := True;
    end;
  end;
end; {SetLastNodeCut}

procedure TDriveView.EmptyClipboard;
begin
  if Windows.OpenClipBoard(0) then
  begin
    Windows.EmptyClipBoard;
    Windows.CloseClipBoard;
    LastPathCut := '';
    LastClipBoardOperation := cboNone;
    if Assigned(FDirView) then
      FDirView.EmptyClipboard;
  end;
end; {EmptyClipBoard}

function TDriveView.CopyToClipBoard(Node: TTreeNode): Boolean;
begin
  Result := Assigned(Selected);
  if Result then
  begin
    EmptyClipBoard;
    ClearDragFileList(FDragDropFilesEx.FileList);
    AddToDragFileList(FDragDropFilesEx.FileList, Selected);
    Result := FDragDropFilesEx.CopyToClipBoard;
    LastClipBoardOperation := cboCopy;
  end;
end; {CopyToClipBoard}

function TDriveView.CutToClipBoard(Node: TTreeNode): Boolean;
begin
  Result := Assigned(Node) and (Node.Level > 0) and CopyToClipBoard(Node);
  if Result then
  begin
    LastPathCut := NodePathName(Node);
    LastClipBoardOperation := cboCut;
  end;
end; {CutToClipBoard}

function TDriveView.CanPasteFromClipBoard: Boolean;
begin
  Result := False;
  if Assigned(Selected) and Windows.OpenClipboard(0) then
  begin
    Result := IsClipboardFormatAvailable(CF_HDROP);
    Windows.CloseClipBoard;
  end;
end; {CanPasteFromClipBoard}

function TDriveView.PasteFromClipBoard(TargetPath: String = ''): Boolean;
begin
  ClearDragFileList(FDragDropFilesEx.FileList);
  Result := False;
  if CanPasteFromClipBoard and
    {MP}{$IFDEF OLD_DND} FDragDropFilesEx.GetFromClipBoard {$ELSE} FDragDropFilesEx.PasteFromClipboard {$ENDIF}{/MP}
    then
  begin
    if TargetPath = '' then
      TargetPath := NodePathName(Selected);
    case LastClipBoardOperation of
      cboCopy,
      cboNone:
        begin
          PerformDragDropFileOperation(Selected, DropEffect_Copy);
          if Assigned(FOnDDExecuted) then
            FOnDDExecuted(Self, DropEffect_Copy);
        end;
      cboCut:
        begin
          PerformDragDropFileOperation(Selected, DropEffect_Move);
          if Assigned(FOnDDExecuted) then
            FOnDDExecuted(Self, DropEffect_Move);
          EmptyClipBoard;
        end;
    end;
    Result := True;
  end;
end; {PasteFromClipBoard}

end.
