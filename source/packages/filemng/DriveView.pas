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
  CustomDriveView, System.Generics.Collections;

const
  msThreadChangeDelay = 50;

  ErrorNodeNA = '%s: Node not assigned';

  {Flags used by TDriveView.RefreshRootNodes:}
  dvdsFloppy          = 8;  {Include floppy drives}
  dvdsRereadAllways   = 16; {Refresh drivestatus in any case}

type
  EInvalidDirName  = class(Exception);
  ENodeNotAssigned = class(Exception);

  TDriveStatus = class
    Scanned: Boolean;          {Drive allready scanned?}
    Verified: Boolean;         {Drive completly scanned?}
    RootNode: TTreeNode;       {Rootnode to drive}
    RootNodeIndex: Integer;
    DiscMonitor: TDiscMonitor; {Monitor thread}
    ChangeTimer: TTimer;       {Change timer for the monitor thread}
    DefaultDir: string;        {Current directory}
  end;

  TScanDirInfo = record
    SearchNewDirs: Boolean;
    StartNode: TTreeNode;
    DriveType: Integer;
  end;

  PScanDirInfo = ^TScanDirInfo;

  TDriveView = class;

  TNodeData = class
  private
    FDirName: string;
    FShortName: string;
    FAttr: Integer;
    FScanned: Boolean;
    FData: Pointer;
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
    property IsRecycleBin: Boolean read FIsRecycleBin;
    property IconEmpty: Boolean read FIconEmpty write FIconEmpty;
  end;

  TDriveTreeNode = class(TTreeNode)
    procedure Assign(Source: TPersistent); override;
  end;

  TDriveView = class(TCustomDriveView)
  private
    FDriveStatus: TObjectDictionary<string, TDriveStatus>;

    FConfirmDelete: Boolean;
    FConfirmOverwrite: Boolean;
    FWatchDirectory: Boolean;
    FDirectory: string;
    FFullDriveScan: Boolean;
    FShowVolLabel: Boolean;
    FVolDisplayStyle: TVolumeDisplayStyle;
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

    {Additional events:}
    FOnDisplayContextMenu: TNotifyEvent;
    FOnRefreshDrives: TNotifyEvent;
    FOnNeedHiddenDirectories: TNotifyEvent;

    {used components:}
    FDirView: TDirView;
    FFileOperator: TFileOperator;

    FChangeInterval: Cardinal;

    {Drag&drop:}
    FLastPathCut: string;

    {Drag&drop helper functions:}
    procedure SignalDirDelete(Sender: TObject; Files: TStringList);

    function CheckForSubDirs(Path: string): Boolean;
    function ReadSubDirs(Node: TTreeNode; DriveType: Integer): Boolean;

    {Callback-functions used by iteratesubtree:}
    function CallBackValidateDir(var Node: TTreeNode; Data: Pointer): Boolean;

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
    procedure SetShowVolLabel(ShowIt: Boolean);
    procedure SetDirView(Value: TDirView);
    procedure SetDirectory(Value: string); override;
    procedure GetNodeShellAttr(ParentNode: TTreeNode; NodeData: TNodeData; GetAttr: Boolean);
    function  DoScanDir(FromNode: TTreeNode): Boolean;
    function  AddChildNode(ParentNode: TTreeNode; SRec: TSearchRec): TTreeNode;
    procedure CreateWatchThread(Drive: string);
    function NodeWatched(Node: TTreeNode): Boolean;
    procedure TerminateWatchThread(Drive: string);
    function WatchThreadActive: Boolean; overload;
    function WatchThreadActive(Drive: string): Boolean; overload;
    procedure InternalWndProc(var Msg: TMessage);

    function DirAttrMask: Integer;
    function CreateDriveStatus: TDriveStatus;

    procedure ValidateDirectoryEx(Node: TTreeNode; Recurse: TRecursiveScan;
      NewDirs: Boolean); override;
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
    procedure DDChooseEffect(KeyState: Integer; var Effect: Integer; PreferredEffect: Integer); override;
    function DragCompleteFileList: Boolean; override;
    function DDExecute: TDragResult; override;

  public
    property Images;
    property StateImages;
    property Items stored False;
    property Selected Write SetSelected stored False;

    property DragImageList: TDragImageList read FDragImageList;

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
    function GetDriveStatus(Drive: string): TDriveStatus;
    function GetDriveTypetoNode(Node: TTreeNode): Integer;  {Returns DRIVE_CDROM etc..}
    function GetDriveToNode(Node: TTreeNode): string;
    function GetDriveText(Drive: string): string;
    procedure ScanDrive(Drive: string);
    procedure RefreshRootNodes(dsFlags: Integer);
    function GetDrives: TStrings;

    {Node handling:}
    procedure SetImageIndex(Node: TTreeNode); virtual;
    function FindNodeToPath(Path: string): TTreeNode;
    function RootNode(Node: TTreeNode): TTreeNode;
    function GetDirName(Node: TTreeNode): string;
    function GetDisplayName(Node: TTreeNode): string;
    function NodePathName(Node: TTreeNode): string; override;

    function GetFQPIDL(Node: TTreeNode): PItemIDList;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {Menu-handling:}
    procedure DisplayContextMenu(Node: TTreeNode; Point: TPoint); override;

    procedure DisplayPropertiesMenu(Node: TTreeNode); override;

    {Watchthread handling:}
    procedure StartWatchThread;
    procedure StopWatchThread;
    procedure SuspendChangeTimer;
    procedure ResumeChangeTimer;
    procedure StartAllWatchThreads;
    procedure StopAllWatchThreads;
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
    {Linked component TDirView:}
    property DirView: TDirView read FDirView write SetDirView;
    {Show the volume labels of drives:}
    property ShowVolLabel: Boolean read FShowVolLabel write SetShowVolLabel default True;
    {Additional events:}
    property OnDisplayContextMenu: TNotifyEvent read FOnDisplayContextMenu
      write FOnDisplayContextMenu;
    property OnRefreshDrives: TNotifyEvent read FOnRefreshDrives
      write FOnRefreshDrives;
    property OnBusy;

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
    property OnNeedHiddenDirectories: TNotifyEvent read FOnNeedHiddenDirectories write FOnNeedHiddenDirectories;
  end;

procedure Register;

implementation

uses
  CompThread, PasTools, UITypes, Types, OperationWithTimeout, System.Generics.Defaults;

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
  FScanned := False;
  FDirName := '';
  FShortName := '';
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
    NewData.FIsRecycleBin := SourceData.FIsRecycleBin;
    NewData.IconEmpty := SourceData.IconEmpty;
    TTreeNode(Source).Data := NewData;
  end;
end;

  { TDriveView }

constructor TDriveView.Create(AOwner: TComponent);
var
  Drive: TRealDrive;
begin
  inherited;

  FCreating := True;

  FDriveStatus := TObjectDictionary<string, TDriveStatus>.Create([doOwnsValues]);
  FChangeInterval := MSecsPerSec;

  for Drive := FirstDrive to LastDrive do
  begin
    FDriveStatus.Add(Drive, CreateDriveStatus);
  end;

  FFileOperator := TFileOperator.Create(Self);
  FFileOperator.Flags := [foAllowUndo, foNoConfirmMkDir];

  FShowVolLabel := True;
  FChangeFlag := False;
  FLastDir := EmptyStr;
  FValidateFlag := False;
  FConfirmDelete := True;
  FDirectory := EmptyStr;
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
  DriveStatus: TDriveStatus;
begin
  Classes.DeallocateHWnd(FInternalWindowHandle);

  for DriveStatus in FDriveStatus.Values do
  begin
    with DriveStatus do
    begin
      if Assigned(DiscMonitor) then
        DiscMonitor.Free;
      if Assigned(ChangeTimer) then
        ChangeTimer.Free;
    end;
  end;
  FDriveStatus.Free;

  if Assigned(FFileOperator) then
    FFileOperator.Free;

  inherited Destroy;
end; {Destroy}

function TDriveView.CreateDriveStatus: TDriveStatus;
begin
  Result := TDriveStatus.Create;
  with Result do
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
  end;
end;

type
  PDevBroadcastHdr = ^TDevBroadcastHdr;
  TDevBroadcastHdr = record
    dbch_size: DWORD;
    dbch_devicetype: DWORD;
    dbch_reserved: DWORD;
  end;

  PDevBroadcastVolume = ^TDevBroadcastVolume;
  TDevBroadcastVolume = record
    dbcv_size: DWORD;
    dbcv_devicetype: DWORD;
    dbcv_reserved: DWORD;
    dbcv_unitmask: DWORD;
    dbcv_flags: WORD;
  end;

const
  DBT_CONFIGCHANGED = $0018;
  DBT_DEVICEARRIVAL = $8000;
  DBT_DEVICEREMOVEPENDING = $8003;
  DBT_DEVICEREMOVECOMPLETE = $8004;
  DBT_DEVTYP_VOLUME = $00000002;

procedure TDriveView.InternalWndProc(var Msg: TMessage);
var
  UnitMask: DWORD;
  Drive: Char;
begin
  with Msg do
  begin
    if Msg = WM_DEVICECHANGE then
    begin
       if (wParam = DBT_CONFIGCHANGED) or
          (wParam = DBT_DEVICEARRIVAL) or
          (wParam = DBT_DEVICEREMOVECOMPLETE) then
      begin
        // Delay refreshing drives for a sec.
        // Particularly with CD/DVD drives, if we query display name
        // immediately after receiving DBT_DEVICEARRIVAL, we do not get media label.
        // Actually one sec does not help usually, but we do not want to wait any longer,
        // because we want to add USB drives asap.
        SetTimer(FInternalWindowHandle, 1, MSecsPerSec, nil);
      end
        else
      if wParam = DBT_DEVICEREMOVEPENDING then
      begin
        if PDevBroadcastHdr(lParam)^.dbch_devicetype = DBT_DEVTYP_VOLUME then
        begin
          UnitMask := PDevBroadcastVolume(lParam)^.dbcv_unitmask;
          Drive := FirstDrive;
          while UnitMask > 0 do
          begin
            if UnitMask and $01 <> 0 then
            begin
              // Disable disk monitor to release the handle to the drive.
              // It may happen that the dirve is not removed in the end. In this case we do not currently resume the
              // monitoring. We can watch for DBT_DEVICEQUERYREMOVEFAILED to resume the monitoring.
              // But currently we implement this for VeraCrypt, which does not send this notification.
              with GetDriveStatus(Drive) do
              begin
                if Assigned(DiscMonitor) then
                begin
                  DiscMonitor.Enabled := False;
                  DiscMonitor.Free;
                  DiscMonitor := nil;
                end;
              end;
            end;
            UnitMask := UnitMask shr 1;
            Drive := Chr(Ord(Drive) + 1);
          end;
        end;
      end;
    end
      else
    if Msg = WM_TIMER then
    begin
      KillTimer(FInternalWindowHandle, 1);
      try
        //DriveInfo.Load;
        RefreshRootNodes(dsAll or dvdsRereadAllways);
        if Assigned(OnRefreshDrives) then
          OnRefreshDrives(Self);
      except
        Application.HandleException(Self);
      end;
    end;

    Result := DefWindowProc(FInternalWindowHandle, Msg, wParam, lParam);
  end;
end;

procedure TDriveView.CreateWnd;
var
  DriveStatus: TDriveStatus;
begin
  inherited;

  if Assigned(PopupMenu) then
    PopupMenu.Autopopup := False;

  OLECheck(SHGetDesktopFolder(FDesktop));

  FDragDropFilesEx.SourceEffects := [deCopy, deMove, deLink];
  FDragDropFilesEx.TargetEffects := [deCopy, deMove, deLink];

  if FPrevSelectedIndex >= 0 then
  begin
    FPrevSelected := Items[FPrevSelectedIndex];
    FPrevSelectedIndex := -1;
  end;

  for DriveStatus in FDriveStatus.Values do
    with DriveStatus do
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
  DriveStatus: TDriveStatus;
begin
  if CreateWndRestores and (Items.Count > 0) and (csRecreating in ControlState) then
  begin
    FPrevSelectedIndex := -1;
    if Assigned(FPrevSelected) then
    begin
      FPrevSelectedIndex := FPrevSelected.AbsoluteIndex;
      FPrevSelected := nil;
    end;

    for DriveStatus in FDriveStatus.Values do
      with DriveStatus do
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
    shAttr := 0;
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
      begin
        if SupportsDarkMode and DarkMode then Result := clSkyBlue
          else Result := clBlue;
      end
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

  if IsRootPath(Result) then
    Result := ExcludeTrailingBackslash(Result);
end;

{NodePathName: Returns the complete path to Node with trailing backslash on rootnodes:
 C:\ ,C:\WINDOWS, C:\WINDOWS\SYSTEM  }
function TDriveView.NodePathName(Node: TTreeNode): string;
begin
  Result := NodePath(Node);
  if IsRootPath(Result) then
    Result := IncludeTrailingBackslash(Result);
end; {NodePathName}

function TDriveView.NodeIsRecycleBin(Node: TTreeNode): Boolean;
begin
  Result := TNodeData(Node.Data).IsRecycleBin;
end;

function TDriveView.NodePathExists(Node: TTreeNode): Boolean;
begin
  Result := DirectoryExists(NodePathName(Node));
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

      if Length(Item.pszText) > 0 then
        raise EInvalidDirName.Create(SErrorInvalidName + Space + Info);
      Exit;
    end;

    StopWatchThread;
    if Assigned(DirView) then
      DirView.StopWatchThread;

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
        if FindFirst(ApiPath(IncludeTrailingBackslash(NodePath(Node.Parent)) + Item.pszText),
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
      StartWatchThread;
      if Assigned(DirView) then
      begin
        DirView.Reload2;
        DirView.StartWatchThread;
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
  Drive: string;
  SaveCursor: TCursor;
begin
  Result := inherited CanExpand(Node);
  Drive := GetDriveToNode(Node);
  if Node.HasChildren then
  begin
    if (Node.Level = 0) and
       (not GetDriveStatus(Drive).Scanned) and
       DriveInfo.IsFixedDrive(Drive) then
    begin
      SubNode := Node.GetFirstChild;
      if not Assigned(SubNode) then
      begin
        ScanDrive(Drive);
        SubNode := Node.GetFirstChild;
        Node.HasChildren := Assigned(SubNode);
        Result := Node.HasChildren;
        if not Assigned(GetDriveStatus(Drive).DiscMonitor) then
          CreateWatchThread(Drive);
      end;
    end
      else
    begin
      SaveCursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
      try
        if (not TNodeData(Node.Data).Scanned) and DoScanDir(Node) then
        begin
          ReadSubDirs(Node, DriveInfo.Get(Drive).DriveType);
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
  RefreshRootNodes(dsDisplayName or dvdsFloppy);
  {Set the initial directory:}
  if (Length(FDirectory) > 0) and DirectoryExists(FDirectory) then
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
  Drive: string;
begin
  Result := inherited CanChange(Node);

  if not Reading and not (csRecreating in ControlState) then
  begin
    if Result and Assigned(Node) then
    begin
      Path := NodePathName(Node);
      if Path <> FLastDir then
      begin
        Drive := DriveInfo.GetDriveKey(Path);

        DriveInfo.ReadDriveStatus(Drive, dsSize or dsImageIndex);
        if not DriveInfo.Get(Drive).DriveReady then
        begin
          MessageDlg(Format(SDriveNotReady, [Drive]), mtError, [mbOK], 0);
          Result := False;
        end
          else
        try
          CheckCanOpenDirectory(Path);
        except
          Application.HandleException(Self);
          Result := False;
        end;
      end;
    end;

    if Result and (csDestroying in ComponentState) then
    begin
      Result := False;
    end;

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
    begin
      DropTarget := nil;
    end;
  end;
end; {CanChange}

procedure TDriveView.Change(Node: TTreeNode);
var
  Drive: string;
  OldSerial: DWORD;
  NewDir: string;
  LastDrive: string;
begin
  if not Reading and not (csRecreating in ControlState) then
  begin
    if Assigned(Node) then
    begin
      NewDir := NodePathName(Node);
      if NewDir <> FLastDir then
      begin
        Drive := DriveInfo.GetDriveKey(NewDir);
        if Length(FLastDir) > 0 then
          LastDrive := DriveInfo.GetDriveKey(FLastDir)
        else
          LastDrive := '';

        FChangeFlag := True;
        FLastDir := NewDir;

        OldSerial := DriveInfo.Get(Drive).DriveSerial;
        DriveInfo.ReadDriveStatus(Drive, dsSize or dsImageIndex);
        with DriveInfo.Get(Drive) do
        begin
          if Assigned(FDirView) and (FDirView.Path <> NewDir) then
            FDirView.Path := NewDir;

          if DriveReady then
          begin
            if not DirectoryExists(NewDir) then
            begin
              ValidateDirectory(GetDriveStatus(Drive).RootNode);
              Exit;
            end;

            GetDriveStatus(Drive).DefaultDir := IncludeTrailingBackslash(NewDir);

            if LastDrive <> Drive then
            begin
              if (LastDrive <> '') and
                 (DriveInfo.Get(LastDrive).DriveType = DRIVE_REMOVABLE) then
              begin
                TerminateWatchThread(LastDrive);
              end;

              {Drive serial has changed or is missing: allways reread the drive:}
              if (DriveSerial <> OldSerial) or (DriveSerial = 0) then
              begin
                if TNodeData(GetDriveStatus(Drive).RootNode.Data).Scanned then
                  ScanDrive(Drive);
              end;
            end;
            StartWatchThread;
          end
            else  {Drive not ready:}
          begin
            GetDriveStatus(Drive).RootNode.DeleteChildren;
            GetDriveStatus(Drive).DefaultDir := EmptyStr;
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
  Drive, NodePath: string;
begin
  if Assigned(Node) and TNodeData(Node.Data).IconEmpty then
  begin
    NodePath := NodePathName(Node);
    Drive := DriveInfo.GetDriveKey(NodePath);
    if Node.Level = 0 then
    begin
      with DriveInfo.Get(Drive) do
      begin
        if ImageIndex = 0 then
        begin
          DriveInfo.ReadDriveStatus(Drive, dsImageIndex);
          Node.ImageIndex := DriveInfo.Get(Drive).ImageIndex;
        end
          else Node.ImageIndex := ImageIndex;
        Node.SelectedIndex := Node.ImageIndex;
      end;
    end
      else
    begin
      if DriveInfo.Get(Drive).DriveType = DRIVE_REMOTE then
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

function TDriveView.GetDriveText(Drive: string): string;
begin
  if FShowVolLabel and (Length(DriveInfo.GetPrettyName(Drive)) > 0) then
  begin
    case FVolDisplayStyle of
      doPrettyName:  Result := DriveInfo.GetPrettyName(Drive);
      doDisplayName: Result := DriveInfo.GetDisplayName(Drive);
    end; {Case}
  end
    else
  begin
    Result := DriveInfo.GetSimpleName(Drive);
  end;
end; {GetDriveText}

procedure TDriveView.GetNodeShellAttr(ParentNode: TTreeNode; NodeData: TNodeData; GetAttr: Boolean);
var
  ParentFolder: IShellFolder;
  ParentData: TNodeData;
begin
  NodeData.shAttr := 0;

  if GetAttr then
  begin
    if Assigned(ParentNode) then
    begin
      ParentData := TNodeData(ParentNode.Data);
      if not Assigned(ParentData) then
      begin
        Assert(False);
        ParentFolder := nil;
      end
        else
      begin
        if not Assigned(ParentData.ShellFolder) then
        begin
          GetNodeShellAttr(ParentNode.Parent, ParentData, GetAttr);
        end;
        ParentFolder := ParentData.ShellFolder;
      end;
    end
      else
    begin
      ParentFolder := FDesktop;
    end;

    if Assigned(ParentFolder) and Assigned(NodeData) then
    begin
      if not Assigned(NodeData.PIDL) then
        NodeData.PIDL := PIDL_GetFromParentFolder(ParentFolder, PChar(NodeData.DirName));
      if Assigned(NodeData.PIDL) then
      begin
        NodeData.shAttr := SFGAO_CONTENTSMASK;

        // Previously we would also make use of SFGAO_SHARE to display a share overlay.
        // But for directories, Windows File Explorer does not display the overlay anymore (probably since Vista).
        // And for drives (where Explorer does display the overlay), it did not work ever since we use "desktop"
        // (and not "workspace" as before) to resolve drive interface (see Bug 1717).
        if not Succeeded(ShellFolderGetAttributesOfWithTimeout(ParentFolder, 1, NodeData.PIDL, NodeData.shAttr, MSecsPerSec)) then
        begin
          NodeData.shAttr := 0;
        end;

        if not Assigned(NodeData.ShellFolder) then
        begin
          ParentFolder.BindToObject(NodeData.PIDL, nil, IID_IShellFolder, Pointer(NodeData.ShellFolder));
        end;
      end
    end;
  end;

  if NodeData.shAttr = 0 then
  begin
    // If we cannot resolve attrs, we do not want to assume that the folder has no subfolders,
    // as that will make us scan the folder.
    NodeData.shAttr := SFGAO_HASSUBFOLDER;
  end;

end; {GetNodeAttr}

function CompareDrive(List: TStringList; Index1, Index2: Integer): Integer;
var
  Drive1, Drive2: string;
  RealDrive1, RealDrive2: Boolean;
begin
  Drive1 := List[Index1];
  Drive2 := List[Index2];
  RealDrive1 := DriveInfo.IsRealDrive(Drive1);
  RealDrive2 := DriveInfo.IsRealDrive(Drive2);
  if RealDrive1 = RealDrive2 then
  begin
    Result := CompareText(Drive1, Drive2);
  end
    else
  if RealDrive1 and (not RealDrive2) then
  begin
    Result := -1;
  end
    else
  begin
    Result := 1;
  end;
end;

function TDriveView.GetDrives: TStrings;
var
  DriveStatusPair: TPair<string, TDriveStatus>;
  Drives: TStringList;
begin
  Drives := TStringList.Create;
  { We could iterate only .Keys here, but that crashes IDE for some reason }
  for DriveStatusPair in FDriveStatus do
  begin
    Drives.Add(DriveStatusPair.Key);
  end;
  Drives.CustomSort(CompareDrive);
  Result := Drives;
end;

procedure TDriveView.RefreshRootNodes(dsFlags: Integer);
var
  Drives: TStrings;
  NewText: string;
  SaveCursor: TCursor;
  WasValid: Boolean;
  NodeData: TNodeData;
  NewDrive: Char;
  DriveStatus: TDriveStatus;
  NextDriveNode: TTreeNode;
  Index: Integer;
  Drive: string;
  GetAttr: Boolean;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Drives := nil;
  try
    Drives := GetDrives;
    NextDriveNode := nil;
    for Index := Drives.Count - 1 downto 0 do
    begin
      Drive := Drives[Index];
      DriveStatus := GetDriveStatus(Drive);
      if ((dsFlags and dvdsFloppy) <> 0) or DriveInfo.IsFixedDrive(Drive) then
      begin
        with DriveInfo.Get(Drive) do
        begin
          WasValid := Assigned(DriveStatus.RootNode);
        end;
        if ((dsFlags and dvdsReReadAllways) = 0) and
           (Length(DriveInfo.Get(Drive).DisplayName) > 0) then
              dsFlags := dsFlags and (not dsDisplayName);

        DriveInfo.ReadDriveStatus(Drive, dsFlags);

        with DriveInfo.Get(Drive), DriveStatus do
        begin
          if Valid then
          begin
            if not WasValid then
            {New drive has arrived: insert new rootnode:}
            begin
              { Create root directory node }
              NodeData := TNodeData.Create;
              NodeData.DirName := DriveInfo.GetDriveRoot(Drive);
              NodeData.ShortName := NodeData.DirName;

              {Get the shared attributes:}
              GetAttr :=
                DriveInfo.IsFixedDrive(Drive) and (DriveType <> DRIVE_REMOVABLE) and
                ((DriveType <> DRIVE_REMOTE) or GetNetWorkConnected(Drive));
              GetNodeShellAttr(nil, NodeData, GetAttr);

              if Assigned(NextDriveNode) then
                RootNode := Items.InsertObject(NextDriveNode, '', NodeData)
              else
                RootNode := Items.AddObject(nil, '', NodeData);

              RootNode.Text := GetDisplayName(RootNode);
              RootNode.HasChildren := True;

              Scanned := False;
              Verified := False;
            end
              else
            if RootNode.ImageIndex <> DriveInfo.Get(Drive).ImageIndex then
            begin {WasValid = True}
              RootNode.ImageIndex    := DriveInfo.Get(Drive).ImageIndex;
              RootNode.SelectedIndex := DriveInfo.Get(Drive).ImageIndex;
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
            if (Directory <> '') and (Directory[1] = Drive) then
            begin
              if DriveInfo.IsRealDrive(Drive) then NewDrive := Drive[1]
                else NewDrive := FirstFixedDrive;

              repeat
                if NewDrive < FirstFixedDrive then NewDrive := FirstFixedDrive
                  else
                if NewDrive = FirstFixedDrive then NewDrive := LastDrive
                  else Dec(NewDrive);
                DriveInfo.ReadDriveStatus(NewDrive, dsSize or dsImageIndex);

                if NewDrive = Drive then
                begin
                  Break;
                end;

                if DriveInfo.Get(NewDrive).Valid and DriveInfo.Get(NewDrive).DriveReady and Assigned(GetDriveStatus(NewDrive).RootNode) then
                begin
                  Directory := NodePathName(GetDriveStatus(NewDrive).RootNode);
                  break;
                end;
              until False;

              if not Assigned(Selected) then
              begin
                Directory := NodePathName(GetDriveStatus(FirstFixedDrive).RootNode);
              end;
            end;
            Scanned := False;
            Verified := False;
            RootNode.Delete;
            RootNode := nil;
          end;
        end;
      end;

      if Assigned(DriveStatus.RootNode) then
        NextDriveNode := DriveStatus.RootNode;

    end;
  finally
    Screen.Cursor := SaveCursor;
    Drives.Free;
  end;
end; {RefreshRootNodes}

function TDriveView.AddChildNode(ParentNode: TTreeNode; SRec: TSearchRec): TTreeNode;
var
  NewNode: TTreeNode;
  NodeData: TNodeData;
  GetAttr: Boolean;
begin
  NodeData := TNodeData.Create;
  NodeData.Attr := SRec.Attr;
  NodeData.DirName := SRec.Name;
  NodeData.ShortName := SRec.FindData.cAlternateFileName;
  NodeData.FIsRecycleBin :=
    (SRec.Attr and faSysFile <> 0) and
    (ParentNode.Level = 0) and
    (SameText(SRec.Name, 'RECYCLED') or
     SameText(SRec.Name, 'RECYCLER') or
     SameText(SRec.Name, '$RECYCLE.BIN'));

  { query content attributes ("has subfolder") only if tree view is visible }
  { to avoid unnecessary scan of subfolders (which may take some time) }
  { if tree view is not visible anyway }
  GetAttr :=
    Visible and
    (GetDriveTypeToNode(ParentNode) <> DRIVE_REMOTE);
  GetNodeShellAttr(ParentNode, NodeData, GetAttr);

  NewNode := Self.Items.AddChildObject(ParentNode, '', NodeData);
  NewNode.Text := GetDisplayName(NewNode);

  Result := NewNode;
end; {AddChildNode}

function TDriveView.GetDriveStatus(Drive: string): TDriveStatus;
begin
  if not FDriveStatus.TryGetValue(Drive, Result) then
  begin
    Result := CreateDriveStatus;
    FDriveStatus.Add(Drive, Result);
    RefreshRootNodes(dsAll or dvdsRereadAllways);
    if Assigned(OnRefreshDrives) then
      OnRefreshDrives(Self);
  end;
end; {GetDriveStatus}

function TDriveView.DoScanDir(FromNode: TTreeNode): Boolean;
begin
  Result := not TNodeData(FromNode.Data).IsRecycleBin;
end; {DoScanDir}

function TDriveView.DirAttrMask: Integer;
begin
  Result := faDirectory or faSysFile;
  if ShowHiddenDirs then
    Result := Result or faHidden;
end;

procedure TDriveView.ScanDrive(Drive: string);
var
  DosError: Integer;
  RootNode: TTreeNode;
  SaveCursor: TCursor;

  procedure ScanPath(const Path: string; ParentNode: TTreeNode);
  var
    SRec: TSearchRec;
    SubNode: TTreeNode;
  begin
    if not DoScanDir(ParentNode) then
      Exit;

    DosError := FindFirst(ApiPath(Path), DirAttrMask, Srec);
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
      ValidateDirectory(FindNodeToPath(DriveInfo.GetDriveRoot(Drive)));
      GetDriveStatus(Drive).Scanned := True;
      GetDriveStatus(Drive).Verified := False;
    end
      else
    begin
      SaveCursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
      Items.BeginUpdate;

      try
        RootNode := GetDriveStatus(Drive).RootNode;
        if not Assigned(RootNode) then Exit;

        iF RootNode.HasChildren then
          RootNode.DeleteChildren;

        ScanPath(DriveInfo.GetDriveRoot(Drive) + '*.*', RootNode);      { scan subdirectories of rootdir}
        TNodeData(RootNode.Data).Scanned := True;

        GetDriveStatus(Drive).Scanned := True;
        GetDriveStatus(Drive).Verified := True;
      finally
        SortChildren(GetDriveStatus(Drive).RootNode, True);
        EndUpdate;
      end;
      RootNode.Expand(False);

      Screen.Cursor := SaveCursor;
    end;
  end;
end; {ScanDrive}

function TDriveView.FindNodeToPath(Path: string): TTreeNode;

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
        begin
          Result := SearchSubDirs(Node, Path)
        end
          else
        begin
          Result := Node;
        end;
        Exit;
      end;
      Node := ParentNode.GetNextChild(Node);
    end;
  end;

  function SearchSubDirs(ParentNode: TTreeNode; Path: string): TTreeNode;
  begin
    Result := nil;
    if Length(Path) > 0 then
    begin
      if not TNodeData(ParentNode.Data).Scanned then
      begin
        ReadSubDirs(ParentNode, GetDriveTypetoNode(ParentNode));
      end;

      // Factored out of DoSearchSubDirs is remnant of Bug 956 superceded by Bug 1320
      Result := DoSearchSubDirs(ParentNode, Path);
    end;
  end; {SearchSubDirs}

var
  Drive: string;
  P: Integer;
begin {FindNodeToPath}
  Result := nil;
  if Length(Path) < 3 then
    Exit;

  // Particularly when used by TDirView to delegate browsing to
  // hidden drive view, the handle may not be created
  HandleNeeded;

  Drive := DriveInfo.GetDriveKey(Path);
  if Assigned(GetDriveStatus(Drive).RootNode) then
  begin
    if DriveInfo.IsRealDrive(Drive) then
    begin
      System.Delete(Path, 1, 3);
    end
      else
    if IsUncPath(Path) then
    begin
      System.Delete(Path, 1, 2);
      P := Pos('\', Path);
      if P = 0 then
      begin
        Path := '';
      end
        else
      begin
        System.Delete(Path, 1, P);
        P := Pos('\', Path);
        if P = 0 then
        begin
          Path := '';
        end
          else
        begin
          System.Delete(Path, 1, P);
        end;
      end;
    end
      else
    begin
      raise EConvertError.Create(Format(ErrorInvalidDrive, [Path]))
    end;
    if Length(Path) > 0 then
    begin
      if not GetDriveStatus(Drive).Scanned then
      begin
        ScanDrive(Drive);
      end;
      Result := SearchSubDirs(GetDriveStatus(Drive).RootNode, UpperCase(Path));
    end
      else Result := GetDriveStatus(Drive).RootNode;
  end;
end; {FindNodetoPath}

function TDriveView.CheckForSubDirs(Path: string): Boolean;
var
  DosError: Integer;
  SRec: TSearchRec;
begin
  Result := False;

  DosError := FindFirst(ApiPath(IncludeTrailingBackslash(Path) + '*.'), DirAttrMask, SRec);
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
  DosError := FindFirst(ApiPath(IncludeTrailingBackslash(NodePath(Node)) + '*.*'), DirAttrMask, SRec);
  while DosError = 0 do
  begin
    if (SRec.Name <> '.' ) and
       (SRec.Name <> '..') and
       (SRec.Attr and faDirectory <> 0) then
    begin
      NewNode := AddChildNode(Node, SRec);
      if DoScanDir(NewNode) then
      begin
        // We have seen the SFGAO_HASSUBFOLDER to be absent on C: drive $Recycle.Bin
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

function TDriveView.CallBackValidateDir(var Node: TTreeNode; Data: Pointer): Boolean;
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
  NodeData: TNodeData;
  ScanDirInfo: PScanDirInfo;
begin {CallBackValidateDir}
  Result := True;
  if (not Assigned(Node)) or (not Assigned(Node.Data)) then
    Exit;

  NewDirFound := False;
  ScanDirInfo := PScanDirInfo(Data);

  {Check, if directory still exists: (but not with root directory) }
  if Assigned(Node.Parent) and (ScanDirInfo^.StartNode = Node) then
    if not DirectoryExists(NodePathName(Node)) then
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

  NodeData := TNodeData(Node.Data);
  if NodeData.Scanned and Assigned(WorkNode) then
  {if node was already scanned: check wether the existing subnodes are still alive
   and add all new subdirectories as subnodes:}
  begin
    if DoScanDir(Node) then
    begin
      ParentDir := IncludeTrailingBackslash(NodePath(Node));
      {Build list of existing subnodes:}
      SubDirList := TStringList.Create;
      SubDirList.CaseSensitive := True; // We want to reflect changes in subfolder name case
      while Assigned(WorkNode) do
      begin
        SubDirList.Add(TNodeData(WorkNode.Data).DirName);
        WorkNode := Node.GetNextChild(WorkNode);
      end;
      {Sorting not required, because the subnodes are already sorted!}

      SRecList := TStringList.Create;
      SRecList.CaseSensitive := True;
      DosError := FindFirst(ApiPath(ParentDir + '*.*'), DirAttrMask, SRec);
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
  if (ScanDirInfo^.SearchNewDirs or
     NodeData.Scanned or
     (Node = ScanDirInfo^.StartNode)) and
     DoScanDir(Node) then
  begin
    ReadSubDirs(Node, ScanDirInfo^.DriveType);
  end;
end; {CallBackValidateDir}

procedure TDriveView.RebuildTree;
var
  Drive: string;
begin
  for Drive in FDriveStatus.Keys do
    with GetDriveStatus(Drive) do
      if Assigned(RootNode) and Scanned then
        ValidateDirectory(RootNode);
end;

procedure TDriveView.ValidateCurrentDirectoryIfNotMonitoring;
begin
  if Assigned(Selected) and
     not Assigned(GetDriveStatus(GetDriveToNode(Selected)).DiscMonitor) then
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
  RestartWatchThread: Boolean;
  SaveCanChange: Boolean;
  CurrentPath: string;
  Drive: string;
begin
  if Assigned(Node) and Assigned(Node.Data) and
     (not FValidateFlag) and DoScanDir(Node) then
  begin
    SelDir := Directory;
    SaveCursor := Screen.Cursor;
    if Self.Focused and (Screen.Cursor <> crHourGlass) then
      Screen.Cursor := crHourGlass;

    CurrentPath := NodePath(Node);

    Drive := DriveInfo.GetDriveKey(CurrentPath);
    if Node.Level = 0 then
      GetDriveStatus(Drive).ChangeTimer.Enabled := False;

    RestartWatchThread := WatchThreadActive;
    try
      if WatchThreadActive then
        StopWatchThread;

      FValidateFlag := True;

      New(Info);
      Info^.StartNode := Node;
      Info^.SearchNewDirs := NewDirs;
      Info^.DriveType := DriveInfo.Get(Drive).DriveType;

      SaveCanChange := FCanChange;
      FCanChange := True;
      FChangeFlag := False;
      IterateSubTree(Node, CallBackValidateDir, Recurse, coScanStartNode, Info);
      FValidateFlag := False;
      if (not Assigned(Selected)) and (Length(SelDir) > 0) then
        Directory := ExtractFileDrive(SelDir);
      if (SelDir <> Directory) and (not FChangeFlag) then
        Change(Selected);
      FCanChange := SaveCanChange;

      Dispose(Info);
    finally
      if RestartWatchThread and FWatchDirectory and not WatchThreadActive then
        StartWatchThread;

      if Screen.Cursor <> SaveCursor then
        Screen.Cursor := SaveCursor;
    end;
  end;
end; {ValidateDirectoryEx}

function TDriveView.GetDriveTypeToNode(Node: TTreeNode): Integer;
begin
  Assert(Assigned(Node));

  Result := DriveInfo.Get(GetDriveToNode(Node)).DriveType;
end; {GetDriveTypeToNode}

procedure TDriveView.CreateWatchThread(Drive: string);
begin
  if csDesigning in ComponentState then
    Exit;

  if (not Assigned(GetDriveStatus(Drive).DiscMonitor)) and
     FWatchDirectory and
     (DriveInfo.Get(Drive).DriveType <> DRIVE_REMOTE) then
  begin
    with GetDriveStatus(Drive) do
    begin
      DiscMonitor := TDiscMonitor.Create(Self);
      DiscMonitor.ChangeDelay := msThreadChangeDelay;
      DiscMonitor.SubTree := True;
      DiscMonitor.Filters := [moDirName];
      DiscMonitor.OnChange := ChangeDetected;
      DiscMonitor.OnInvalid := ChangeInvalid;
      DiscMonitor.SetDirectory(DriveInfo.GetDriveRoot(Drive));
      DiscMonitor.Open;
    end;
  end;
end; {CreateWatchThread}

procedure TDriveView.SetWatchDirectory(Value: Boolean);
begin
  if FWatchDirectory <> Value then
  begin
    FWatchDirectory := Value;
    if (not (csDesigning in ComponentState)) and Value then
      StartAllWatchThreads
    else
      StopAllWatchThreads;
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

function TDriveView.NodeWatched(Node: TTreeNode): Boolean;
var
  Drive: string;
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
  with GetDriveStatus(DriveInfo.GetDriveKey(Dir)) do
  begin
    DiscMonitor.Close;
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
      with GetDriveStatus(DriveInfo.GetDriveKey(DirChanged)) do
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
  DriveStatus: TDriveStatus;
begin
  if (FChangeTimerSuspended = 0) and (Sender is TTimer) then
  begin
    for DriveStatus in FDriveStatus.Values do
    begin
      if DriveStatus.ChangeTimer = Sender then
      begin
        with DriveStatus.ChangeTimer do
        begin
          Interval := 0;
          Enabled := False;
        end;

        if Assigned(DriveStatus.RootNode) then
        begin
          {Check also collapsed (invisible) subdirectories:}
          ValidateDirectory(DriveStatus.RootNode);
        end;
      end;
    end;
  end;
end; {ChangeTimerOnTimer}

procedure TDriveView.StartWatchThread;
var
  Drive: string;
begin
  if (csDesigning in ComponentState) or
     not Assigned(Selected) or
     not fWatchDirectory then Exit;

  Drive := GetDriveToNode(Selected);

  with GetDriveStatus(Drive) do
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
    with GetDriveStatus(GetDriveToNode(Selected)) do
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

procedure TDriveView.TerminateWatchThread(Drive: string);
begin
  with GetDriveStatus(Drive) do
    if Assigned(DiscMonitor) then
    begin
      DiscMonitor.Free;
      DiscMonitor := nil;
    end;
end; {StopWatchThread}

procedure TDriveView.StartAllWatchThreads;
var
  DriveStatusPair: TPair<string, TDriveStatus>;
  Drive: string;
begin
  if (csDesigning in ComponentState) or (not FWatchDirectory) then
     Exit;

  for DriveStatusPair in FDriveStatus do
    with DriveStatusPair.Value do
      if Scanned then
      begin
        if not Assigned(DiscMonitor) then
          CreateWatchThread(DriveStatusPair.Key);
        if Assigned(DiscMonitor) and (not DiscMonitor.Active) then
          DiscMonitor.Open;
      end;

  if Assigned(Selected) then
  begin
    Drive := GetDriveToNode(Selected);
    if not DriveInfo.IsFixedDrive(Drive) then
    begin
      StartWatchThread;
    end;
  end;
end; {StartAllWatchThreads}

procedure TDriveView.StopAllWatchThreads;
var
  DriveStatusPair: TPair<string, TDriveStatus>;
begin
  if (csDesigning in ComponentState) or (not FWatchDirectory) then
     Exit;

  for DriveStatusPair in FDriveStatus do
    with DriveStatusPair.Value do
    begin
      if Assigned(DiscMonitor) then
        DiscMonitor.Close;
    end;
end; {StopAllWatchThreads}

function TDriveView.WatchThreadActive(Drive: string): Boolean;
begin
  Result := FWatchDirectory and
    Assigned(GetDriveStatus(Drive).DiscMonitor) and
    GetDriveStatus(Drive).DiscMonitor.Active and
    GetDriveStatus(Drive).DiscMonitor.Enabled;
end; {WatchThreadActive}

function TDriveView.WatchThreadActive: Boolean;
var
  Drive: string;
begin
  if not Assigned(Selected) then
  begin
    Result := False;
    Exit;
  end;

  Drive := GetDriveToNode(Selected);

  Result := WatchThreadActive(Drive);
end; {WatchThreadActive}

procedure TDriveView.SetFullDriveScan(DoFullDriveScan: Boolean);
begin
  FFullDriveScan := DoFullDriveScan;
end; {SetAutoScan}

function TDriveView.FindPathNode(Path: string): TTreeNode;
var
  PossiblyHiddenPath: string;
  Attrs: Integer;
begin
  if Assigned(FOnNeedHiddenDirectories) and
     (not ShowHiddenDirs) and
     DirectoryExistsFix(Path) then // do not even bother if the path does not exist
  begin
    PossiblyHiddenPath := ExcludeTrailingPathDelimiter(Path);
    while (PossiblyHiddenPath <> '') and
          (not IsRootPath(PossiblyHiddenPath)) do // Drives have hidden attribute
    begin
      Attrs := FileGetAttr(PossiblyHiddenPath, False);
      if (Attrs and faHidden) = faHidden then
      begin
        if Assigned(FOnNeedHiddenDirectories) then
        begin
          FOnNeedHiddenDirectories(Self);
        end;
        Break;
      end
        else
      begin
        PossiblyHiddenPath := ExtractFileDir(PossiblyHiddenPath);
      end;
    end;
  end;

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
    if not GetDriveStatus(GetDriveToNode(Selected)).Scanned then
      ScanDrive(GetDriveToNode(Selected));
  end;
end; {SetDirectory}

function TDriveView.GetDirName(Node: TTreeNode): string;
begin
  if Assigned(Node) and Assigned(Node.Data) then
    Result := TNodeData(Node.Data).DirName
  else
    Result := '';
end; {GetDirName}

{GetDrive: returns the drive of the Node.}
function TDriveView.GetDriveToNode(Node: TTreeNode): string;
var
  Path: string;
begin
  if (not Assigned (Node)) or (not Assigned(Node.Data)) then
    raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['GetDrive']));
  Path := NodePath(Node);
  Result := DriveInfo.GetDriveKey(Path);
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

function TDriveView.GetDisplayName(Node: TTreeNode): string;
begin
  Result := '';
  if (not Assigned(Node)) or (not Assigned(Node.Data)) then
    raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['GetDisplayName']));

  if Node.Level = 0 then Result := GetDriveText(GetDriveToNode(Node))
    else
  begin
    Result := GetDirName(Node);
  end;
end; {GetDisplayName}

procedure TDriveView.SetShowVolLabel(ShowIt: Boolean);
begin
  if ShowIt = FShowVolLabel then
    Exit;
  FShowVolLabel := ShowIt;
  RefreshRootNodes(dvdsFloppy);
end; {SetShowVolLabel}

procedure TDriveView.DisplayContextMenu(Node: TTreeNode; Point: TPoint);
var
  Verb: string;
  DirWatched: Boolean;
begin
  DirWatched := NodeWatched(Node) and WatchThreadActive;

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

procedure TDriveView.DDChooseEffect(KeyState: Integer; var Effect: Integer; PreferredEffect: Integer);
begin
  if DropTarget = nil then Effect := DROPEFFECT_NONE
    else
  if (KeyState and (MK_CONTROL or MK_SHIFT) = 0) and (PreferredEffect = 0) then
  begin
    if FDragDrive <> '' then
    begin
      if FExeDrag and DriveInfo.IsFixedDrive(GetDriveToNode(DropTarget)) and DriveInfo.IsFixedDrive(FDragDrive) then
      begin
        Effect := DROPEFFECT_LINK;
      end
        else
      if (Effect = DROPEFFECT_COPY) and
         (SameText(FDragDrive, GetDriveToNode(DropTarget)) and
           (FDragDropFilesEx.AvailableDropEffects and DROPEFFECT_MOVE <> 0)) then
      begin
        Effect := DROPEFFECT_MOVE;
      end;
    end;
  end;

  inherited;
end;

function TDriveView.DragCompleteFileList: Boolean;
begin
  Result := (GetDriveTypeToNode(FDragNode) <> DRIVE_REMOVABLE);
end;

function TDriveView.DDExecute: TDragResult;
var
  WatchThreadOK: Boolean;
  DragParentPath: string;
  DragPath: string;
begin
  WatchThreadOK := WatchThreadActive;

  Result := FDragDropFilesEx.Execute(nil);

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
end;

procedure TDriveView.PerformDragDropFileOperation(Node: TTreeNode; Effect: Integer);
var
  Index: Integer;
  SourcePath: string;
  SourceParentPath: string;
  SourceIsDirectory: Boolean;
  SaveCursor: TCursor;
  TargetNode: TTreeNode;
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
    if (Effect = DROPEFFECT_COPY) or (Effect = DROPEFFECT_MOVE) then
    begin
      StopAllWatchThreads;
      if Assigned(FDirView) then
        FDirView.StopWatchThread;

      if Assigned(DropSourceControl) and
         (DropSourceControl is TDirView) and
         (DropSourceControl <> FDirView) then
      begin
        TDirView(DropSourceControl).StopWatchThread;
      end;

      if DropFiles(
           DragDropFilesEx, Effect, FFileOperator, TargetPath, false, IsRecycleBin, ConfirmDelete, ConfirmOverwrite, False,
           Self, OnDDFileOperation, SourcePath, SourceIsDirectory) then
      begin
        if Assigned(FOnDDFileOperationExecuted) then
          FOnDDFileOperationExecuted(Self, Effect, SourcePath, TargetPath);
      end;

      ClearDragFileList(FDragDropFilesEx.FileList);
      SourceParentPath := ExtractFilePath(ExcludeTrailingBackslash(SourcePath));
    end
      else
    if Effect = DROPEFFECT_LINK then
    { Create Link requested: }
    begin
      for Index := 0 to FDragDropFilesEx.FileList.Count - 1 do
      begin
        if not DropLink(PFDDListItem(FDragDropFilesEx.FileList[Index]), TargetPath) then
        begin
          DDError(DDCreateShortCutError);
        end;
      end;
    end;

    if Effect = DROPEFFECT_MOVE then
      Items.BeginUpdate;

    {Update source directory, if move-operation was performed:}
    if ((Effect = DROPEFFECT_MOVE) or IsRecycleBin) then
    begin
      ValidateDirectory(FindNodeToPath(SourceParentPath));
    end;

    {Update subdirectories of target directory:}
    TargetNode := FindNodeToPath(TargetPath);
    if Assigned(TargetNode) then
      ValidateDirectory(TargetNode)
    else
      ValidateDirectory(GetDriveStatus(DriveInfo.GetDriveKey(TargetPath)).RootNode);

    if Effect = DROPEFFECT_MOVE then
      Items.EndUpdate;

    {Update linked component TDirView:}
    if Assigned(FDirView) and (not FDirView.WatchThreadActive) then
    begin
      case Effect of
        DROPEFFECT_COPY,
        DROPEFFECT_LINK:
          if (IncludeTrailingBackslash(TargetPath) = IncludeTrailingBackslash(DirView.Path)) then
            FDirView.Reload2;

        DROPEFFECT_MOVE:
          if (IncludeTrailingBackslash(TargetPath) = IncludeTrailingBackslash(DirView.Path)) or
             (IncludeTrailingBackslash(SourceParentPath) = IncludeTrailingBackslash(DirView.Path)) then
          begin
            if FDirView <> DropSourceControl then FDirView.Reload2;
          end;
      end; {Case}
    end;

    {Update the DropSource control, if files are moved and it is a TDirView:}
    if (Effect = DROPEFFECT_MOVE) and (DropSourceControl is TDirView) then
    begin
      TDirView(DropSourceControl).ValidateSelectedFiles;
    end;

  finally
    FFileOperator.OperandFrom.Clear;
    FFileOperator.OperandTo.Clear;
    StartAllWatchThreads;

    if Assigned(FDirView) and (not FDirView.WatchThreadActive) then
      FDirView.StartWatchThread;
    if Assigned(DropSourceControl) and (DropSourceControl is TDirView) and
       (not TDirView(DropSourceControl).WatchThreadActive) then
      TDirView(DropSourceControl).StartWatchThread;
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
    StopAllWatchThreads;
    Result := FFileOperator.UndoExecute;

    ValidateDirectory(FindNodeToPath(ExtractFilePath(LastTarget)));
    ValidateDirectory(FindNodeToPath(ExtractFilePath(LastSource)));
    StartAllWatchThreads;

    if Assigned(FDirView) then
      with FDirView do
        if not WatchThreadActive then
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
  if CanPasteFromClipBoard and {MP}FDragDropFilesEx.GetFromClipBoard{/MP}
    then
  begin
    if TargetPath = '' then
      TargetPath := NodePathName(Selected);
    case LastClipBoardOperation of
      cboCopy,
      cboNone:
        begin
          PerformDragDropFileOperation(Selected, DROPEFFECT_COPY);
          if Assigned(FOnDDExecuted) then
            FOnDDExecuted(Self, DROPEFFECT_COPY);
        end;
      cboCut:
        begin
          PerformDragDropFileOperation(Selected, DROPEFFECT_MOVE);
          if Assigned(FOnDDExecuted) then
            FOnDDExecuted(Self, DROPEFFECT_MOVE);
          EmptyClipBoard;
        end;
    end;
    Result := True;
  end;
end; {PasteFromClipBoard}

end.
