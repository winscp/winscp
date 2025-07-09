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
  DiscMon, IEDriveInfo, IEListView, BaseUtils, CustomDirView,
  CustomDriveView, System.Generics.Collections, CompThread;

const
  msThreadChangeDelay = 50;

  ErrorNodeNA = '%s: Node not assigned';

  {Flags used by TDriveView.RefreshRootNodes:}
  dvdsFloppy          = 8;  {Include floppy drives}
  dvdsRereadAllways   = 16; {Refresh drivestatus in any case}

  WM_USER_SHCHANGENOTIFY = WM_USER + $2000 + 13;
  WM_USER_SUBDIRREADER = WM_USER_SHCHANGENOTIFY + 1;

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
    DriveHandle: THandle;
    NotificationHandle: HDEVNOTIFY;
  end;

  TDriveStatusPair = TPair<string, TDriveStatus>;

  TScanDirInfo = record
    SearchNewDirs: Boolean;
    StartNode: TTreeNode;
    DriveType: Integer;
  end;

  PScanDirInfo = ^TScanDirInfo;

  TDriveView = class;

  TSubDirReaderSchedule = class
    Node: TTreeNode;
    Path: string;
    Deleted: Boolean;
    Processed: Boolean;
  end;

  TNodeData = class
  private
    FDirName: string;
    FAttr: Integer;
    FScanned: Boolean;
    FData: Pointer;
    FIsRecycleBin: Boolean;
    FIconEmpty: Boolean;
    FSchedule: TSubDirReaderSchedule;

  public
    DelayedSrec: TSearchRec;
    DelayedExclude: TStringList;

    constructor Create;
    destructor Destroy; override;

    property DirName: string read FDirName write FDirName;
    property Attr: Integer read FAttr write FAttr;
    property Scanned: Boolean read FScanned write FScanned;
    property Data: Pointer read FData write FData;
    property IsRecycleBin: Boolean read FIsRecycleBin;
    property IconEmpty: Boolean read FIconEmpty write FIconEmpty;
    property Schedule: TSubDirReaderSchedule read FSchedule write FSchedule;
  end;

  TDriveTreeNode = class(TTreeNode)
    procedure Assign(Source: TPersistent); override;
  end;

  TSubDirReaderThread = class(TCompThread)
  public
    destructor Destroy; override;
    procedure Terminate; override;

  protected
    constructor Create(DriveView: TDriveView);
    procedure Add(Node: TTreeNode; Path: string);
    procedure Delete(Node: TTreeNode);
    function Detach: Integer;
    procedure Reattach(Count: Integer);

    procedure Execute; override;

  private
    FDriveView: TDriveView;
    FEvent: THandle;
    FQueue: TStack<TSubDirReaderSchedule>;
    FResults: TQueue<TSubDirReaderSchedule>;
    FSection: TRTLCriticalSection;
    FTimer: TTimer;
    FWindowHandle: HWND;

    procedure TriggerEvent;
    procedure ScheduleProcess;
    procedure Process;
    function ProcessResult: Boolean;
    procedure Timer(Sender: TObject);
    procedure WndProc(var Msg: TMessage);
  end;

  TDriveViewRefreshDrives = procedure(Sender: TObject; Global: Boolean) of object;

  TDriveView = class(TCustomDriveView)
  private
    FDriveStatus: TObjectDictionary<string, TDriveStatus>;

    FConfirmDelete: Boolean;
    FConfirmOverwrite: Boolean;
    FWatchDirectory: Boolean;
    FDirectory: string;
    FShowVolLabel: Boolean;
    FVolDisplayStyle: TVolumeDisplayStyle;
    FChangeFlag: Boolean;
    FLastDir: string;
    FValidateFlag: Boolean;
    FSysColorChangePending: Boolean;
    FCreating: Boolean;
    FForceRename: Boolean;
    FRenameNode: TTreeNode;
    FLastRenameName: string;
    FInternalWindowHandle: HWND;
    FChangeNotify: ULONG;
    FPrevSelected: TTreeNode;
    FPrevSelectedIndex: Integer;
    FChangeTimerSuspended: Integer;
    FSubDirReaderThread: TSubDirReaderThread;
    FDelayedNodes: TStringList;
    FDelayedNodeTimer: TTimer;
    FRecreateScheduledCount: Integer;

    {Additional events:}
    FOnDisplayContextMenu: TNotifyEvent;
    FOnRefreshDrives: TDriveViewRefreshDrives;
    FOnNeedHiddenDirectories: TNotifyEvent;

    {used components:}
    FDirView: TDirView;
    FFileOperator: TFileOperator;

    FChangeInterval: Cardinal;

    {Drag&drop:}
    FLastPathCut: string;

    {Drag&drop helper functions:}
    procedure SignalDirDelete(Sender: TObject; Files: TStringList);

    function GetSubDir(var SRec: TSearchRec): Boolean;
    function FindFirstSubDir(Path: string; var SRec: TSearchRec): Boolean;
    function FindNextSubDir(var SRec: TSearchRec): Boolean;
    procedure ReadSubDirs(Node: TTreeNode);
    procedure CancelDelayedNode(Node: TTreeNode);
    procedure DelayedNodeTimer(Sender: TObject);
    function ReadSubDirsBatch(Node: TTreeNode; var SRec: TSearchRec; CheckInterval, Limit: Integer): Boolean;
    procedure UpdateDelayedNodeTimer;

    {Callback-functions used by iteratesubtree:}
    function CallBackValidateDir(var Node: TTreeNode; Data: Pointer): Boolean;
    procedure DeleteNode(Node: TTreeNode);

    { Notification procedures used by component TDiscMonitor: }
    procedure ChangeDetected(Sender: TObject; const Directory: string;
      var SubdirsChanged: Boolean);
    procedure ChangeInvalid(Sender: TObject; const Directory: string; const ErrorStr: string);

    {Notification procedure used by component TTimer:}
    procedure ChangeTimerOnTimer(Sender: TObject);

  protected
    procedure SetSelected(Node: TTreeNode);
    procedure SetWatchDirectory(Value: Boolean);
    procedure SetShowVolLabel(ShowIt: Boolean);
    procedure SetDirView(Value: TDirView);
    procedure SetDirectory(Value: string); override;
    function  DoScanDir(FromNode: TTreeNode): Boolean;
    procedure AddChildNode(ParentNode: TTreeNode; ParentPath: string; SRec: TSearchRec);
    procedure CreateWatchThread(Drive: string);
    function NodeWatched(Node: TTreeNode): Boolean;
    procedure TerminateWatchThread(Drive: string);
    function WatchThreadActive: Boolean; overload;
    function WatchThreadActive(Drive: string): Boolean; overload;
    procedure InternalWndProc(var Msg: TMessage);
    procedure UpdateDriveNotifications(Drive: string);
    procedure DriveRemoved(Drive: string);
    procedure DriveRemoving(Drive: string);
    procedure CancelDriveRefresh;
    procedure DoRefreshDrives(Global: Boolean);
    procedure RefreshRootNodes(dsFlags: Integer);

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
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;

    function GetCustomDirView: TCustomDirView; override;
    procedure SetCustomDirView(Value: TCustomDirView); override;

    function NodePath(Node: TTreeNode): string; override;
    function NodeIsRecycleBin(Node: TTreeNode): Boolean; override;
    function NodePathExists(Node: TTreeNode): Boolean; override;
    function NodeColor(Node: TTreeNode): TColor; override;
    function FindPathNode(Path: string): TTreeNode; override;
    function DoFindNodeToPath(Path: string; ExistingOnly: Boolean): TTreeNode;
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
    function GetDrives: TStrings;
    procedure ScheduleDriveRefresh;

    {Node handling:}
    procedure SetImageIndex(Node: TTreeNode); virtual;
    function FindNodeToPath(Path: string): TTreeNode;
    function TryFindNodeToPath(Path: string): TTreeNode;
    function RootNode(Node: TTreeNode): TTreeNode;
    function GetDirName(Node: TTreeNode): string;
    function GetDisplayName(Node: TTreeNode): string;
    function NodePathName(Node: TTreeNode): string; override;

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
    {Enable automatic update on filesystem changes:}
    property WatchDirectory: Boolean read FWatchDirectory write SetWatchDirectory default False;
    {Linked component TDirView:}
    property DirView: TDirView read FDirView write SetDirView;
    {Show the volume labels of drives:}
    property ShowVolLabel: Boolean read FShowVolLabel write SetShowVolLabel default True;
    {Additional events:}
    property OnDisplayContextMenu: TNotifyEvent read FOnDisplayContextMenu
      write FOnDisplayContextMenu;
    property OnRefreshDrives: TDriveViewRefreshDrives read FOnRefreshDrives write FOnRefreshDrives;
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

var
  DriveViewLoadingTooLongLimit: Integer = 0;

procedure Register;

implementation

uses
  PasTools, UITypes, SyncObjs, IOUtils, System.DateUtils;

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
  FIsRecycleBin := False;
  FIconEmpty := True;
  FSchedule := nil;
  DelayedExclude := nil;
end; {TNodeData.Create}

destructor TNodeData.Destroy;
begin
  Assert(not Assigned(FSchedule));
  SetLength(FDirName, 0);
  inherited;
end; {TNodeData.Destroy}

  { TSubDirReaderThread }

constructor TSubDirReaderThread.Create(DriveView: TDriveView);
begin
  inherited Create(True);
  FDriveView := DriveView;
  FSection.Initialize;
  FEvent := CreateEvent(nil, False, False, nil);
  FQueue := TStack<TSubDirReaderSchedule>.Create;
  FResults := TQueue<TSubDirReaderSchedule>.Create;
  FTimer := TTimer.Create(FDriveView);
  FTimer.Enabled := False;
  FTimer.Interval := 200;
  FTimer.OnTimer := Timer;
  FWindowHandle := AllocateHWnd(WndProc);
end;

destructor TSubDirReaderThread.Destroy;

  procedure DestroyScheduleList(List: TEnumerable<TSubDirReaderSchedule>);
  var
    Schedule: TSubDirReaderSchedule;
  begin
    for Schedule in List do
    begin
      if not Schedule.Deleted then
        TNodeData(Schedule.Node.Data).Schedule := nil;
      Schedule.Free;
    end;
    List.Destroy;
  end;

begin
  inherited;

  DeallocateHWnd(FWindowHandle);

  DestroyScheduleList(FQueue);
  DestroyScheduleList(FResults);

  CloseHandle(FEvent);
  FTimer.Destroy;
  FSection.Destroy;
end;

procedure TSubDirReaderThread.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_USER_SUBDIRREADER then
    ScheduleProcess
  else
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

procedure TSubDirReaderThread.Process;
var
  Started: DWORD;
  Elapsed: Integer;
  Later: Boolean;
begin
  Started := GetTickCount;
  Later := False;
  while (not Later) and ProcessResult do
  begin
    Elapsed := GetTickCount - Started;
    Later := (Elapsed < 0) or (Elapsed > 20);
  end;

  if not Later then
    FTimer.Enabled := False;
end;

procedure TSubDirReaderThread.Timer(Sender: TObject);
begin
  Process;
end;

procedure TSubDirReaderThread.Add(Node: TTreeNode; Path: string);
var
  NodeData: TNodeData;
  Schedule: TSubDirReaderSchedule;
begin
  if Suspended then
    Resume;

  FSection.Enter;
  try
    NodeData := TNodeData(Node.Data);
    Assert(not Assigned(NodeData.Schedule));
    Schedule := TSubDirReaderSchedule.Create;
    Schedule.Node := Node;
    Schedule.Path := Path;
    Schedule.Deleted := False;
    Schedule.Processed := False;
    FQueue.Push(Schedule);
    NodeData.Schedule := Schedule;
  finally
    FSection.Leave;
  end;
  TriggerEvent;
end;

procedure TSubDirReaderThread.Delete(Node: TTreeNode);
var
  NodeData: TNodeData;
begin
  FSection.Enter;
  try
    NodeData := TNodeData(Node.Data);
    if Assigned(NodeData.Schedule) then
    begin
      NodeData.Schedule.Deleted := True;
      NodeData.Schedule := nil;
    end;
  finally
    FSection.Leave;
  end;
  TriggerEvent;
end;

function TSubDirReaderThread.Detach: Integer;

  procedure DetachList(List: TEnumerable<TSubDirReaderSchedule>);
  var
    Schedule: TSubDirReaderSchedule;
  begin
    for Schedule in List do
    begin
      if Schedule.Deleted then Schedule.Free
        else
      begin
        Assert(Schedule.Processed = (List = FResults));
        Schedule.Node := nil;
        Inc(Result);
      end;
    end;
  end;

begin
  // block thread while handle is being recreated
  FSection.Enter;
  try
    Result := 0;

    DetachList(FQueue);
    DetachList(FResults);

    FQueue.Clear;
    FResults.Clear;
  except
    FSection.Leave;
    raise;
  end;
end;

procedure TSubDirReaderThread.Reattach(Count: Integer);
var
  Node: TTreeNode;
  Schedule: TSubDirReaderSchedule;
begin
  try
    if Count > 0 then
    begin
      Node := FDriveView.Items.GetFirstNode;
      while Assigned(Node) do
      begin
        Schedule := TNodeData(Node.Data).Schedule;
        if Assigned(Schedule) then
        begin
          Assert(not Assigned(Schedule.Node));
          Schedule.Node := Node;
          if not Schedule.Processed then
            FQueue.Push(Schedule)
          else
            FResults.Enqueue(Schedule);
          Assert(Count > 0);
          // Can be optimized to stop once Count = 0
          Dec(Count);
        end;
        Node := Node.GetNext;
      end;
      if Count <> 0 then Assert(False); // shut up
    end;
  finally
    FSection.Leave;
  end;

  TriggerEvent;
  ScheduleProcess;
end;

procedure TSubDirReaderThread.Terminate;
begin
  inherited;
  TriggerEvent;
end;

procedure TSubDirReaderThread.TriggerEvent;
begin
  SetEvent(FEvent);
end;

function TSubDirReaderThread.ProcessResult: Boolean;
var
  Node: TTreeNode;
  NodeData: TNodeData;
  Schedule: TSubDirReaderSchedule;
begin
  FSection.Enter;
  try
    Result := (FResults.Count > 0);
    if Result then
    begin
      Schedule := FResults.Dequeue;
      if not Schedule.Deleted then
      begin
        Assert(Schedule.Processed);
        Node := Schedule.Node;
        Node.HasChildren := False;
        NodeData := TNodeData(Node.Data);
        NodeData.Scanned := not Node.HasChildren; // = True
        Assert(NodeData.Schedule = Schedule);
        NodeData.Schedule := nil;
      end;
      Schedule.Free;
    end;
  finally
    FSection.Leave;
  end;
end;

procedure TSubDirReaderThread.ScheduleProcess;
begin
  // process the first batch immediatelly, to make it more likely that the first seen subdirectories
  // will immediatelly show correct status
  Process;
  FTimer.Enabled := True;
end;

procedure TSubDirReaderThread.Execute;
var
  SRec: TSearchRec;
  HasSubDirs: Boolean;
  NodeData: TNodeData;
  Schedule: TSubDirReaderSchedule;
  DelayStart, DelayStartStep: Integer;
begin
  DelayStart := 3000;
  DelayStartStep := 100;
  while (DelayStart > 0) and (not Terminated) do
  begin
    Sleep(DelayStartStep);
    Dec(DelayStart, DelayStartStep)
  end;

  while not Terminated do
  begin
    WaitForSingleObject(FEvent, INFINITE);

    while not Terminated do
    begin
      FSection.Enter;
      try
        if FQueue.Count = 0 then
        begin
          Break;
        end
          else
        begin
          Schedule := FQueue.Pop;
          if Schedule.Deleted then
          begin
            Schedule.Free;
            // Can be optimized to loop within locked critical section until first non-deleted schedule is found
            Continue;
          end;
          Assert(not Schedule.Processed);
        end
      finally
        FSection.Leave;
      end;

      HasSubDirs := FDriveView.FindFirstSubDir(IncludeTrailingBackslash(Schedule.Path) + '*.*', SRec);
      FindClose(SRec);

      FSection.Enter;
      try
        if Schedule.Deleted then
        begin
          Schedule.Free;
        end
          else
        begin
          Schedule.Processed := True;
          if not HasSubDirs then // optimization
          begin
            FResults.Enqueue(Schedule);
            if FResults.Count = 1 then
              PostMessage(FWindowHandle, WM_USER_SUBDIRREADER, 0, 0);
          end
            else
          begin
            // can happen only if the tree handle is just being recreated
            if Assigned(Schedule.Node) then
            begin
              NodeData := TNodeData(Schedule.Node.Data);
              NodeData.Schedule := nil;
            end;
            Schedule.Free;
          end;
        end;
      finally
        FSection.Leave;
      end;
    end;
  end;
end;

  { TDriveTreeNode }

// Not sure if this is ever used (possibly only then "assigning" tree view to another instance, what never do).
// It is NOT used when recreating a tree view handle - for that a node is serialized and deserialized,
// including a pointer to TNodeData. See csRecreating condition in TDriveView.Delete.
procedure TDriveTreeNode.Assign(Source: TPersistent);
var
  SourceData: TNodeData;
  NewData: TNodeData;
begin
  Assert(False);
  inherited Assign(Source);

  if not Deleting and (Source is TTreeNode) then
  begin
    SourceData := TNodeData(TTreeNode(Source).Data);
    NewData := TNodeData.Create();
    NewData.DirName := SourceData.DirName;
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
  ChangeNotifyEntry: TSHChangeNotifyEntry;
  Dummy: string;
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
  FSubDirReaderThread := TSubDirReaderThread.Create(Self);
  FDelayedNodes := TStringList.Create;
  FDelayedNodeTimer := TTimer.Create(Self);
  UpdateDelayedNodeTimer;
  FDelayedNodeTimer.Interval := 250;
  FDelayedNodeTimer.OnTimer := DelayedNodeTimer;

  FShowVolLabel := True;
  FChangeFlag := False;
  FLastDir := EmptyStr;
  FValidateFlag := False;
  FSysColorChangePending := False;
  FConfirmDelete := True;
  FDirectory := EmptyStr;
  FForceRename := False;
  FLastRenameName := '';
  FRenameNode := nil;
  FPrevSelected := nil;
  FPrevSelectedIndex := -1;
  FChangeTimerSuspended := 0;
  FRecreateScheduledCount := -1;

  FConfirmOverwrite := True;
  FLastPathCut := '';
  FStartPos.X := -1;
  FStartPos.Y := -1;
  FDragPos := FStartPos;

  FInternalWindowHandle := Classes.AllocateHWnd(InternalWndProc);

  // Source: petr.solin 2022-02-25
  FChangeNotify := 0;
  if SpecialFolderLocation(CSIDL_DESKTOP, Dummy, ChangeNotifyEntry.pidl) then
  begin
    ChangeNotifyEntry.fRecursive := False;

    FChangeNotify :=
      SHChangeNotifyRegister(
        FInternalWindowHandle, SHCNRF_ShellLevel or SHCNRF_NewDelivery,
        SHCNE_RENAMEFOLDER or SHCNE_MEDIAINSERTED or SHCNE_MEDIAREMOVED,
        WM_USER_SHCHANGENOTIFY, 1, ChangeNotifyEntry);
  end;

  with FDragDropFilesEx do
  begin
    ShellExtensions.DragDropHandler := True;
  end;
end; {Create}

destructor TDriveView.Destroy;
var
  DriveStatusPair: TDriveStatusPair;
begin
  if FChangeNotify <> 0 then SHChangeNotifyDeregister(FChangeNotify);
  Classes.DeallocateHWnd(FInternalWindowHandle);

  for DriveStatusPair in FDriveStatus do
  begin
    with DriveStatusPair.Value do
    begin
      if Assigned(DiscMonitor) then
        FreeAndNil(DiscMonitor);
      if Assigned(ChangeTimer) then
        FreeAndNil(ChangeTimer);
    end;
    UpdateDriveNotifications(DriveStatusPair.Key);
  end;
  FDriveStatus.Free;

  if Assigned(FFileOperator) then
    FFileOperator.Free;
  FSubDirReaderThread.Free;

  Assert(FDelayedNodes.Count = 0);
  FreeAndNil(FDelayedNodes);

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
    DriveHandle := INVALID_HANDLE_VALUE;
    NotificationHandle := nil;
  end;
end;

procedure TDriveView.DriveRemoving(Drive: string);
begin
  DriveRemoved(Drive);
  TerminateWatchThread(Drive);
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

  PDEV_BROADCAST_HANDLE = ^DEV_BROADCAST_HANDLE;
  DEV_BROADCAST_HANDLE = record
    dbch_size       : DWORD;
    dbch_devicetype : DWORD;
    dbch_reserved   : DWORD;
    dbch_handle     : THandle;
    dbch_hdevnotify : HDEVNOTIFY  ;
    dbch_eventguid  : TGUID;
    dbch_nameoffset : LongInt;
    dbch_data       : Byte;
  end;

  PPItemIDList = ^PItemIDList;

const
  DBT_DEVTYP_HANDLE = $00000006;
  DBT_CONFIGCHANGED = $0018;
  DBT_DEVICEARRIVAL = $8000;
  DBT_DEVICEQUERYREMOVE = $8001;
  DBT_DEVICEREMOVEPENDING = $8003;
  DBT_DEVICEREMOVECOMPLETE = $8004;
  DBT_DEVTYP_VOLUME = $00000002;

// WORKAROUND Declaration in Winapi.ShlObj.pas is wrong
function SHChangeNotification_Lock(hChange: THandle; dwProcId: DWORD;
  var PPidls: PPItemIDList; var plEvent: Longint): THANDLE; stdcall;
external 'shell32.dll' name 'SHChangeNotification_Lock';

procedure TDriveView.InternalWndProc(var Msg: TMessage);
var
  DeviceType: DWORD;
  UnitMask: DWORD;
  DeviceHandle: THandle;
  Drive: Char;
  DriveStatusPair: TDriveStatusPair;
  PPIDL: PPItemIDList;
  Event: LONG;
  Lock: THandle;
begin
  with Msg do
  begin
    if Msg = WM_USER_SHCHANGENOTIFY then
    begin
      Lock := SHChangeNotification_Lock(wParam, lParam, PPIDL, Event);
      try
        if (Event = SHCNE_RENAMEFOLDER) or // = drive rename
           (Event = SHCNE_MEDIAINSERTED) or // also bitlocker drive unlock (also sends SHCNE_UPDATEDIR)
           (Event = SHCNE_MEDIAREMOVED) then
        begin
          ScheduleDriveRefresh;
        end;
      finally
        SHChangeNotification_Unlock(Lock);
      end;
    end
      else
    if Msg = WM_DEVICECHANGE then
    begin
      if (wParam = DBT_CONFIGCHANGED) or
         (wParam = DBT_DEVICEARRIVAL) or
         (wParam = DBT_DEVICEREMOVECOMPLETE) then
      begin
        ScheduleDriveRefresh;
      end
        else
      if (wParam = DBT_DEVICEQUERYREMOVE) or
         (wParam = DBT_DEVICEREMOVEPENDING) then
      begin
        DeviceType := PDevBroadcastHdr(lParam)^.dbch_devicetype;
        // This is specifically for VeraCrypt.
        // For normal drives, see DBT_DEVTYP_HANDLE below
        // (and maybe now that we have generic implementation, this specific code for VeraCrypt might not be needed anymore)
        if DeviceType = DBT_DEVTYP_VOLUME then
        begin
          UnitMask := PDevBroadcastVolume(lParam)^.dbcv_unitmask;
          Drive := FirstDrive;
          while UnitMask > 0 do
          begin
            if UnitMask and $01 <> 0 then
            begin
              DriveRemoving(Drive);
            end;
            UnitMask := UnitMask shr 1;
            Drive := Chr(Ord(Drive) + 1);
          end;
        end
          else
        if DeviceType = DBT_DEVTYP_HANDLE then
        begin
          DeviceHandle := PDEV_BROADCAST_HANDLE(lParam)^.dbch_handle;
          for DriveStatusPair in FDriveStatus do
            if DriveStatusPair.Value.DriveHandle = DeviceHandle then
            begin
              DriveRemoving(DriveStatusPair.Key);
            end;
        end;
      end;
    end
      else
    if Msg = WM_TIMER then
    begin
      CancelDriveRefresh;
      try
        //DriveInfo.Load;
        RefreshRootNodes(dsAll or dvdsRereadAllways);
        DoRefreshDrives(True);
      except
        Application.HandleException(Self);
      end;
    end;

    Result := DefWindowProc(FInternalWindowHandle, Msg, wParam, lParam);
  end;
end;

procedure TDriveView.DoRefreshDrives(Global: Boolean);
begin
  if Assigned(OnRefreshDrives) then
    OnRefreshDrives(Self, Global);
end;

procedure TDriveView.CancelDriveRefresh;
begin
  KillTimer(FInternalWindowHandle, 1);
end;

procedure TDriveView.ScheduleDriveRefresh;
begin
  CancelDriveRefresh;
  // Delay refreshing drives for a sec.
  // Particularly with CD/DVD drives, if we query display name
  // immediately after receiving DBT_DEVICEARRIVAL, we do not get media label.
  // Actually one sec does not help usually, but we do not want to wait any longer,
  // because we want to add USB drives asap.
  // And this problem might be solved now by SHChangeNotifyRegister/SHCNE_RENAMEFOLDER.
  SetTimer(FInternalWindowHandle, 1, MSecsPerSec, nil);
end;

procedure TDriveView.CreateWnd;
var
  DriveStatus: TDriveStatus;
begin
  inherited;

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

  UpdateDelayedNodeTimer;

  if FRecreateScheduledCount >= 0 then
  begin
    FSubDirReaderThread.Reattach(FRecreateScheduledCount);
    FRecreateScheduledCount := -1;
  end;
end; {CreateWnd}

procedure TDriveView.DestroyWnd;
var
  DriveStatus: TDriveStatus;
  I: Integer;
begin
  FDelayedNodeTimer.Enabled := False;
  for I := 0 to FDelayedNodes.Count - 1 do
    FDelayedNodes.Objects[I] := nil;
  if not (csRecreating in ControlState) then
  begin
    FSubDirReaderThread.Terminate;
    FSubDirReaderThread.WaitFor;
  end
    else
  if CreateWndRestores then
  begin

    Assert(FRecreateScheduledCount < 0);
    // Have to use field, instead of local variable in CM_RECREATEWND handler,
    // as CM_RECREATEWND is not invoked, when the recreation is trigerred recursivelly from parent
    // control/form.
    FRecreateScheduledCount := FSubDirReaderThread.Detach;

    if Items.Count > 0 then // redundant test?
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
  end;
  inherited;
end;

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
  Node := Node.Parent;

  while Assigned(Node) do
  begin
    ParentNode := Node.Parent;
    if Assigned(ParentNode) then
      Result := GetDirName(Node) + '\' + Result
    else
      Result := GetDirName(Node) + Result;

    Node := ParentNode;
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
  Result := DirectoryExists(ApiPath(NodePathName(Node)));
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
      Flags := FileOperatorDefaultFlags + [foNoConfirmation];
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
    if (not Assigned(Node.Parent)) and
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
          ReadSubDirs(Node);
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
  if (Length(FDirectory) > 0) and DirectoryExists(ApiPath(FDirectory)) then
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
    FSubDirReaderThread.Delete(Node);

    if Assigned(NodeData.DelayedExclude) then
    begin
      CancelDelayedNode(Node);
      FDelayedNodes.Delete(FDelayedNodes.IndexOfObject(Node));
      UpdateDelayedNodeTimer;
    end;

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
  PrevDrive: string;
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
          PrevDrive := DriveInfo.GetDriveKey(FLastDir)
        else
          PrevDrive := '';

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
            if not DirectoryExists(ApiPath(NewDir)) then
            begin
              ValidateDirectory(GetDriveStatus(Drive).RootNode);
              Exit;
            end;

            GetDriveStatus(Drive).DefaultDir := IncludeTrailingBackslash(NewDir);

            if PrevDrive <> Drive then
            begin
              if (PrevDrive <> '') and
                 (DriveInfo.Get(PrevDrive).DriveType = DRIVE_REMOVABLE) then
              begin
                TerminateWatchThread(PrevDrive);
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
    if not Assigned(Node.Parent) then
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
  DriveStatusPair: TDriveStatusPair;
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

procedure TDriveView.DriveRemoved(Drive: string);
var
  NewDrive: Char;
begin
  if (Directory <> '') and (Directory[1] = Drive) then
  begin
    if DriveInfo.IsRealDrive(Drive) then NewDrive := Drive[1]
      else NewDrive := SystemDrive;

    repeat
      if NewDrive < SystemDrive then NewDrive := SystemDrive
        else
      if NewDrive = SystemDrive then NewDrive := LastDrive
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
      Directory := NodePathName(GetDriveStatus(SystemDrive).RootNode);
    end;
  end;
end;

procedure TDriveView.RefreshRootNodes(dsFlags: Integer);
var
  Drives: TStrings;
  NewText: string;
  SaveCursor: TCursor;
  WasValid: Boolean;
  NodeData: TNodeData;
  DriveStatus: TDriveStatus;
  NextDriveNode: TTreeNode;
  Index: Integer;
  Drive: string;
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
            DriveRemoved(Drive);
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

procedure TDriveView.AddChildNode(ParentNode: TTreeNode; ParentPath: string; SRec: TSearchRec);
var
  NewNode: TTreeNode;
  NodeData: TNodeData;
begin
  NodeData := TNodeData.Create;
  NodeData.Attr := SRec.Attr;
  NodeData.DirName := SRec.Name;
  NodeData.FIsRecycleBin :=
    (SRec.Attr and faSysFile <> 0) and
    (not Assigned(ParentNode.Parent)) and
    (SameText(SRec.Name, 'RECYCLED') or
     SameText(SRec.Name, 'RECYCLER') or
     SameText(SRec.Name, '$RECYCLE.BIN'));
  NodeData.Scanned := False;

  NewNode := Self.Items.AddChildObject(ParentNode, '', NodeData);
  NewNode.Text := GetDisplayName(NewNode);
  NewNode.HasChildren := True;
  if GetDriveTypeToNode(ParentNode) <> DRIVE_REMOTE then
    FSubDirReaderThread.Add(NewNode, IncludeTrailingBackslash(ParentPath) + SRec.Name);

end; {AddChildNode}

function TDriveView.GetDriveStatus(Drive: string): TDriveStatus;
begin
  if not FDriveStatus.TryGetValue(Drive, Result) then
  begin
    Result := CreateDriveStatus;
    FDriveStatus.Add(Drive, Result);
    RefreshRootNodes(dsAll or dvdsRereadAllways);
    DoRefreshDrives(False);
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
begin {ScanDrive}
  with Self.Items do
  begin
    ValidateDirectory(FindNodeToPath(DriveInfo.GetDriveRoot(Drive)));
    GetDriveStatus(Drive).Scanned := True;
    GetDriveStatus(Drive).Verified := False;
  end;
end; {ScanDrive}

function TDriveView.DoFindNodeToPath(Path: string; ExistingOnly: Boolean): TTreeNode;
var
  SelectionHierarchy: array of TTreeNode;
  SelectionHierarchyHeight: Integer;

  function SearchSubDirs(ParentNode: TTreeNode; Path: string; Level: Integer): TTreeNode; forward;

  function ExtractFirstName(S: string): string;
  var
    I: Integer;
  begin
    I := Pos('\', S);

    if I = 0 then
      I := Length(S);

    Result := System.Copy(S, 1, I);
  end;

  function DoSearchSubDirs(ParentNode: TTreeNode; Path: string; Level: Integer): TTreeNode;
  var
    Node: TTreeNode;
    Dir: string;
  begin
    {Extract first directory from path:}
    Dir := ExtractFirstName(Path);
    System.Delete(Path, 1, Length(Dir));

    if Dir[Length(Dir)] = '\' then
      SetLength(Dir, Pred(Length(Dir)));

    // Optimization. Avoid iterating possibly thousands of nodes,
    // when the node we are looking for is the selected node or its ancestor.
    // This is often the case, when navigating under node that has lot of siblings.
    // Typically, when navigating in user's profile folder, and there are many [thousands] other user profile folders.
    if (SelectionHierarchyHeight > 0) and
       // Change of selection might indicate that the tree was rebuilt meanwhile and
       // the references in SelectionHierarchy might not be valid anymore
       (Selected = SelectionHierarchy[SelectionHierarchyHeight - 1]) and
       (Level < SelectionHierarchyHeight) and
       (Uppercase(GetDirName(SelectionHierarchy[Level])) = Dir) then
    begin
      Result := SelectionHierarchy[Level];
    end
      else
    begin
      // Paths have diverted
      SelectionHierarchyHeight := 0;

      Node := ParentNode.GetFirstChild;
      if (not Assigned(Node)) and (not ExistingOnly) then
      begin
        ValidateDirectoryEx(ParentNode, rsRecursiveExisting, True);
        Node := ParentNode.GetFirstChild;
      end;

      Result := nil;
      while (not Assigned(Result)) and Assigned(Node) do
      begin
        if UpperCase(GetDirName(Node)) = Dir then
        begin
          Result := Node;
        end
          else
        begin
          Node := ParentNode.GetNextChild(Node);
        end;
      end;
    end;

    if Assigned(Result) and (Length(Path) > 0) then
    begin
      Result := SearchSubDirs(Result, Path, Level + 1);
    end;
  end;

  function SearchSubDirs(ParentNode: TTreeNode; Path: string; Level: Integer): TTreeNode;
  var
    ParentPath, SubPath: string;
    SRec: TSearchRec;
    ParentNodeData: TNodeData;
  begin
    Result := nil;
    if Length(Path) > 0 then
    begin
      ParentNodeData := TNodeData(ParentNode.Data);
      if (not ParentNodeData.Scanned) and (not ExistingOnly) then
      begin
        ReadSubDirs(ParentNode);
      end;

      // Factored out of DoSearchSubDirs is remnant of Bug 956 superceded by Bug 1320
      Result := DoSearchSubDirs(ParentNode, Path, Level);

      if (not Assigned(Result)) and (not ExistingOnly) then
      begin
        ParentPath := NodePath(ParentNode);
        if DirectoryExists(ApiPath(IncludeTrailingBackslash(ParentPath) + Path)) then
        begin
          SubPath := IncludeTrailingBackslash(ParentPath) + ExcludeTrailingBackslash(ExtractFirstName(Path));
          if FindFirstSubDir(SubPath, SRec) then
          begin
            AddChildNode(ParentNode, ParentPath, SRec);
            if Assigned(ParentNodeData.DelayedExclude) then
              ParentNodeData.DelayedExclude.Add(SRec.Name);
            SortChildren(ParentNode, False);
            FindClose(SRec);
          end;
          Result := DoSearchSubDirs(ParentNode, Path, Level);
        end;
      end;
    end;
  end; {SearchSubDirs}

var
  Drive: string;
  P, I: Integer;
  RootNode, Node: TTreeNode;
begin {FindNodeToPath}
  Result := nil;
  if Length(Path) < 3 then
    Exit;

  // Particularly when used by TDirView to delegate browsing to
  // hidden drive view, the handle may not be created
  HandleNeeded;

  Drive := DriveInfo.GetDriveKey(Path);
  if (not Assigned(GetDriveStatus(Drive).RootNode)) and
     // hidden or possibly recently un-hidden by other drive view (refresh is pending)
     (DriveInfo.Get(Drive).Valid or DriveInfo.Get(Drive).ValidButHiddenByDrivePolicy) then
  begin
    if DriveInfo.Get(Drive).ValidButHiddenByDrivePolicy then
      DriveInfo.OverrideDrivePolicy(Drive);

    if DriveInfo.Get(Drive).Valid then
    begin
      CancelDriveRefresh; // cancel a possible pending refresh (see the previous comment)
      RefreshRootNodes(dsAll or dvdsRereadAllways); // overkill and is likely already called by GetDriveStatus
      DoRefreshDrives(False);
    end;
  end;

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
      if (not GetDriveStatus(Drive).Scanned) and (not ExistingOnly) then
      begin
        ScanDrive(Drive);
      end;
      Node := Selected;
      RootNode := GetDriveStatus(Drive).RootNode;
      if not Assigned(Node) then
      begin
        SelectionHierarchyHeight := 0;
      end
        else
      begin
        SelectionHierarchyHeight := Node.Level + 1;
        SetLength(SelectionHierarchy, SelectionHierarchyHeight);
        for I := SelectionHierarchyHeight - 1 downto 0 do
        begin
          SelectionHierarchy[I] := Node;
          Node := Node.Parent;
        end;
        Assert(Selected = SelectionHierarchy[SelectionHierarchyHeight - 1]);
        // Different drive - nothing to optimize
        if RootNode <> SelectionHierarchy[0] then
          SelectionHierarchyHeight := 0;
      end;
      Result := SearchSubDirs(RootNode, UpperCase(Path), 1);
    end
      else Result := GetDriveStatus(Drive).RootNode;
  end;
end; {FindNodetoPath}

function TDriveView.FindNodeToPath(Path: string): TTreeNode;
begin
  Result := DoFindNodeToPath(Path, False);
end;

function TDriveView.TryFindNodeToPath(Path: string): TTreeNode;
begin
  Result := DoFindNodeToPath(Path, True);
end;

function TDriveView.GetSubDir(var SRec: TSearchRec): Boolean;
begin
  Result := True;
  while Result and
        ((SRec.Name = '.' ) or
         (SRec.Name = '..') or
         ((SRec.Attr and faDirectory) = 0)) do
  begin
    if FindNext(SRec) <> 0 then
    begin
      Result := False;
    end;
  end;
end;

function TDriveView.FindFirstSubDir(Path: string; var SRec: TSearchRec): Boolean;
begin
  Result := (FindFirstEx(ApiPath(Path), DirAttrMask, SRec, FIND_FIRST_EX_LARGE_FETCH_PAS, FindExSearchLimitToDirectories) = 0);
  if Result then
  begin
    Result := GetSubDir(SRec);
    if not Result then FindClose(SRec);
  end;
end;

function TDriveView.FindNextSubDir(var SRec: TSearchRec): Boolean;
begin
  Result := (FindNext(SRec) = 0) and GetSubDir(SRec);
end;

function TDriveView.ReadSubDirsBatch(Node: TTreeNode; var SRec: TSearchRec; CheckInterval, Limit: Integer): Boolean;
var
  Start: TDateTime;
  Cont: Boolean;
  Path: string;
  Count: Integer;
  DelayedExclude: TStringList;
begin
  Start := Now;
  Path := NodePath(Node);
  Result := True;
  Count := 0;
  DelayedExclude := TNodeData(Node.Data).DelayedExclude;
  // At least from SetDirectory > DoFindNodeToPath and CanExpand, this is not called within BeginUpdate/EndUpdate block.
  // But in any case, adding it here makes expanding (which calls CanExpand) noticeably slower, when there are lot of nodes,
  // because EndUpdate triggers TVN_GETDISPINFO for all nodes in the tree.
  repeat
    if (not Assigned(DelayedExclude)) or
       (DelayedExclude.IndexOf(SRec.Name) < 0) then
    begin
      AddChildNode(Node, Path, SRec);
      Inc(Count);
    end;

    Cont := FindNextSubDir(SRec);

    // There are two other directory reading loops, where this is not checked
    if Cont and
       ((Count mod CheckInterval) = 0) and
       (Limit > 0) and
       (MilliSecondsBetween(Now, Start) > Limit) then
    begin
      Result := False;
      Cont := False;
    end
  until not Cont;

  if Result then
    FindClose(Srec);
end;

procedure TDriveView.DelayedNodeTimer(Sender: TObject);
var
  Node: TTreeNode;
  NodeData: TNodeData;
begin
  Assert(FDelayedNodes.Count > 0);
  if FDelayedNodes.Count > 0 then
  begin
    // Control was recreated
    if not Assigned(FDelayedNodes.Objects[0]) then
    begin
      FDelayedNodes.Objects[0] := TryFindNodeToPath(FDelayedNodes.Strings[0]);
    end;
    Node := TTreeNode(FDelayedNodes.Objects[0]);

    if not Assigned(Node) then
    begin
      FDelayedNodes.Delete(0);
    end
      else
    begin
      NodeData := TNodeData(Node.Data);
      if ReadSubDirsBatch(Node, NodeData.DelayedSrec, 10, 50) then
      begin
        FreeAndNil(NodeData.DelayedExclude);
        FDelayedNodes.Delete(0);
        SortChildren(Node, False);
      end;
    end;
  end;

  UpdateDelayedNodeTimer;
end;

procedure TDriveView.UpdateDelayedNodeTimer;
begin
  FDelayedNodeTimer.Enabled := HandleAllocated and (FDelayedNodes.Count > 0);
end;

procedure TDriveView.ReadSubDirs(Node: TTreeNode);
var
  SRec: TSearchRec;
  NodeData: TNodeData;
  Path: string;
  CheckInterval, Limit: Integer;
begin
  NodeData := TNodeData(Node.Data);
  Path := NodePath(Node);
  if not FindFirstSubDir(IncludeTrailingBackslash(Path) + '*.*', SRec) then
  begin
    Node.HasChildren := False;
  end
    else
  begin
    CheckInterval := 100;
    Limit := DriveViewLoadingTooLongLimit * 1000;
    if not Showing then
    begin
      Limit := Limit div 10;
      CheckInterval := CheckInterval div 10;
    end;
    if not ReadSubDirsBatch(Node, SRec, CheckInterval, Limit) then
    begin
      NodeData.DelayedSrec := SRec;
      NodeData.DelayedExclude := TStringList.Create;
      NodeData.DelayedExclude.CaseSensitive := False;
      NodeData.DelayedExclude.Sorted := True;
      FDelayedNodes.AddObject(Path, Node);
      Assert(FDelayedNodes.Count < 20); // if more, something went likely wrong
      UpdateDelayedNodeTimer;
    end;
    SortChildren(Node, False);
  end;

  NodeData.Scanned := True;

  Application.ProcessMessages;
end; {ReadSubDirs}

procedure TDriveView.CancelDelayedNode(Node: TTreeNode);
var
  NodeData: TNodeData;
begin
  NodeData := TNodeData(Node.Data);
  FindClose(NodeData.DelayedSrec);
  FreeAndNil(NodeData.DelayedExclude);
end;

procedure TDriveView.DeleteNode(Node: TTreeNode);
var
  ValidNode: TTreeNode;
begin
  if Assigned(Selected) and Assigned(Node.Parent) and
     ((Selected = Node) or Selected.HasAsParent(Node)) then
  begin
    ValidNode := Node.Parent;
    while (not NodePathExists(ValidNode)) and Assigned(ValidNode.Parent) do
      ValidNode := ValidNode.Parent;
    Selected := ValidNode;
  end;

  if DropTarget = Node then
    DropTarget := nil;

  Node.Delete;
end;

function TDriveView.CallBackValidateDir(var Node: TTreeNode; Data: Pointer): Boolean;
var
  WorkNode: TTreeNode;
  DelNode: TTreeNode;
  SRec: TSearchRec;
  SrecList: TStringList;
  SubDirList: TStringList;
  R: Boolean;
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
  if Assigned(Node.Parent) and (ScanDirInfo^.StartNode = Node) and
     (not NodePathExists(Node)) then
  begin
    DeleteNode(Node);
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
      // Nodes are sorted using natural sorting, while TStringList.Find uses simple sorting
      SubDirList.Sort;

      SRecList := TStringList.Create;
      SRecList.CaseSensitive := True;
      R := FindFirstSubDir(ParentDir + '*.*', SRec);
      while R do
      begin
        SrecList.Add(Srec.Name);
        if not SubDirList.Find(Srec.Name, Index) then
        {Subnode does not exists: add it:}
        begin
          AddChildNode(Node, ParentDir, SRec);
          NewDirFound  := True;
        end;
        R := FindNextSubDir(Srec);
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
          DeleteNode(DelNode);
        end
          else
        begin
          if (SrecList[Index] <> TNodeData(WorkNode.Data).DirName) then
          begin
            {Case of directory letters has changed:}
            TNodeData(WorkNode.Data).DirName := SrecList[Index];
            WorkNode.Text := SrecList[Index];
          end;
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
    ReadSubDirs(Node);
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
    if not Assigned(Node.Parent) then
      GetDriveStatus(Drive).ChangeTimer.Enabled := False;

    RestartWatchThread := WatchThreadActive;
    try
      if WatchThreadActive then
        StopWatchThread;

      FValidateFlag := True;
      FSysColorChangePending := False;

      New(Info);
      Info^.StartNode := Node;
      Info^.SearchNewDirs := NewDirs;
      Info^.DriveType := DriveInfo.Get(Drive).DriveType;

      SaveCanChange := FCanChange;
      FCanChange := True;
      FChangeFlag := False;
      Items.BeginUpdate;
      try
        IterateSubTree(Node, CallBackValidateDir, Recurse, coScanStartNode, Info);
      finally
        Items.EndUpdate;
      end;
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

      if FSysColorChangePending then
      begin
        FSysColorChangePending := False;
        if HandleAllocated then Perform(CM_SYSCOLORCHANGE, 0, 0);
      end;
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
    UpdateDriveNotifications(Drive);
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
  Drive: string;
begin
  Drive := DriveInfo.GetDriveKey((Sender as TDiscMonitor).Directories[0]);
  with GetDriveStatus(Drive) do
  begin
    DiscMonitor.Close;
  end;
  UpdateDriveNotifications(Drive);
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
  DriveStatusPair: TDriveStatusPair;
begin
  if (FChangeTimerSuspended = 0) and (Sender is TTimer) then
  begin
    for DriveStatusPair in FDriveStatus do
    begin
      if DriveStatusPair.Value.ChangeTimer = Sender then
      begin
        // Messages are processed during ValidateDirectory, so we may detect another change while
        // updating the directory. Prevent the recursion.
        // But retry the update afterwards (by reenabling the timer in ChangeDetected)
        SuspendChangeTimer;
        try
          with DriveStatusPair.Value.ChangeTimer do
          begin
            Interval := 0;
            Enabled := False;
          end;

          if Assigned(DriveStatusPair.Value.RootNode) then
          begin
            {Check also collapsed (invisible) subdirectories:}
            ValidateDirectory(DriveStatusPair.Value.RootNode);
          end;
        finally
          ResumeChangeTimer;
        end;
      end;
    end;
  end;
end; {ChangeTimerOnTimer}

procedure TDriveView.UpdateDriveNotifications(Drive: string);
var
  NeedNotifications: Boolean;
  Path: string;
  DevBroadcastHandle: DEV_BROADCAST_HANDLE;
  Size: Integer;
begin
  if DriveInfo.IsFixedDrive(Drive) then
  begin
    with GetDriveStatus(Drive) do
    begin
      NeedNotifications :=
        WatchThreadActive(Drive) and
        (DriveInfo.Get(Drive).DriveType <> DRIVE_REMOTE) and
        DriveInfo.Get(Drive).DriveReady;

      if NeedNotifications <> (DriveHandle <> INVALID_HANDLE_VALUE) then
      begin
        if NeedNotifications then
        begin
          Path := DriveInfo.GetDriveRoot(Drive);
          DriveHandle :=
            CreateFile(PChar(Path), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
            OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_ATTRIBUTE_NORMAL, 0);
          if DriveHandle <> INVALID_HANDLE_VALUE then
          begin
            Size := SizeOf(DevBroadcastHandle);
            ZeroMemory(@DevBroadcastHandle, Size);
            DevBroadcastHandle.dbch_size := Size;
            DevBroadcastHandle.dbch_devicetype := DBT_DEVTYP_HANDLE;
            DevBroadcastHandle.dbch_handle := DriveHandle;

            NotificationHandle :=
              RegisterDeviceNotification(FInternalWindowHandle, @DevBroadcastHandle, DEVICE_NOTIFY_WINDOW_HANDLE);
            if NotificationHandle = nil then
            begin
              CloseHandle(DriveHandle);
              DriveHandle := INVALID_HANDLE_VALUE;
            end;
          end;
        end
          else
        begin
          UnregisterDeviceNotification(NotificationHandle);
          NotificationHandle := nil;

          CloseHandle(DriveHandle);
          DriveHandle := INVALID_HANDLE_VALUE;
        end;
      end;
    end;
  end;
end;

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
  UpdateDriveNotifications(Drive);
end; {StartWatchThread}

procedure TDriveView.StopWatchThread;
var
  Drive: string;
begin
  if Assigned(Selected) then
  begin
    Drive := GetDriveToNode(Selected);
    with GetDriveStatus(Drive) do
      if Assigned(DiscMonitor) then
        DiscMonitor.Enabled := False;

    UpdateDriveNotifications(Drive);
  end;
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

  UpdateDriveNotifications(Drive);
end; {StopWatchThread}

procedure TDriveView.StartAllWatchThreads;
var
  DriveStatusPair: TDriveStatusPair;
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
        begin
          DiscMonitor.Open;
          UpdateDriveNotifications(DriveStatusPair.Key);
        end;
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
  DriveStatusPair: TDriveStatusPair;
begin
  if (csDesigning in ComponentState) or (not FWatchDirectory) then
     Exit;

  for DriveStatusPair in FDriveStatus do
    with DriveStatusPair.Value do
    begin
      if Assigned(DiscMonitor) then
      begin
        DiscMonitor.Close;
        UpdateDriveNotifications(DriveStatusPair.Key);
      end;
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

  if Assigned(Selected) and (not Assigned(Selected.Parent)) then
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

  if not Assigned(Node.Parent) then Result := GetDriveText(GetDriveToNode(Node))
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
  if not Assigned(FDragNode.Parent) then
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

    if Assigned(FDragNode.Parent) or (DragParentPath <> NodePathName(Selected.Parent)) then
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
  SourceNode, TargetNode: TTreeNode;
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
      // TDirView.PerformDragDropFileOperation validates the SourcePath and that actually seems correct
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
      // See comment in corresponding operation in TDirView.PerformDragDropFileOperation
      SourceNode := TryFindNodeToPath(SourceParentPath);
      if Assigned(SourceNode) then
        ValidateDirectory(SourceNode);
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
  Result := Assigned(Node) and Assigned(Node.Parent) and CopyToClipBoard(Node);
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

procedure TDriveView.CMSysColorChange(var Message: TMessage);
begin
  if not FValidateFlag then
  begin
    inherited;
  end
    else
  begin
    // Do not recreate the handle, if we are just iterating nodes, at that invalidates the node objects.
    // This is not perfect, as the handle can be recreated for other reasons.
    // But system color change is by far the most common case.
    FSysColorChangePending := True;
  end;
end;

end.
