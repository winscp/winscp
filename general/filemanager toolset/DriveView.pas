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


    V2.6:
    - Shows "shared"-symbol with directories
    - Delphi5 compatible

    For detailed documentation and history see TDriveView.htm.


{==================================================================}
interface

{$IFDEF USE_DRIVEVIEW}

{ Define ENHVALIDATE to scan all existing directories on a detected filesystem change:}
{.$DEFINE ENHVALIDATE}

{Required compiler options for TDriveView:}
{$A+,B-,X+,H+,P+}

uses
  Windows, Messages, SysUtils, Classes,  Graphics, Controls, Forms, ComObj,
  Dialogs, ComCtrls, ShellApi, CommCtrl, ExtCtrls, ActiveX,  ShlObj,
  DirView,
  ShellDialogs,
  DragDrop,
  DragDropFilesEx,
  FileChanges,
  FileOperator,
  DiscMon,
  IEDriveInfo,
  IEListView,
  PIDL,
  BaseUtils,
  ListExt,
  CustomDirView;

{$I ResStrings.pas}

const
{$IFNDEF NO_THREADS}
  msThreadChangeDelay = 50;
{$ENDIF}
  C_InvalidSize       = $FFFFFFFF;

  DDMaxSlowCount      = 3;
  DDVScrollDelay      = 2000000;
  DDHScrollDelay      = 2000000;
  DDDragStartDelay    = 500000;
  DDExpandDelay       = 25000000;

  ErrorNodeNA = '%s: Node not assigned';
  DirAttrMask = faDirectory or faSysFile or faHidden;

  {Flags used by TDriveView.RefreshRootNodes:}
  dvdsFloppy          = 8;  {Include floppy drives}
  dvdsRereadAllways   = 16; {Refresh drivestatus in any case}

  {Types uses by the function IterateSubTree:}
  {TRecursiveScan: determines, wich nodes are scanned by the function IterateSubTree:
  rsNoRecursive:       Scan startnode only.
  rsRecursive:         Scan all subnodes of the startnode.
  rsRecursiveExisting: Scan all subnodes of the startnode but not new created subnodes.
  rsRecursiveExpanded: Scan all expanded subnodes of the startnode.}
  {TScanStartnode: determines, wether the startnode should also be scanned:}

type
  TRecursiveScan = (rsNoRecursive, rsRecursive, rsRecursiveExisting, rsRecursiveExpanded);
  TScanStartNode = (coNoScanStartNode, coScanStartNode);

  TString12 = string[12];

  TCallBackFunc = function(var Node :TTreeNode; Data: Pointer): Boolean of object;

  ECreateShortCut  = class(Exception);
  EInvalidDirName  = class(Exception);
  EInvalidPath     = class(Exception);
  ENodeNotAssigned = class(Exception);

  TDriveStatus = record
    Scanned: Boolean;          {Drive allready scanned?}
    Verified: Boolean;         {Drive completly scanned?}
    RootNode: TTreeNode;       {Rootnode to drive}
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

  TDriveViewScanDirEvent    = Procedure(Sender: TObject; Node: TTreeNode; Var DoScanDir : Boolean) of object;
  TDriveViewDiskChangeEvent = Procedure(Sender: TObject; Drive : TDrive) of object;

  TDriveView = Class;

  TNodeData = Class
    Private
      FDirName  : String;
      FShortName: TString12;
      FAttr     : Integer;
      FScanned  : Boolean;
      FData     : Pointer;
      FExpanded : Boolean;
      FDrawBold : Boolean;
      FDirSize  : Cardinal;
      FisRecycleBin : Boolean;
      FIconEmpty : Boolean;

    Public
      shAttr : ULONG;
      PIDL   : PItemIDList;
      ShellFolder : iShellFolder;
      Property DirName      : String    Read FDirName   Write FDirName;
      Property ShortName    : TString12 Read FShortName Write FShortName;
      Property Attr         : Integer   Read Fattr      Write Fattr;
      Property Scanned      : Boolean   Read FScanned   Write FScanned;
      Property Data         : Pointer   Read FData      Write FData;
      Property Expanded     : Boolean   Read FExpanded  Write FExpanded;
      Property DrawBold     : Boolean   Read FDrawBold  Write FDrawBold;
      Property DirSize      : Cardinal  Read FDirSize   Write FDirSize;
      Property isRecycleBin : Boolean   Read FIsRecycleBin;
      Property IconEmpty    : Boolean   Read FIconEmpty Write FIconEmpty;
      Constructor Create;
      Destructor Destroy; Override;
  End;


{---------------------------------------------------------------}
  TDriveView = class(TCustomTreeView)
{---------------------------------------------------------------}
  private
{---------------------------------------------------------------}
    DriveStatus     : Array[FirstDrive .. LastDrive] Of TDriveStatus;

    FConfirmDelete    : Boolean;
    FConfirmOverwrite : Boolean;
    FWatchDirectory   : Boolean;
    FDirectory        : String;
    FFullDriveScan    : Boolean;
    FDimmHiddenDirs   : Boolean;
    FColorBold        : TColor;
    FShowDirSize      : Boolean;
    FShowVolLabel     : Boolean;
    FVolDisplayStyle  : TVolumeDisplayStyle;
    FUseSystemContextMenu : Boolean;
    FContinue         : Boolean;
    FShowAnimation    : Boolean;
    FChangeFlag       : Boolean;
    FContextMenu      : Boolean;
    FLastDir          : String;
    FValidateFlag     : Boolean;
    FCreating         : Boolean;
    FParentForm       : TCustomForm;
    FReadDrives       : Boolean;
    FForceRename      : Boolean;
    FRenameNode       : TTreeNode;
    FLastRenameName   : String;

    FDesktop          : iShellFolder;
    FWorkPlace        : iShellFolder;
    {Additional events:}
    FOnStartScan      : TNotifyEvent;
    FOnEndScan        : TNotifyEvent;
    FOnScanDir        : TDriveViewScanDirEvent;
    FOnDiskChange         : TDriveViewDiskChangeEvent;
    FOnInsertedDiskChange : TDriveViewDiskChangeEvent;
    FOnChangeDetected : TDriveViewDiskChangeEvent;
    FOnChangeInvalid  : TDriveViewDiskChangeEvent;
    FOnDisplayContextMenu: TNotifyEvent;

    {used components:}
    FDirView          : TDirView;
    FDriveBox         : TObject;
    FFileOperator     : TFileOperator;

    FChangeInterval   : Cardinal;
    FCanChange        : Boolean;

    FDragImageList    : TDragImageList;
    FNoCheckDrives    : String;
    FCompressedColor  : TColor;
    FFileNameDisplay  : TFileNameDisplay;

    {Drag&drop:}
    FDragDrive              : TDrive;
    DragFileList            : TStringList;
    DragNode                : TTreeNode;
    FDD                     : TDragDropFilesEx;
    DragOverTime            : FILETIME;
    DragStartTime           : FILETIME;
    LastVScrollTime         : FILETIME;
    LastHScrollTime         : FILETIME;
    VScrollCount            : Integer;
    FLastPathCut            : String;
    FTargetPopUpMenu        : Boolean;
    FUseDragImages          : Boolean;
    FStartPos               : TPoint;
    FDragPos                : TPoint;
    FExeDrag                : Boolean;
    FDDLinkOnExeDrag        : Boolean;

    FOnDDDragEnter          : TDDOnDragEnter;
    FOnDDDragLeave          : TDDOnDragLeave;
    FOnDDDragOver           : TDDOnDragOver;
    FOnDDDrop               : TDDOnDrop;
    FOnDDQueryContinueDrag  : TDDOnQueryContinueDrag;
    FOnDDGiveFeedback       : TDDOnGiveFeedback;
    FOnDDDragDetect         : TDDOnDragDetect;
    FOnDDProcessDropped     : TOnProcessDropped;
    FOnDDError              : TDDErrorEvent;
    FOnDDExecuted           : TDDExecutedEvent;
    FOnDDFileOperation      : TDDFileOperationEvent;
    FOnDDFileOperationExecuted : TDDFileOperationExecutedEvent;

    {Drag&Drop eventhandling:}
    Procedure DDDragEnter(DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: longint; var Accept:boolean);
    Procedure DDDragLeave;
    Procedure DDDragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: longint);
    Procedure DDDrop(DataObj: IDataObject; grfKeyState: Longint;  pt: TPoint; var dwEffect: longint);
    Procedure DDQueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint; var Result: HResult);
    Procedure DDGiveFeedback(dwEffect: Longint; var Result: HResult);
    Procedure DDDragDetect(grfKeyState: Longint; DetectStart, Pt: TPoint; DragStatus:TDragDetectStatus);
    Procedure DDProcessDropped(Sender: TObject; grfKeyState: Longint;  pt: TPoint; dwEffect: Longint);
    Procedure DDSpecifyDropTarget(Sender: TObject; DragDropHandler : boolean; pt: TPoint; var pidlFQ : PItemIDList; var Filename : string);

    Procedure SetTargetPopUpMenu(PopMe : Boolean);

    {Drag&drop helper functions:}
    Procedure SignalDirDelete(Sender: TObject; Files : TStringList);

    Function CheckForSubDirs(Path: String) : Boolean;
    Function ReadSubDirs(Node : TTreeNode; DriveType: Integer) : Boolean;

    {Callback-functions used by iteratesubtree:}
    Function  CallBackValidateDir      (Var Node : TTreeNode; Data: Pointer) : Boolean;
    Function  CallBackSaveNodeState    (Var Node : TTreeNode; Data: Pointer) : Boolean;
    Function  CallBackRestoreNodeState (Var Node : TTreeNode; Data: Pointer) : Boolean;
    Function  CallBackDisplayName      (Var Node : TTreeNode; Data: Pointer) : Boolean;
    Function  CallBackSetDirSize       (Var Node : TTreeNode; Data: Pointer) : Boolean;
    Function  CallBackExpandLevel      (Var Node : TTreeNode; Data: Pointer) : Boolean;

    {Notification procedures used by component TDiscMonitor:}
    Procedure ChangeDetected(Sender: TObject);
    Procedure ChangeInvalid(Sender: TObject);

    {Notification procedure used by component TTimer:}
    Procedure ChangeTimerOnTimer(Sender : TObject);

    {Special procedure for events OnEdited / OnDrawItem. Used to overwrite these events:}
    Procedure InternalOnDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);

{---------------------------------------------------------------}
  protected
{---------------------------------------------------------------}
    Procedure SetSelected(Node : TTreeNode);
    Procedure SetFullDriveScan(DoFullDriveScan : Boolean);
    Procedure SetWatchDirectory(Watch : Boolean);
    Procedure SetShowDirSize(ShowIt : Boolean);
    Procedure SetShowVolLabel(ShowIt : Boolean);
    Procedure SetVolDisplayStyle(doStyle : TVolumeDisplayStyle);
    Procedure SetDirView(DV : TDirView);
    Procedure SetChangeInterval(Interval : Cardinal);
    Procedure SetDimmHiddenDirs(DimmIt : Boolean);
    Procedure SetNoCheckDrives(Value : String);
    Procedure SetCompressedColor(Value : TColor);
    Procedure SetFileNameDisplay(Value : TFileNameDisplay);
    Function  GetDirectory : String;
    Procedure SetDirectory(Path : String);
    Procedure SetDrive(Drive : TDrive);
    Function  GetDrive : TDrive;
    Function  GetNodeFromHItem(Item: TTVItem): TTreeNode;
    Procedure GetNodeShellAttr(ParentFolder : iShellFolder; NodeData : TNodeData; Path : String; ContentMask : Boolean = True);
    Function  DoScanDir(FromNode : TTreeNode) : Boolean;                           Virtual;
    Function  AddChildNode(ParentNode : TTreeNode; SRec : TSearchRec) : TTreeNode; Virtual;
{$IFNDEF NO_THREADS}
    Procedure CreateWatchThread(Drive : TDrive);                                   Virtual;
{$ENDIF}

    Procedure SetLastPathCut(Path : String);
    Function  GetCanUndoCopyMove : Boolean;                 Virtual;
    Procedure DDError(ErrorNo : TDDError);                  Dynamic;
    Procedure CNNotify(Var Msg: TWMNotify);                 Message CN_NOTIFY;
    Procedure CreateWnd;                                    Override;
    Procedure Edit(Const Item: TTVItem);                    Override;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); Override;

    Procedure WMLButtonDown(var Msg: TWMLButtonDown);       Message WM_LBUTTONDOWN;
    Procedure WMLButtonUp  (var Msg: TWMLButtonDown);       Message WM_LBUTTONUP;
    Procedure WMRButtonDown(Var Msg: TWMRButtonDown);       Message WM_RBUTTONDOWN;
    Procedure WMContextMenu(Var Msg: TWMContextMenu);       Message WM_CONTEXTMENU;
    Procedure WMUserRename(Var Message : TMessage);         Message WM_USER_RENAME;
{---------------------------------------------------------------}
  public
{---------------------------------------------------------------}
    {Runtime-only properties:}

    property  Images;
    Property  StateImages;
    Property  Items Stored False;
    Property  Selected Write SetSelected Stored False;

    Property WorkPlace : iShellFolder Read FWorkPlace;

    Property  DragImageList : TDragImageList Read FDragImageList;

    Property  Continue : Boolean Read FContinue
                                 Write FContinue;

    Property DriveBox : TObject Read FDriveBox
                                Write FDriveBox;

   {Current drive:}
   Property Drive       : TDrive Read GetDrive
                                 Write SetDrive
                                 Stored False;

    Property DragDropFilesEx : TDragDropFilesEx Read FDD;
    Property DragDrive       : TDrive Read FDragDrive;
    Property CanUndoCopyMove : Boolean Read GetCanUndoCopyMove;
    Property DDFileOperator  : TFileOperator Read FFileOperator;
    Property LastPathCut     : String        Read FLastPathCut
                                             Write SetLastPathCut;

    Function  UndoCopyMove : Boolean;                          Dynamic;
    Procedure EmptyClipboard;                                  Dynamic;
    Function  CopyToClipBoard(Node : TTreeNode) : Boolean;     Dynamic;
    Function  CutToClipBoard(Node : TTreeNode)  : Boolean;     Dynamic;
    Function  CanPasteFromClipBoard : Boolean;                 Dynamic;
    Function  PasteFromClipBoard(TargetPath : String = '') : Boolean; Dynamic;
    Procedure PerformDragDropFileOperation(TargetPath : String; dwEffect: Integer; isRecycleBin : Boolean);

    {Drive handling:}
    Function  GetDriveStatus(Drive : TDrive) : TDriveStatus;
    Function  GetDriveTypetoNode(Node : TTreeNode) : Integer;  {Returns DRIVE_CDROM etc..}
    Function  GetDriveType(Drive : TDrive) : Integer;           {Returns DRIVE_CDROM etc..}
    Function  GetDriveToNode(Node : TTreeNode) : Char;
    Function  GetDriveText(Drive : TDrive) : String;
    Procedure ScanDrive(Drive : TDrive);
    Procedure RefreshRootNodes(ScanDirectory : Boolean; dsFlags : Integer);
    Function  GetValidDrivesStr : String;
    Procedure RefreshDirSize(Node : TTreeNode);
    Procedure RefreshDriveDirSize(Drive : TDrive);

    {Node handling:}
    Procedure SetImageIndex(Node : TTreeNode);                                     Virtual;
    Function  HasSubNodes(Node : TTreeNode) : Boolean;
    Function  FindNodeToPath(Path : String) : TTreeNode;
    Procedure SetBoldDraw(Node : TTreeNode; BoldDraw : Boolean);                   Dynamic;
    Function  NodeVerified(Node : TTreeNode) : Boolean;
    Function  NodeAttr(Node : TTreeNode) : Integer;
    Function  RootNode(Node : TTreeNode) : TTreeNode;
    Function  GetDirPathName(Node: TTreeNode) : String;
    Function  GetDirPath (Node : TTreeNode) : String;
    Function  GetDirName(Node : TTreeNode)   : String;
    Procedure CenterNode(Node : TTreeNode);                                        Virtual;
    Function  SortChildren(ParentNode : TTreeNode; Recurse : Boolean) : Boolean;   Virtual;
    Function  GetDirSize(Node : TTreeNode) : Cardinal;                             Virtual;
    Procedure SetDirSize(Node : TTreeNode);                                        Virtual;
    Function  GetDisplayName(Node : TTreeNode) : String;
    Function  NodeUpdateAble(Node : TTreeNode) : Boolean;                          Virtual;
    Function  FormatDirSize(Size : Cardinal) : String;                             Virtual;
    Procedure ExpandLevel(Node : TTreeNode; Level : Integer);                      Virtual;

    Function  GetFQPIDL(Node : TTreeNode) : PItemIDList;

    Procedure ValidateDirectoryEx(Node : TTreeNode;
                                  Recurse : TRecursiveScan;
                                  NewDirs : Boolean);                              Virtual;
    Procedure ValidateDirectory(Node : TTreeNode);                                 Virtual;
    Procedure ValidateDirectoryEasy(Node : TTreeNode);                             Virtual;
    Procedure ValidateVisibleDirectories(Node : TTreeNode);                        Virtual;
    Procedure ValidateAllDirectories(Node : TTreeNode);                            Dynamic;
    Function  GetSubTreeSize(Node : TTreeNode) : Integer;                          Dynamic;

    {Directory update:}
    Function  CreateDirectory(ParentNode : TTreeNode; NewName : String) : TTreeNode; Dynamic;
    Function  DeleteDirectory(Node: TTreeNode; AllowUndo : Boolean)     : Boolean;   Dynamic;

    Procedure DeleteSubNodes(Node : TTreeNode);                                      Dynamic;

    {Basic recursive function for scanning a subtree:}
    Function  IterateSubTree(Var StartNode : TTreeNode;
                             CallBackFunc  : TCallBackFunc;
                             Recurse       : TRecursiveScan;
                             ScanStartNode : TScanStartNode;
                             Data          : Pointer) : Boolean;

    constructor Create(AOwner: TComponent);           Override;
    Destructor  Destroy;                              Override;

    {Save and restore the subnodes expanded state:}
    Procedure SaveNodesState(Node : TTreeNode);
    Procedure RestoreNodesState(Node : TTreeNode);

    {Menu-handling:}
    Procedure DisplayContextMenu(Node : TTreeNode);                     Overload;
    Procedure DisplayContextMenu(Node : TTreeNode; ScreenPos : TPoint); Overload;

    Procedure DisplayPropertiesMenu(Node : TTreeNode);    Dynamic;

{$IFNDEF NO_THREADS}
    {Watchthread handling:}
    Procedure StartWatchThread;                           Virtual;
    Procedure StopWatchThread;                            Virtual;
    Procedure TerminateWatchThread(Drive : TDrive);       Virtual;
    Procedure StartAllWatchThreads;                       Virtual;
    Procedure StopAllWatchThreads;                        Virtual;
    Function  WatchThreadActive : Boolean;                 Overload;
    Function  WatchThreadActive(Drive : TDrive) : Boolean; Overload;
    Function  NodeWatched(Node : TTreeNode) : Boolean;    Virtual;
{$ENDIF}

    (* Modified Events: *)
    Procedure GetImageIndex(Node: TTreeNode);             Override;
    Function  CanEdit(Node: TTreeNode) : Boolean;         Override;
    Function  CanChange(Node: TTreeNode): Boolean;        Override;
    Function  CanExpand(Node: TTreeNode): Boolean;        Override;
    Procedure Delete(Node: TTreeNode);                    Override;
    Procedure Loaded;                                     Override;
    Procedure KeyDown(var Key: Word; Shift: TShiftState); Override;
    Procedure KeyPress(Var Key : Char);                   Override;
    Procedure KeyUp(var Key: Word; Shift: TShiftState);   Override;
    Procedure Change(Node: TTreeNode);                    Override;

{---------------------------------------------------------------}
  published
{---------------------------------------------------------------}
   {Additional properties:}

   {Current selected directory:}
   Property Directory   : String          Read GetDirectory
                                          Write SetDirectory;

   {Confirm deleting directories:}
   Property ConfirmDelete       : Boolean Read fConfirmDelete
                                          Write fConfirmDelete
                                          Default True;

   {Confirm overwriting directories:}
   Property ConfirmOverwrite    : Boolean Read fConfirmOverwrite
                                          Write fConfirmOverwrite
                                          Default True;

   {Scan all directories in method ScanDrive:}
   Property FullDriveScan     : Boolean Read fFullDriveScan
                                        Write SetFullDriveScan;

   Property DimmHiddenDirs     : Boolean Read fDimmHiddenDirs
                                        Write SetDimmHiddenDirs;

   {Enable automatic update on filesystem changes:}
   Property WatchDirectory    : Boolean Read fWatchDirectory
                                        Write SetWatchDirectory;

   {Peform automatic update after ChangeInterval milliseconds:}
   Property ChangeInterval    : Cardinal Read fChangeInterval
                                         Write SetChangeInterval
                                         Default 1000;

   {Enables or disables the system context menu for a directory:}
   Property UseSystemContextMenu : Boolean Read FUseSystemContextMenu
                                           Write FUseSystemContextMenu
                                           Default True;

   {Linked component TDirView:}
   Property DirView           : TDirView Read fDirView
                                         Write SetDirView;

   Property ColorBold         : TColor  Read fColorBold
                                        Write fColorBold
                                        Default clBlue;

   Property ShowDirSize       : Boolean Read fShowDirSize
                                        Write SetShowDirSize;

   {Show the volume labels of drives:}
   Property ShowVolLabel      : Boolean Read fShowVolLabel
                                        Write SetShowVolLabel;

   {How to display the drives volume labels:}
   Property VolDisplayStyle   : TVolumeDisplayStyle Read fVolDisplayStyle
                                                    Write SetVolDisplayStyle
                                                    Default doPrettyName;

   {Show AVI-animation when performing a full drive scan:}
   Property ShowAnimation    : Boolean Read  FShowAnimation
                                       Write FShowAnimation;

   {Don't watch these drives for changes:}
   Property NoCheckDrives    : String  Read  FNoCheckDrives
                                       Write SetNoCheckDrives;

   Property ReadDrives       : Boolean Read FReadDrives
                                       Write FReadDrives
                                       Default True;

   Property CompressedColor  : TColor  Read FCompressedColor
                                       Write SetCompressedColor
                                       Default clBlue;

    Property FileNameDisplay : TFileNameDisplay Read FFileNameDisplay
                                                Write SetFileNameDisplay;

   {Additional events:}
   Property OnStartScan : TNotifyEvent Read fOnStartScan
                                       Write fOnStartScan;

   Property OnEndScan   : TNotifyEvent Read fOnEndScan
                                       Write fOnEndScan;

   Property OnScanDir   : TDriveViewScanDirEvent    Read fOnScanDir
                                                    Write fOnScanDir;

   Property OnDiskChange: TDriveViewDiskChangeEvent Read fOnDiskChange
                                                    Write fOnDiskChange;

   Property OnInsertedDiskChange: TDriveViewDiskChangeEvent Read fOnInsertedDiskChange
                                                           Write fOnInsertedDiskChange;

   Property OnChangeDetected : TDriveViewDiskChangeEvent Read fOnChangeDetected
                                                         Write fOnChangeDetected;

   Property OnChangeInvalid  : TDriveViewDiskChangeEvent Read fOnChangeInvalid
                                                         Write fOnChangeInvalid;

   Property OnDisplayContextMenu: TNotifyEvent           Read FOnDisplayContextMenu
                                                         Write FOnDisplayContextMenu;

    {Drag&Drop properties:}
    Property DDLinkOnExeDrag       : Boolean      Read FDDLinkOnExeDrag
                                                  Write FDDLinkOnExeDrag
                                                  Default True;

    {Show drag images during a drag&drop operation:}
    Property UseDragImages         : Boolean      Read FUseDragImages
                                                  Write FUseDragImages
                                                  Default True;

    {Show popupmenu when dropping a file with the right mouse button:}
    Property TargetPopUpMenu       : Boolean      Read FTargetPopUpMenu
                                                  Write SetTargetPopUpMenu
                                                  Default True;

    {The mouse has entered the component window as a target of a drag&drop operation:}
    Property OnDDDragEnter         : TDDOnDragEnter Read FOnDDDragEnter
                                                    Write FOnDDDragEnter;

    {The mouse has leaved the component window as a target of a drag&drop operation:}
    Property OnDDDragLeave         : TDDOnDragLeave Read FOnDDDragLeave
                                                    Write FOnDDDragLeave;

    {The mouse is dragging in the component window as a target of a drag&drop operation:}
    Property OnDDDragOver          : TDDOnDragOver  Read FOnDDDragOver
                                                    Write FOnDDDragOver;
    {The Drag&drop operation is about to be executed:}
    Property OnDDDrop              : TDDOnDrop      Read FOnDDDrop
                                                    Write FOnDDDrop;

    Property OnDDQueryContinueDrag : TDDOnQueryContinueDrag  Read FOnDDQueryContinueDrag
                                                             Write FOnDDQueryContinueDrag;

    Property OnDDGiveFeedback      : TDDOnGiveFeedback Read FOnDDGiveFeedback
                                                       Write FOnDDGiveFeedback;

    {A drag&drop operation is about to be initiated whith the components window as the
     source:}
    Property OnDDDragDetect        : TDDOnDragDetect   Read FOnDDDragDetect
                                                       Write FOnDDDragDetect;

    {The component window is the target of a drag&drop operation:}
    Property OnDDProcessDropped    : TOnProcessDropped Read FOnDDProcessDropped
                                                       Write FOnDDProcessDropped;

    {An error has occured during a drag&drop operation:}
    Property OnDDError             : TDDErrorEvent Read FOnDDError
                                                   Write FOnDDError;

    {The drag&drop operation has been executed:}
    Property OnDDExecuted          : TDDExecutedEvent Read FOnDDExecuted
                                                      Write FOnDDExecuted;
    {Event is fired just before executing the fileoperation. This event is also fired when
     files are pasted from the clipboard:}
    Property OnDDFileOperation     : TDDFileOperationEvent Read FOnDDFileOperation
                                                           Write FOnDDFileOperation;

    {Event is fired after executing the fileoperation. This event is also fired when
     files are pasted from the clipboard:}
    Property OnDDFileOperationExecuted  : TDDFileOperationExecutedEvent Read FOnDDFileOperationExecuted
                                                                        Write FOnDDFileOperationExecuted;

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
   {Delphi's drag&drop is not compatible with the OLE windows drag&drop:}
   property DragKind;
   property DragCursor;
   property DragMode;
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
   property ParentFont;
   property ParentShowHint;
   property PopupMenu;
   property ReadOnly;
   property RightClickSelect;
   property RowSelect;
   property ShowButtons;
   property ShowHint;
   property ShowLines;
  {property ShowRoot;}
  {property SortType;}
   property TabOrder;
   property TabStop;
   property ToolTips;
   property Visible;
   property OnChange;
   property OnChanging;
   property OnClick;
   property OnCollapsing;
   property OnCollapsed;
   property OnCompare;
   {Internal used events:
   property OnCustomDraw;
   property OnCustomDrawItem;}
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
{---------------------------------------------------------------}



// ===========================================================
// Other service procedures and functions:}
// ===========================================================

procedure Register;

{$ENDIF}

{==============================================================}
implementation

{$IFDEF USE_DRIVEVIEW}

{==============================================================}
uses IEComboBox;

resourceString
   English_ErrorInvalidDirName = 'New name contains Invalid characters:';
   English_DragDropError = 'DragDrop Error: %d';

{MP}{   German_ErrorInvalidDirName  = 'Verzeichnisname enthält ungültige Zeichen:';
   French_ErrorInvalidDirName = 'Le nouveau nom contient des caractères invalides:';}

Type
  PInt      = ^Integer;

  TLogFileNode = Record
       Level : Integer;
       Attrs : Integer;
       ShortName : array[0..13] of AnsiChar;
       NameLen : Integer;
  End;

  TLogFileHeader = Record
       ID : String[10];
       Version : String[3];
  End;

// ===========================================================
// Global variables
// ===========================================================


Var ErrorInvalidDirName : String;




procedure Register;
begin
  {MP}RegisterComponents({'IE'}'DriveDir', [TDriveView]);
end;  {Register}


Constructor TNodeData.Create;
Begin
  Inherited Create;
  FAttr         := 0;
  FDrawBold     := False;
  FExpanded     := False;
  FScanned      := False;
  FDirName      := '';
  FShortName    := '';
  FDirSize      := C_InvalidSize;
  FIsRecycleBin := False;
  FIconEmpty    := True;
  shAttr        := 0;
  PIDL          := NIL;
  ShellFolder   := NIL;
End; {TNodeData.Create}


Destructor TNodeData.Destroy;
Begin
  SetLength(fDirName, 0);
  IF Assigned(PIDL) Then
  FreePIDL(PIDL);
  Inherited Destroy;
End; {TNodeData.Destroy}


Function TDriveView.GetFQPIDL(Node : TTreeNode) : PItemIDList;
Var WStr   : WideString;
    Eaten  : ULONG;
    shAttr : ULONG;

Begin
  Result := NIL;
  IF Assigned(Node) Then
  Begin
    WStr := GetDirPathName(Node);
    FDesktop.ParseDisplayName(FParentForm.Handle, NIL, PWideChar(WStr), Eaten, Result, shAttr);
  End;
End; {GetFQPIDL}

// ===========================================================
// Class TDriveView:
// ===========================================================


(* -------------------------*)
(* Events:                  *)
(* -------------------------*)

(* Overwrite Event OnCustomDraw: *)


Procedure TDriveview.InternalOnDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
Begin
  IF Assigned(Node) And Assigned(Node.Data) And (Node <> DropTarget) Then
  With TNodeData(Node.Data) Do
  IF Not Node.Selected Then
  Begin
    {Colored display of compressed directories:}
    IF Bool(Attr And FILE_ATTRIBUTE_COMPRESSED) Then
    Canvas.Font.Color := FCompressedColor
    Else
    {Dimmed display, if hidden-atrribut set:}
    IF fDimmHiddenDirs And Bool(Attr And FILE_ATTRIBUTE_HIDDEN) Then
    Canvas.Font.Color := clGrayText
    Else

    IF DrawBold Then
    Begin
      Canvas.Font.Color := fColorBold;
      Canvas.Font.Style := Canvas.Font.Style + [fsBold];
    End;        End
  Else
    {HideSelection:}
    IF Not Self.Focused And HideSelection Then
    Begin
        Canvas.Brush.Color := clBtnFace;
        Canvas.Font.Color := clBtnText;
    End;
End; {InternalOnDrawItem}


(* Overwrite Event OnEditing: *)

Function TDriveView.CanEdit(Node: TTreeNode) : Boolean;
Begin
  Result := Inherited CanEdit(Node) Or FForceRename;
  IF Result Then
  Result := Assigned(Node.Parent) And
            Not TNodeData(Node.Data).isRecycleBin And
            Not ReadOnly And
            (FDD.DragDetectStatus <> ddsDrag) And
            (TNodeData(Node.Data).Attr and (faReadOnly or faSysFile) = 0) And
            (UpperCase(Node.Text) = UpperCase(GetDirName(Node)));
  FForceRename := False;
End; {CanEdit}


(* event OnEdited: *)

procedure TDriveView.Edit(const Item: TTVItem);
Var NewDirName : String;
    SRec       : TSearchRec;
    Node       : TTreeNode;
    Info       : String;
    i          : Integer;

Begin
  Node := GetNodeFromHItem(Item);
  IF (Length(Item.pszText) > 0) And (Item.pszText <> Node.Text) Then
  Begin
    IF StrContains(coInvalidDosChars, Item.pszText) Then
    Begin
      Info := coInvalidDosChars;
      For i := Length(Info) DownTo 1 Do
      System.Insert(Space, Info, i);

      IF Assigned(OnEdited) Then
      Begin
        NewDirName := Node.Text;
        OnEdited(Self, Node, NewDirName);
      End;
      IF Length(Item.pszText) > 0 Then
        Raise EInvalidDirName.Create(ErrorInvalidDirName + Space + Info);
      Exit;
    End;

{$IFNDEF NO_THREADS}
    StopWatchThread;
    IF Assigned(DirView) Then
    DirView.StopWatchThread;
{$ENDIF}

    With FFileOperator Do
    Begin
      Flags := [foAllowUndo, foNoConfirmation];
      Operation := foRename;
      OperandFrom.Clear;
      OperandTo.Clear;
      OperandFrom.Add(GetDirPath(Node));
      OperandTo.Add(AddSlash(GetDirPath(Node.Parent)) + Item.pszText);
    End;

    Try
      IF FFileOperator.Execute Then
      {IF RenameFile(GetDirPath(Node), AddSlash(GetDirPath(Node.Parent)) + Item.pszText) Then}
      Begin
        Node.Text := Item.pszText;
        TNodeData(Node.Data).Dirname := Item.pszText;
        IF FindFirst(AddSlash(GetDirPath(Node.Parent)) + Item.pszText, faAnyFile, Srec) = 0 Then
          TNodeData(Node.Data).ShortName := Srec.FindData.cAlternateFileName;
        FindClose(Srec);
        SortChildren(Node.Parent, False);

        Inherited Edit(Item);
      End
      Else
      Begin
        {
         Raise ERenameFileFailed.Create(ErrorRenameFile + Item.pszText);
         }
        IF FileOrDirExists(AddSlash(GetDirPath(Node.Parent)) + Item.pszText) Then
          Info := SErrorRenameFileExists + Item.pszText
        Else
          Info := SErrorRenameFile + Item.pszText;

        MessageBeep(MB_ICONHAND);
        IF MessageDlg(Info, mtError, [mbOK, mbAbort], 0) = mrOK Then
        Begin
          FLastRenameName := Item.pszText;
          FRenameNode     := Node;
          PostMessage(Self. Handle, WM_USER_RENAME, 0 , 0);
        End;

      End;
    Finally
{$IFNDEF NO_THREADS}
      StartWatchThread;
{$ENDIF}
      IF Assigned(DirView) Then
      Begin
        DirView.Reload2;
{$IFNDEF NO_THREADS}
        DirView.StartWatchThread;
{$ENDIF}
      End;
    End;
  End;
End; {Edit}


Procedure TDriveView.WMUserRename(Var Message : TMessage);
Begin
  IF Assigned(FRenameNode) Then
  Begin
    FForceRename := True;
    TreeView_EditLabel(Handle, FRenameNode.ItemID);
    SetWindowText(TreeView_GetEditControl(Self.Handle), PChar(FLastRenameName));
    FRenameNode := NIL;
  End;
End; {WMUserRename}


(* Overwrite Event OnCanChange: *)

Function  TDriveView.CanChange(Node: TTreeNode): Boolean;
Begin
  Result := Inherited CanChange(Node);
  IF Result              And
     Not FCanChange      And
     Assigned(Node)      And
     Assigned(Node.Data) And
     Assigned(Selected)  And
     Assigned(Selected.Data) Then
  Begin
    DropTarget := Node;
    Result := False;
  End
  Else
  DropTarget := NIL;
End; {CanChange}


(* Overwrite Event OnExpanding: *)

Function  TDriveView.CanExpand(Node: TTreeNode): Boolean;
Var SubNode : TTreeNode;
    Drive   : TDrive;
    SaveCursor : TCursor;

Begin
  Result := Inherited CanExpand(Node);
  Drive := GetDriveToNode(Node);
  IF Node.HasChildren Then
  Begin
    IF (Node.Level = 0) And
        Not DriveStatus[Drive].Scanned  And
        (Drive >= FirstFixedDrive)      Then
    Begin
      SubNode := Node.GetFirstChild;
      IF Not Assigned(SubNode) Then
      Begin
        ScanDrive(Drive);
        SubNode             := Node.GetFirstChild;
        Node.HasChildren    := Assigned(SubNode);
        Result              := Node.HasChildren;
        IF Not Assigned(DriveStatus[Drive].DiscMonitor) Then
{$IFNDEF NO_THREADS}
        CreateWatchThread(Drive);
{$ENDIF}
      End;
    End
    Else
    Begin
      SaveCursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
      Try
        IF Not TNodeData(Node.Data).Scanned And DoScanDir(Node) Then
        Begin
          ReadSubDirs(Node, DriveInfo[Drive].DriveType);
        End;
      Finally
        Screen.Cursor := SaveCursor;
      End;
    End;
  End;
End; {CanExpand}


(* Overwrite event OnGetImageIndex: *)

procedure TDriveView.GetImageIndex(Node: TTreeNode);
Begin
  IF TNodeData(Node.Data).IconEmpty Then
  SetImageIndex(Node);
  Inherited GetImageIndex(Node);
End; {GetImageIndex}


(* Overwrite event Loaded: *)

Procedure TDriveView.Loaded;
Begin
  Inherited Loaded;
  {Create the drive nodes:}
  RefreshRootNodes(False, dsDisplayName Or dvdsFloppy);
  {Set the initial directory:}
  IF (Length(FDirectory) > 0) And DirExists(FDirectory) Then
  Directory := FDirectory;

  fCreating := FALSE;
End; {Loaded}



(* Overwrite event OnDeletion: *)
Procedure TDriveView.Delete(Node: TTreeNode);
Var NodeData : TNodeData;

Begin
  If Node = DragNode Then
  DragNode := NIL;
  IF Node = DropTarget Then
  Begin
    DropTarget := NIL;
    Update;
  End;

  NodeData := NIL;

  IF Assigned(Node) And Assigned(Node.Data) Then
  NodeData := TNodeData(Node.Data);
  Node.Data := NIL;

  Inherited Delete(Node);

  If Assigned(NodeData) Then
  NodeData.Destroy;
End; {OnDelete}


(* Overwrite event OnKeyDown: *)

procedure TDriveView.KeyDown(var Key: Word; Shift: TShiftState);
Begin
  IF (Key = VK_RETURN) And
     (ssAlt in Shift)  And
     Not isEditing     And
     Assigned(Selected) Then
  Begin
     DisplayPropertiesMenu(Selected);
     Key := 0;
  End;
  Inherited KeyDown(Key, Shift);
End; {KeyDown}


(* Overwrite event OnKeyPress: *)

Procedure TDriveView.KeyPress(Var Key : Char);
Begin
  IF Assigned(Selected) Then
  Begin
    IF Not isEditing Then
    Case Key of
      #13, ' ':
           Begin
             Selected.Expanded := Not Selected.Expanded;
             Key := #0;
           End;
      '/': Begin
             Selected.Collapse(True);
             Selected.MakeVisible;
             Key := #0;
           End;
      '*': Selected.MakeVisible;
    End {Case}
    Else
    IF (Pos(Key, coInvalidDosChars) <> 0) Then
    Begin
      Beep;
      Key := #0;
    End;
  End;
  Inherited KeyPress(Key);
End; {KeyPress}


Procedure TDriveView.KeyUp(var Key: Word; Shift: TShiftState);
Var P : TPoint;

Begin
  Inherited KeyUp(Key, Shift);
  IF (Key = VK_APPS) And Assigned(Selected) Then
  Begin
    P := ClientToScreen(Selected.DisplayRect(True).TopLeft);
    INC(P.Y, 20);
    DisplayContextMenu(Selected, P);
  End;
End; {KeyUp}


(* Overwrite event OnChange: *)

Procedure TDriveView.Change(Node: TTreeNode);
Var Drive     : TDrive;
    OldSerial : DWORD;
    NewDir    : String;
    LastDrive : TDrive;

Begin
  IF Assigned(Node) Then
  Begin
    NewDir := GetDirPath(Node);
    IF NewDir <> FLastDir Then
    Begin
      Drive := NewDir[1];
      IF Length(FLastDir) > 0 Then
      LastDrive := FLastDir[1]
      Else
      LastDrive := #0;

      fChangeFlag := True;
      fLastDir := NewDir;

      OldSerial := DriveInfo[Drive].DriveSerial;
      DriveInfo.ReadDriveStatus(Drive, dsSize or dsImageIndex);
      With DriveInfo[Drive] Do
      Begin
        {MP}{IF Assigned(FDriveBox) Then
          TIEDriveComboBox(FDriveBox).Drive := NewDir[1];}
        IF Assigned(fDirView) And (fDirView.Path <> NewDir) Then
          fDirView.Path := NewDir;

        IF DriveReady Then
        Begin
          IF Not DirExists(NewDir) Then
          Begin
            ValidateDirectory(DriveStatus[Upcase(NewDir[1])].RootNode);
            Exit;
          End;

          DriveStatus[Drive].DefaultDir := AddSlash(NewDir);

          IF LastDrive <> Drive Then
          Begin
            {IF LastDrive < FirstFixedDrive Then
              TerminateWatchThread(LastDrive);}


{$IFNDEF NO_THREADS}
            IF (LastDrive >= FirstDrive) And (DriveInfo[LastDrive].DriveType = DRIVE_REMOVABLE) Then
              TerminateWatchThread(LastDrive);
{$ENDIF}

            {Drive serial has changed or is missing: allways reread the drive:}
            IF (DriveSerial <> OldSerial) Or (DriveSerial = 0) Then
            Begin
              IF TNodeData(DriveStatus[Drive].RootNode.Data).Scanned Then
              ScanDrive(Drive);
              IF Assigned(FOnInsertedDiskChange) Then
              FOnInsertedDiskChange(Self, Drive);
            End;

            If Assigned(fOnDiskChange) Then
            fOnDiskChange(Self, Drive);
          End;
{$IFNDEF NO_THREADS}
          StartWatchThread;
{$ENDIF}
        End
        Else  {Drive not ready:}
        Begin
          DriveStatus[Drive].RootNode.DeleteChildren;
          DriveStatus[Drive].DefaultDir := EmptyStr;
          If (LastDrive <> Drive) Then
          Begin
            IF Assigned(fOnInsertedDiskChange) Then
            FOnInsertedDiskChange(Self, Drive);
            If Assigned(fOnDiskChange) Then
            FOnDiskChange(Self, Drive);
          End;
        End;
      End;
    End;
  End;
  Inherited Change(Node);
End; {Change}


// ===========================================================
// Methods of object TDriveView:
// ===========================================================

constructor TDriveView.Create(AOwner: TComponent);
Var Drive : TDrive;
    WinVer : TOSVersionInfo;


Begin
  Inherited Create(AOwner);
  fCreating := TRUE;
  WinVer.dwOSVersionInfoSize := SizeOf(WinVer);
  GetVersionEx(WinVer);

  IF fChangeInterval = 0 Then
  fChangeInterval := 1000;

  For Drive := FirstDrive To LastDrive Do
  With DriveStatus[Drive] Do
  Begin
    Scanned      := False;
    Verified     := False;
    RootNode     := NIL;
    DiscMonitor  := NIL;
    DefaultDir   := EmptyStr;
    {ChangeTimer: }
    ChangeTimer := TTimer.Create(Self);
    ChangeTimer.Interval := 0;
    ChangeTimer.Enabled  := False;
    ChangeTimer.OnTimer  := ChangeTimerOnTimer;
    ChangeTimer.Tag      := Ord(Drive);
  End;

  FFileOperator := TFileOperator.Create(Self);
  FFileOperator.ProgressTitle := coFileOperatorTitle;
  FFileOperator.Flags := [foAllowUndo, foNoConfirmMkDir];

  FCompressedColor   := clBlue;
  FColorBold         := clBlue;
  FShowVolLabel      := True;
  FChangeFlag        := False;
  FContextMenu       := False;
  FLastDir           := EmptyStr;
  FValidateFlag      := False;
  FConfirmDelete     := True;
  FUseSystemContextMenu := True;
  FCanChange         := True;
  FContinue          := True;
  FShowAnimation     := False;
  FDirectory         := EmptyStr;
  FFileNameDisplay   := fndStored;
  FReadDrives        := True;
  FForceRename       := False;
  FLastRenameName    := '';
  FRenameNode        := NIL;

  {Drag&drop:}
  DragMode          := dmAutomatic;
  fConfirmOverwrite := True;
  FDragDrive        := #0;
  DragFileList      := TStringList.Create;
  FLastPathCut      := '';
  FTargetPopupMenu  := True;
  FUseDragImages    := (Win32PlatForm = VER_PLATFORM_WIN32_NT) Or (WinVer.dwMinorVersion > 0);
  FStartPos.X       := -1;
  FStartPos.Y       := -1;
  FDragPos          := FStartPos;
  FExeDrag          := False;
  FDDLinkOnExeDrag  := True;

  FDD := TDragDropFilesEx.Create(Self);
  With FDD Do
  Begin
    AcceptOwnDnd     := True;
    {MP}
    {$IFDEF OLD_DND}
    AutoDetectDnD    := False;
    {$ELSE}
    DragDetect.Automatic := False;
    {$ENDIF}
    {/MP}
    BringToFront     := True;
    CompleteFileList := True;
    NeedValid        := [nvFileName];
    RenderDataOn     := rdoEnterAndDropSync;
    TargetPopUpMenu  := FTargetPopupMenu;
    {OnDragDetect     := DDDragDetect;}
    OnDragEnter      := DDDragEnter;
    OnDragLeave      := DDDragLeave;
    OnDragOver       := DDDragOver;
    OnProcessDropped := DDProcessDropped;
    OnDrop           := DDDrop;
    OnQueryContinueDrag := DDQueryContinueDrag;
    OnGiveFeedback   := DDGiveFeedback;
    ShellExtensions.DragDropHandler := True;
    OnSpecifyDropTarget := DDSpecifyDropTarget;
  End;
  OnCustomDrawItem      := InternalOnDrawItem;
End; {Create}


Procedure TDriveView.CreateWnd;
Var FileInfo        : TShFileInfo;
    PIDLWorkPlace   : PItemIDList;

Begin
  Inherited CreateWnd;
  IF Not Assigned(Images) Then
  Begin
    Images := TImageList.Create(Self);
    Images.Handle := SHGetFileInfo('', 0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
    Images.ShareImages := True;
  End;

  IF Not Assigned(StateImages) Then
  Begin
    StateImages := TImageList.Create(Self);
    StateImages.Handle := SHGetFileInfo('', 0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_OPENICON);
    StateImages.ShareImages := True;
  End;

  IF Not (csDesigning in ComponentState) Then
    FDragImageList := TDragImageList.Create(Self);
  IF Not Assigned(GlobalDragImageList) Then
  GlobalDragImageList := FDragImageList;

  IF Assigned(PopupMenu) Then
  PopupMenu.Autopopup := False;
  FParentForm := GetParentForm(Self);

  OLECheck(shGetDesktopFolder(FDesktop));
  OLECheck(shGetSpecialFolderLocation(Self.Handle, CSIDL_DRIVES, PIDLWorkPlace));
  FDesktop.BindToObject(PIDLWorkPlace, NIL, IID_IShellFolder, Pointer(FWorkPlace));
  FreePIDL(PIDLWorkPlace);

  FDD.DragDropControl := Self;
  FDD.SourceEffects   := [deCopy, deMove, deLink];
  FDD.TargetEffects   := [deCopy, deMove, deLink];
End; {CreateWnd}


Destructor TDriveView.Destroy;
Var Drive : TDrive;
Begin

  IF Assigned(Images) Then
  Images.Free;

  IF Assigned(StateImages) Then
  StateImages.Free;

  IF Assigned(FDragImageList) Then
  Begin
    IF GlobalDragImageList = FDragImageList Then
    GlobalDragImageList := NIL;
    FDragImageList.Free;
  End;

  For Drive := FirstDrive To LastDrive Do
  With DriveStatus[Drive] Do
  Begin
    IF Assigned(DiscMonitor) Then
       Discmonitor.Free;
    IF Assigned(ChangeTimer) Then
       ChangeTimer.Free;
  End;

  IF Assigned(FFileOperator) Then
     FFileOperator.Free;

  DragFileList.Destroy;

  IF Assigned(FDD) Then
  FDD.Free;
  
  Inherited Destroy;
End; {Destroy}


Function TDriveView.GetNodeFromHItem(Item: TTVItem): TTreeNode;
begin
  with Item do
    if (state and TVIF_PARAM) <> 0 then
      Result := Pointer(lParam)
    else
      Result := Items.GetNode(hItem);
end; {GetNodeFromItem}


Procedure TDriveView.CNNotify(Var Msg: TWMNotify);
Begin
  Case Msg.NMHdr.code Of
    TVN_BEGINDRAG:  DDDragDetect(MK_LBUTTON, FStartPos, Mouse.CursorPos, ddsDrag);
    TVN_BEGINRDRAG: DDDragDetect(MK_RBUTTON, FStartPos, Mouse.CursorPos, ddsDrag);
  Else
    Inherited;
  End;
End; {CNNotify}


Procedure TDriveView.WMLButtonDown(var Msg: TWMLButtonDown);
Begin
  FCanChange := False;
  GetCursorPos(FStartPos);
  Inherited;
End; {WMLButtonDown}


Procedure TDriveView.WMLButtonUp(var Msg: TWMLButtonDown);
Begin
  FCanChange := True;
  IF Assigned(DropTarget) And Assigned(DropTarget.Data) Then
    Selected   := DropTarget;
  DropTarget := NIL;
  Inherited;
End; {WMLButtonUp}


Procedure TDriveView.WMRButtonDown(var Msg: TWMRButtonDown);
Begin
  GetCursorPos(FStartPos);
  IF FDD.DragDetectStatus <> ddsDrag Then
  fContextMenu := True;
  Inherited;
End; {WMRButtonDown}


Procedure TDriveView.WMContextMenu(Var Msg: TWMContextMenu);
Var Node       : TTreeNode;
    DirWatched : Boolean;
    P          : TPoint;

Begin
  IF Assigned(PopupMenu) Then
  PopupMenu.Autopopup := False;
  Inherited;
  FStartPos.X := -1;
  FStartPos.Y := -1;
  Try
    IF fContextMenu Then
    Begin
      P.X := Msg.XPos;
      P.Y := Msg.YPos;
      P := ScreenToClient(P);
      Node := GetNodeAt(P.X, P.Y);
      IF FUseSystemContextMenu And Assigned(Node) Then
      Begin
        IF Assigned(OnMouseDown) Then
          OnMouseDown(Self, mbRight, [], Msg.XPos, Msg.YPos);
{$IFNDEF NO_THREADS}
        DirWatched := NodeWatched(Node) And WatchThreadActive;
#else
        DirWatched := False;
{$ENDIF}
        DisplayContextMenu(Node);
        IF Not DirWatched Then
        ValidateDirectory(Node);
      End
      Else
      Begin
        {P.X := Msg.XPos;
        P.Y := Msg.YPos;
        P := ClientToScreen(P);}
        IF Assigned(PopupMenu) And Not PopupMenu.AutoPopup Then
          PopupMenu.Popup(Msg.XPos, Msg.YPos);
      End;
    End;
    fContextMenu := False;
  Finally
    DropTarget := NIL;
  End;
End; {WMContextMenu}



Procedure TDriveView.SetImageIndex(Node : TTreeNode);
Var FileInfo  : TShFileInfo;
    NodePath  : String;

Begin
  IF Assigned(Node) And TNodeData(Node.Data).IconEmpty Then
  Begin
    NodePath := GetDirPathName(Node);
    IF Node.Level = 0 Then
    Begin
      With DriveInfo[NodePath[1]] Do
      Begin
        IF ImageIndex = 0 Then
        Begin
          DriveInfo.ReadDriveStatus(NodePath[1], dsImageIndex);
          Node.ImageIndex    := DriveInfo[NodePath[1]].ImageIndex;
        End
        Else
        Node.ImageIndex    := ImageIndex;
        Node.SelectedIndex := Node.ImageIndex;
      End;

    End
    Else
    Begin
      IF (DriveInfo[NodePath[1]].DriveType = DRIVE_REMOTE) Then
      Begin
        Node.ImageIndex    := StdDirIcon;
        Node.SelectedIndex := StdDirSelIcon;
      End
      Else
      Begin
        Try
          SHGetFileInfo(PChar(NodePath), 0, FileInfo, SizeOf(FileInfo),
                         SHGFI_SYSICONINDEX Or SHGFI_SMALLICON);

          IF (FileInfo.iIcon < Images.Count) And (FileInfo.iIcon > 0) Then
          Begin
            Node.ImageIndex    := FileInfo.iIcon;
            SHGetFileInfo(PChar(NodePath), 0, FileInfo, SizeOf(FileInfo),
                           SHGFI_SYSICONINDEX Or SHGFI_SMALLICON Or SHGFI_OPENICON);
            Node.SelectedIndex    := FileInfo.iIcon;
          End
          Else
          Begin
            Node.ImageIndex    := StdDirIcon;
            Node.SelectedIndex := StdDirSelIcon;
          End;
        Except
          Begin
            Node.ImageIndex    := StdDirIcon;
            Node.SelectedIndex := StdDirSelIcon;
          End;
        End;
      End;
    End;
  End; {IconEmpty}
  TNodeData(Node.Data).IconEmpty := False;
End; {SetImageIndex}


Function TDriveView.GetDriveText(Drive : TDrive) : String;
Begin
  With DriveInfo[Drive] Do
  Begin
    IF fShowVolLabel And (Length(PrettyName) >  0) Then
    Begin
      Case fVolDisplayStyle Of
      doPrettyName:     Result := Prettyname;
      doDisplayName:    Result := DisplayName;
      doLongPrettyName: Result := LongPrettyname;
      End; {Case}
    End Else
      Result := Drive + ':';
  End;
End; {GetDriveText}


Function TDriveView.GetValidDrivesStr : String;
Var Drive : TDrive;

Begin
  Result := '';
  For Drive := FirstDrive to LastDrive Do
  IF DriveInfo[Drive].Valid Then
  Result := Result + Drive;
End; {GetValidDriveStr}


Procedure TDriveView.GetNodeShellAttr(ParentFolder : iShellFolder; NodeData : TNodeData; Path : String; ContentMask : Boolean = True);
Begin
  IF Not Assigned(ParentFolder) Or Not Assigned(NodeData) Then
    Exit;

  IF Not Assigned(NodeData.PIDL) Then
  NodeData.PIDL := PIDL_GetFromParentFolder(ParentFolder, PChar(Path));
  IF Assigned(NodeData.PIDL) Then
  Begin
    {NodeData.shAttr := SFGAO_CAPABILITYMASK or SFGAO_DISPLAYATTRMASK and
    (not SFGAO_READONLY) or SFGAO_REMOVABLE or $F0000000 (* SFGAO_CONTENTSMASK *);}
    IF ContentMask Then
      NodeData.shAttr := SFGAO_DISPLAYATTRMASK Or SFGAO_CONTENTSMASK
    Else
      NodeData.shAttr := SFGAO_DISPLAYATTRMASK;

    Try
      IF Not Succeeded(ParentFolder.GetAttributesOf(1, NodeData.PIDL, NodeData.shAttr)) Then
        NodeData.shAttr := 0;
    Except
    End;

    IF Not Assigned(NodeData.ShellFolder) Then
      ParentFolder.BindToObject(NodeData.PIDL, NIL, IID_IShellFolder, Pointer(NodeData.ShellFolder));
  End;
End; {GetNodeAttr}



Procedure TDriveView.RefreshRootNodes(ScanDirectory : Boolean; dsFlags : Integer);
Var Drive       : Char;
    NewText     : String;
    NextDrive   : TDrive;
    D           : TDrive;
    SaveCursor  : TCursor;
    WasValid    : Boolean;
    OldSerial   : DWORD;
    wFirstDrive : TDrive;
    NodeData    : TNodeData;

Begin
  {Fetch disabled drives from the registry:}

  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Try
    IF dsFlags And dvdsFloppy <> 0 Then
    wFirstDrive := FirstDrive
    Else
    wFirstDrive := FirstFixedDrive;
    For Drive := wFirstDrive to LastDrive Do
    Begin
      With DriveInfo[Drive] Do
      Begin
        WasValid  := {Valid And } Assigned(DriveStatus[Drive].RootNode);
        OldSerial := DriveSerial;
      End;
      IF (dsFlags And dvdsReReadAllways = 0) And (Length(DriveInfo[Drive].DisplayName) > 0) Then
      dsFlags := dsFlags And Not dsDisplayName;

      IF FReadDrives Then
      DriveInfo.ReadDriveStatus(Drive, dsFlags);

      With DriveInfo[Drive], DriveStatus[Drive] Do
      Begin
        IF Valid Then
        Begin
          IF Not WasValid Then
          {New drive has arrived: insert new rootnode:}
          Begin
            NextDrive := LastDrive;
            IF Not fCreating Then
            For D := Drive To LastDrive Do
            Begin
              IF Assigned(DriveStatus[D].RootNode) Then
              Begin
                NextDrive := D;
                Break;
              End;
            End;

            { Create root directory node }
            NodeData := TNodeData.Create;
            NodeData.DirName   := Drive + ':\';
            NodeData.ShortName := Drive + ':\';

            {Get the shared attributes:}
            IF Drive >= FirstFixedDrive Then
            GetNodeShellAttr(FWorkPlace, NodeData, NodeData.DirName);

            IF Assigned(DriveStatus[NextDrive].RootNode) Then
            RootNode := Items.InsertObject(DriveStatus[NextDrive].RootNode, '', NodeData)
            Else
            RootNode := Items.AddObject(nil, '', NodeData);

            If Bool(NodeData.shAttr And SFGAO_SHARE) Then
            RootNode.OverlayIndex := 0;

            RootNode.Text        := GetDisplayName(RootNode);
            RootNode.HasChildren := TRUE;

            Scanned              := False;
            Verified             := False;

          End
          Else
          If RootNode.ImageIndex <> DriveInfo[Drive].ImageIndex Then
          Begin {WasValid = True}
            RootNode.ImageIndex    := DriveInfo[Drive].ImageIndex;
            RootNode.SelectedIndex := DriveInfo[Drive].ImageIndex;
          End;


          IF (Drive >= FirstFixedDrive) And Scanned Then
          Begin
            IF ScanDirectory And (DriveSerial <> OldSerial) Then
            ScanDrive(Drive);
          End;

          IF Assigned(RootNode) Then
          Begin
            NewText := GetDisplayName(RootNode);
            IF RootNode.Text <> NewText Then
            RootNode.Text := NewText;
          End;
        End
        Else
        IF WasValid Then
        {Drive has been removed => delete rootnode:}
        Begin
          IF Directory[1] = Drive Then
          Begin
            Directory := GetDirPathName(DriveStatus[Drive].RootNode.GetPrevSibling);
            IF Not Assigned(Selected) Then
            Directory := GetDirPathName(DriveStatus[FirstFixedDrive].RootNode);
          End;
          Scanned     := False;
          Verified    := False;
          RootNode.Delete;
          RootNode := NIL;
        End;
      End;
    End;
  Finally
    Screen.Cursor := SaveCursor;
  End;
End; {RefreshRootNodes}


Function TDriveView.AddChildNode(ParentNode : TTreeNode; Srec : TSearchRec) : TTreeNode;
Var NewNode  : TTreeNode;
    NodeData : TNodeData;

Begin
  NodeData := TNodeData.Create;
  NodeData.Attr          := Srec.Attr;
  NodeData.DirName       := Srec.Name;
  NodeData.ShortName     := Srec.FindData.cAlternateFileName;
  NodeData.fisRecycleBin := (Srec.Attr And faSysFile <> 0) And
                                         (ParentNode.Level = 0) And
                                         (UpperCase(Srec.Name) = 'RECYCLED');

  IF Not Assigned(TNodeData(ParentNode.Data).ShellFolder) Then
  GetNodeShellAttr(FWorkPlace, TNodeData(ParentNode.Data), GetDirPathName(ParentNode));

  GetNodeShellAttr(TNodeData(ParentNode.Data).ShellFolder, NodeData, SRec.Name );

  NewNode := Self.Items.AddChildObject(ParentNode, '', NodeData);
  NewNode.Text := GetDisplayName(NewNode);

  If Bool(NodeData.shAttr And SFGAO_SHARE) Then
  NewNode.OverlayIndex := 0;

  Result := NewNode;
End; {AddChildNode}



Function  TDriveView.GetDriveStatus(Drive : TDrive) : TDriveStatus;
Begin
  Result := DriveStatus[Upcase(Drive)];
End; {GetDriveStatus}



Function TDriveView.DoScanDir(FromNode : TTreeNode) : Boolean;
Var ScanThisDir : Boolean;
Begin
  With TNodeData(FromNode.Data) Do
  ScanThisDir := Not isRecycleBin And (DirName <> 'RECYCLER');
  IF Assigned(fOnScanDir) Then
  fOnScanDir(Self, FromNode, ScanThisDir);
  Result := ScanThisDir;
End; {DoScanDir}


Procedure TDriveView.ScanDrive(Drive : TDrive);
Var DosError   : Integer;
    RootNode   : TTreeNode;
    SaveCursor : TCursor;
    FAnimate   : TAnimate;

Procedure ScanPath(Const Path : String; ParentNode : TTreeNode);
Var Srec      : TSearchRec;
    SubNode   : TTreeNode;

Begin
  IF Not DoScanDir(ParentNode) Then
  Exit;

  DosError := FindFirst(Path, DirAttrMask, Srec);
  While DosError = 0 Do
  Begin
     IF (Srec.Name <> '.') And
        (Srec.Name <> '..') And
        (Srec.Attr And faDirectory <> 0) Then
     Begin
       IF (Srec.Attr And faDirectory) <> 0 Then
       Begin { Scan subdirectory }
         SubNode := AddChildNode(ParentNode , Srec);
         TNodeData(SubNode.Data).Scanned := True;
         ScanPath(ExtractFilePath(Path) + Srec.Name + '\*.*', SubNode );
         IF Not FContinue Then
         Break;
       End;
     End;
     DosError := FindNext(Srec);
  End;
  FindClose(Srec);
  IF (Items.Count Mod 10) = 0 Then
  Application.ProcessMessages;
  IF Not FContinue Then
  Exit;
End; {ScanPath}


Begin {ScanDrive}
  with Self.Items do
  begin
    FContinue := True;
    IF Not fFullDriveScan Then
    Begin
      ValidateDirectory(FindNodeToPath(Drive + ':\'));
      DriveStatus[Drive].Scanned  := TRUE;
      DriveStatus[Drive].Verified := FALSE;
    End
    Else
    Begin
      FAnimate      := NIL;
      SaveCursor    := Screen.Cursor;
      Screen.Cursor := crHourglass;
      Items.BeginUpdate;

      IF FShowAnimation Then
      Begin
        FAnimate := TAnimate.Create(Self);
        FAnimate.Top  := (Height - FAnimate.Height) DIV 2;
        FAnimate.Left := ((Width - FAnimate.Width) * 2) DIV 3;
        FAnimate.Parent := Self;
        FAnimate.CommonAVI := aviFindFolder;
        FAnimate.Active := True;
      End;

      If Assigned(fOnStartScan) Then
      fOnStartScan(Self);
      Try
        RootNode := DriveStatus[Drive].RootNode;
        IF Not Assigned(RootNode) Then Exit;

        IF RootNode.HasChildren Then
          RootNode.DeleteChildren;

        ScanPath(Drive + ':\*.*', RootNode);      { scan subdirectories of rootdir}
        TNodeData(RootNode.Data).Scanned := True;

        DriveStatus[Drive].Scanned  := TRUE;
        DriveStatus[Drive].Verified := TRUE;
      finally
        SortChildren(DriveStatus[Drive].RootNode, True);
        EndUpdate;
        IF Assigned(FAnimate) Then
        FAnimate.Free;
      End;
      RootNode.Expand(False);

      Screen.Cursor := SaveCursor;
      If Assigned(FOnEndScan) Then
      FOnEndScan(Self);
    End;
  End;
End; {ScanDrive}


Function TDriveView.HasSubNodes(Node : TTreeNode) : Boolean;
Var NewNode : TTreeNode;
Begin
  Result := Assigned(Node);
  IF Result Then
  Begin
    NewNode := Node.GetFirstChild;
    Result := Assigned(NewNode);
  End;
End; {HasSubNodes}


Function TDriveView.FindNodeToPath(Path : String) : TTreeNode;
Var Drive: Char;

Function SearchSubDirs(ParentNode : TTreeNode; Path : String) : TTreeNode;
Var i    : Integer;
    Node : TTreeNode;
    Dir  : String;

Begin
  Result := NIL;
  IF Length(Path) = 0 Then
  Exit;

  {Extract first directory from path:}
  i := Pos('\', Path);

  IF (i = 0) Then
  i := Length(Path);

  Dir := System.Copy(Path, 1, i);
  System.Delete(Path, 1, i);

  IF Dir[Length(Dir)] = '\' Then
  SetLength(Dir, Pred(Length(Dir)));

  IF Not TNodeData(ParentNode.Data).Scanned Then
  ReadSubDirs(ParentNode, GetDriveTypeToNode(ParentNode));

  Result := NIL;
  Node := ParentNode.GetFirstChild;
  IF Not Assigned(Node) Then
  Begin
    ValidateDirectoryEx(ParentNode, rsRecursiveExisting, True);
    Node := ParentNode.GetFirstChild;
  End;

  While Assigned(Node) Do
  Begin
    IF (UpperCase(GetDirName(Node)) = Dir) OR (TNodeData(Node.Data).ShortName = Dir) Then
    Begin
      IF Length(Path) > 0 Then
      Result := SearchSubDirs(Node, Path)
      Else
      Result := Node;
      Exit;
    End;
    Node := ParentNode.GetNextChild(Node);
  End;

End; {SearchSubDirs}


Begin {FindNodeToPath}
  Result := NIL;
  IF Length(Path) < 3 Then
  Exit;
  Drive := UpCase(Path[1]);
  IF (Drive < FirstDrive) Or (Drive > LastDrive) Then
  EConvertError.Create(Format(ErrorInvalidDrive, [Drive]))
  Else
    IF Assigned(DriveStatus[Drive].RootNode) Then
    Begin
      System.Delete(Path, 1, 3);
      IF Length(Path) > 0 Then
      Begin
        IF Not DriveStatus[Drive].Scanned Then
        ScanDrive(Drive);
        Result := SearchSubDirs(DriveStatus[Drive].RootNode, UpperCase(Path));
      End
      Else
        Result := DriveStatus[Drive].RootNode;
    End;
End; {FindNodetoPath}


Function TDriveView.IterateSubTree(Var StartNode : TTreeNode;
                                   CallBackFunc  : TCallBackFunc;
                                   Recurse       : TRecursiveScan;
                                   ScanStartNode : TScanStartNode;
                                   Data          : Pointer) : Boolean;

(* Scans StartNode and level-1 Subdirectories plus open subdirectories*)

Function ScanSubDirs(Var StartNode : TTreeNode) : Boolean;
(* Scans all subdirectories of Startnode *)
Var Node          : TTreeNode;
    NextNode      : TTreeNode;
    NodeHasChilds : Boolean;

Begin
  Result := False;
  IF Not Assigned(StartNode) Then Exit;
  Node := StartNode.GetFirstChild;

  While Assigned(Node) And FContinue Do
  Begin
    NextNode := StartNode.GetNextChild(Node);
    NodeHasChilds := HasSubNodes(Node);

    IF Not FContinue Or Not CallBackFunc(Node, Data) Then
    Exit;

    IF Assigned(Node) And
      ((Recurse = rsRecursive) Or
        ((Recurse = rsRecursiveExpanded) And Node.Expanded) Or
        ((Recurse = rsRecursiveExisting) And NodeHasChilds)) Then
    IF Not ScanSubDirs(Node) Or Not FContinue Then
    Exit;

    Node := NextNode;
  End;
  Result := True;
End; {ScanSubDirs}


Begin {IterateSubTree}
  Result := False;
  FContinue := True;
  IF Not Assigned(CallBackFunc) Then
  Exit;

  IF ScanStartNode = coScanStartNode Then
  CallBackFunc(StartNode, Data);

  IF Assigned(StartNode) Then
  IF Not FContinue Or Not ScanSubDirs(StartNode) Then
  Exit;

  Result := True;
End; {IterateSubTree}


Function TDriveView.CheckForSubDirs(Path: String) : Boolean;
Var DosError : Integer;
    SRec     : TSearchRec;


Begin
  Result := False;

  DosError := FindFirst(AddSlash(Path) + '*.', DirAttrMask, SRec);
  While DosError = 0 Do
  Begin
    IF (Srec.Name <> '.' ) And
       (Srec.Name <> '..') And
       (Srec.Attr And faDirectory <> 0) Then
    Begin
      Result := True;
      Break;
    End;
    DosError := FindNext(Srec);
  End;
  FindClose(Srec);
End; {CheckForSubDirs}


Function TDriveView.ReadSubDirs(Node : TTreeNode; DriveType: Integer) : Boolean;
Var DosError : Integer;
    SRec     : TSearchRec;
    NewNode  : TTreeNode;


Begin
  Result := False;
  DosError := FindFirst(AddSlash(GetDirPath(Node)) + '*.*', DirAttrMask, SRec);
  While DosError = 0 Do
  Begin
    IF (Srec.Name <> '.' ) And
       (Srec.Name <> '..') And
       (Srec.Attr And faDirectory <> 0) Then
    Begin
      NewNode := AddChildNode(Node, SRec);
      IF DoScanDir(NewNode) Then
      Begin
        NewNode.HasChildren := Bool(TNodeData(NewNode.Data).shAttr And SFGAO_HASSUBFOLDER);

        {IF (DriveType = DRIVE_REMOTE) Then
          NewNode.HasChildren := CheckForSubDirs(GetDirPath(NewNode))
        Else
          NewNode.HasChildren := Bool(TNodeData(NewNode.Data).shAttr And SFGAO_HASSUBFOLDER);}

        TNodeData(NewNode.Data).Scanned := Not NewNode.HasChildren;
      End
      Else
      Begin
        NewNode.HasChildren := False;
        TNodeData(NewNode.Data).Scanned := True;
      End;
      Result := True;
    End;
    DosError := FindNext(Srec);
  End; {While DosError = 0}
  FindClose(Srec);
  TNodeData(Node.Data).Scanned := True;

  IF Result Then
  {Sort subnodes:}
    SortChildren(Node, False)
  Else
  Node.HasChildren := False;
  Application.ProcessMessages;
End; {ReadSubDirs}


Function TDriveView.CallBackValidateDir(Var Node : TTreeNode; Data: Pointer) : Boolean;

Type PSearchRec = ^TSearchRec;

Var WorkNode    : TTreeNode;
    DelNode     : TTreeNode;
    NewNode     : TTreeNode;
    SRec        : TSearchRec;
    SrecList    : TStringList;
    SubDirList  : TStringList;
    DosError    : Integer;
    Index       : Integer;
    NewDirFound : Boolean;
    ParentDir   : String;


Begin {CallBackValidateDir}
  Result := True;
  IF Not Assigned(Node) Or Not Assigned(Node.Data) Then
  Exit;

  NewDirFound := False;

  {Check, if directory still exists: (but not with root directory) }
  IF Assigned(Node.Parent) And (PScanDirInfo(Data)^.StartNode = Node) Then
  IF Not DirExists(GetDirPathName(Node)) Then
  Begin
    WorkNode := Node.Parent;
    IF Selected = Node Then
    Selected := WorkNode;
    IF DropTarget = Node Then
    DropTarget := NIL;
    Node.Delete;
    Node := NIL;
    Exit;
  End;

  WorkNode := Node.GetFirstChild;

  IF TNodeData(Node.Data).Scanned And Assigned(WorkNode) Then
  {if node was already scanned: check wether the existing subnodes are still alive
   and add all new subdirectories as subnodes:}
  Begin
    IF DoScanDir(Node) Then
    Begin
      ParentDir := AddSlash(GetDirPath(Node));
      {Build list of existing subnodes:}
      SubDirList := TStringList.Create;
      While Assigned(Worknode) Do
      Begin
        SubDirList.Add(TNodeData(WorkNode.Data).DirName);
        WorkNode := Node.GetNextChild(WorkNode);
      End;
      {Sorting not required, because the subnodes are already sorted!}
      {SubDirList.Sort;}

      SRecList := TStringList.Create;
      DosError := FindFirst(ParentDir + '*.*', DirAttrMask, SRec);
      While DosError = 0 Do
      Begin
        IF (Srec.Name <> '.' ) And
           (Srec.Name <> '..') And
           (Srec.Attr And faDirectory <> 0) Then
        Begin
          SrecList.Add(Srec.Name);
          IF Not SubDirList.Find(Srec.Name, Index) Then
          {Subnode does not exists: add it:}
          Begin
            NewNode := AddChildNode(Node, SRec);
            NewNode.HasChildren := CheckForSubDirs(ParentDir + Srec.Name);
            TNodeData(NewNode.Data).Scanned := Not NewNode.HasChildren;
            NewDirFound  := True;
          End;
        End;
        DosError := FindNext(Srec);
      End;
      FindClose(Srec);
      Sreclist.Sort;

      {Remove not existing subnodes:}
      WorkNode := Node.GetFirstChild;
      While Assigned(WorkNode) Do
      Begin
        IF Not Assigned(WorkNode.Data) Or
           NOT SrecList.Find(TNodeData(WorkNode.Data).DirName, Index) Then
        Begin
          DelNode := WorkNode;
          WorkNode := Node.GetNextChild(WorkNode);
          DelNode.Delete;
        End
        Else
        Begin
          IF (SrecList[Index] <> TNodeData(WorkNode.Data).DirName) Then
          Begin
            {Case of directory letters has changed:}
            TNodeData(WorkNode.Data).DirName := SrecList[Index];
            TNodeData(WorkNode.Data).ShortName := ExtractShortPathName(GetDirPathName(WorkNode));
            WorkNode.Text := SrecList[Index];
          End;
          SrecList.Delete(Index);
          WorkNode := Node.GetNextChild(WorkNode);
        End;
      End;
      SrecList.Free;
      SubDirList.Free;
      {Sort subnodes:}
      IF NewDirFound Then
        SortChildren(Node, False);
    End;
  End
  Else
  {Node was not already scanned:}
  IF (PScanDirInfo(Data)^.SearchNewDirs     Or
     TNodeData(Node.Data).Scanned           Or
    (Node = PScanDirInfo(Data)^.StartNode)) And
    DoScanDir(Node) Then
    ReadSubDirs(Node, PScanDirInfo(Data)^.DriveType);

  {Application.ProcessMessages; <== causes the treeview flickering!}
End; {CallBackValidateDir}



Procedure TDriveView.ValidateDirectoryEx(Node : TTreeNode; Recurse : TRecursiveScan; NewDirs : Boolean);
Var Info     : PScanDirInfo;
    SelDir   : String;
    SaveCursor : TCursor;
{$IFNDEF NO_THREADS}
    RestartWatchThread : Boolean;
{$ENDIF}
    SaveCanChange      : Boolean;
    CurrentPath        : String;

Begin
  IF Not Assigned(Node)      Or
     Not Assigned(Node.Data) Or
     fValidateFlag           Or
     Not DoScanDir(Node)     Then
  Exit;

  SelDir        := Directory;
  SaveCursor    := Screen.Cursor;
  IF Self.Focused And (Screen.Cursor <> crHourGlass) Then
  Screen.Cursor := crHourGlass;

  CurrentPath := GetDirPath(Node);

  IF Node.Level = 0 Then
  DriveStatus[CurrentPath[1]].ChangeTimer.Enabled := False;

{$IFNDEF NO_THREADS}
  RestartWatchThread := WatchThreadActive;
{$ENDIF}
  Try
{$IFNDEF NO_THREADS}
    IF WatchThreadActive Then
    StopWatchThread;
{$ENDIF}

    fValidateFlag := True;

    New(Info);
    Info^.StartNode := Node;
    Info^.SearchNewDirs := NewDirs;
    Info^.DriveType := DriveInfo[CurrentPath[1]].DriveType;

    SaveCanChange := FCanChange;
    FCanChange := True;
    FChangeFlag := False;
    IterateSubTree(Node, CallBackValidateDir, Recurse, coScanStartNode, Info);
    fValidateFlag := False;
    IF Not Assigned(Selected) And (Length(SelDir) > 0) Then
    Directory := Copy(SelDir, 1, 3);
    IF (SelDir <> Directory) And Not FChangeFlag Then
    Change(Selected);
    FCanChange := SaveCanChange;

    Dispose(Info);
  Finally
{$IFNDEF NO_THREADS}
    IF RestartWatchThread And fWatchDirectory And Not WatchThreadActive Then
    StartWatchThread;
{$ENDIF}

    IF Screen.Cursor <> SaveCursor Then
    Screen.Cursor := SaveCursor;
  End;
End; {ValidateDirectoryEx}


Procedure TDriveView.ValidateDirectoryEasy(Node : TTreeNode);
Begin
  IF Not Assigned(Node) Then
  Exit;
  IF Not Assigned(Node.Data) or Not TNodeData(Node.Data).Scanned Then
  ValidateDirectoryEx(Node, rsRecursiveExpanded, False);
End; {ValidateDirectoryEasy}


Procedure TDriveView.ValidateDirectory(Node : TTreeNode);
Begin
  ValidateDirectoryEx(Node, rsRecursiveExisting, False);
End;  {ValidateDirectory}


Procedure TDriveView.ValidateVisibleDirectories(Node : TTreeNode);
Begin
  ValidateDirectoryEx(Node, rsRecursiveExpanded, False);
End; {ValidateVisibleDirectories}


Procedure TDriveView.ValidateAllDirectories(Node : TTreeNode);
Begin
  ValidateDirectoryEx(Node, rsRecursive, True);
End; {ValidateAllDirectories}


Function  TDriveView.GetSubTreeSize(Node : TTreeNode) : Integer;
Var PSubSize : PInt;
    SaveCursor : TCursor;
Begin
  IF Not Assigned(Node) Then
  Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['GetSubTreeSize']));
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
End; {GetSubTreeSize}


Function TDriveView.GetDriveTypeToNode(Node : TTreeNode) : Integer;
Begin
  IF Not Assigned(Node) Then
    Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['GetDriveTypeToNode']));
  Result := DriveInfo[GetDirPath(Node)[1]].DriveType
End; {GetDriveTypeToNode}


Function TDriveView.GetDriveType(Drive : TDrive) : Integer;           {Returns DRIVE_CDROM etc..}
Begin
  Result := DriveInfo[UpCase(Drive)].DriveType;
End; {GetDriveType}


Function TDriveView.NodeUpdateAble(Node : TTreeNode) : Boolean;
Begin
  Result := Assigned(Node)        And
            Assigned(Node.Data)   And
            (Node.Level > 0);
End; {NodeUpdateAble}


Function  TDriveView.CallBackSaveNodeState(Var Node : TTreeNode; Data: Pointer) : Boolean;
Begin
  Result := True;
  TNodeData(Node.Data).Expanded := Node.Expanded;
End; {CallBackSaveNodeState}


Function  TDriveView.CallBackRestoreNodeState(Var Node : TTreeNode; Data: Pointer) : Boolean;
Begin
  Result := True;
  Node.Expanded := TNodeData(Node.Data).Expanded;
End; {CallBackRestoreNodeState}


Procedure TDriveView.SaveNodesState(Node : TTreeNode);
Begin
  IterateSubTree(Node, CallbackSaveNodeState, rsRecursive, coScanStartNode, NIL);
End; {SaveNodesState}


Procedure TDriveView.RestoreNodesState(Node : TTreeNode);
Begin
  Items.BeginUpdate;
  IterateSubTree(Node, CallbackRestoreNodeState, rsRecursive, coScanStartNode, NIL);
  Items.EndUpdate;
End; {RestoreNodesState}



Function TDriveView.CreateDirectory(ParentNode : TTreeNode; NewName : String) : TTreeNode;
Var Srec    : TSearchRec;

Begin
    IF Not Assigned(ParentNode) Then
    Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['CreateDirectory']));

    Result := NIL;
    IF Not TNodeData(ParentNode.Data).Scanned Then
    ValidateDirectory(ParentNode);
{$IFNDEF NO_THREADS}
    StopWatchThread;
{$ENDIF}
    Try
{$IFNDEF NO_THREADS}
      IF Assigned(FDirView) Then
      FDirView.StopWatchThread;
{$ENDIF}

      {create phyical directory:}
      LastIOResult := 0;
      IF Not Windows.CreateDirectory(PChar(GetDirPath(ParentNode) + '\' + NewName), NIL) Then
      LastIOResult := GetLastError;
      IF LastIOResult = 0 Then
      Begin
        {Create treenode:}
        FindFirst(GetDirPath(ParentNode) + '\' + NewName, faAnyFile, SRec);
        Result := AddChildNode(ParentNode, Srec);
        FindClose(Srec);
        TNodeData(Result.Data).Scanned := True;
        SortChildren(ParentNode, False);
        ParentNode.Expand(False);
      End;
    Finally
{$IFNDEF NO_THREADS}
      StartWatchThread;
{$ENDIF}
      IF Assigned(FDirView) Then
      Begin
{$IFNDEF NO_THREADS}
        FDirView.StartWatchThread;
{$ENDIF}
        FDirView.Reload2;
      End;

    End;
End; {CreateDirectory}


Function TDriveView.DeleteDirectory(Node: TTreeNode; AllowUndo : Boolean) :Boolean;
Var DelDir         : String;
    OperatorResult : Boolean;
    FileOperator   : TFileOperator;
    SaveCursor     : TCursor;

Begin
  IF Not Assigned(Node) Then
    Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['DeleteDirectory']));

  Result := False;
  IF Assigned(Node) And (Node.Level > 0) Then
  Begin
    SaveCursor    := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    FileOperator  := TFileOperator.Create(Self);
    DelDir := GetDirPathName(Node);
    FileOperator.OperandFrom.Add(DelDir);
    FileOperator.Operation := foDelete;
    IF AllowUndo Then
    FileOperator.Flags := FileOperator.Flags + [foAllowUndo]
    Else
    FileOperator.Flags := FileOperator.Flags - [foAllowUndo];
    IF Not ConfirmDelete Then
    FileOperator.Flags := FileOperator.Flags + [foNoConfirmation];


    Try
      IF DirExists(DelDir) Then
      Begin
{$IFNDEF NO_THREADS}
        StopWatchThread;
{$ENDIF}
        OperatorResult := FileOperator.Execute;

        IF OperatorResult And Not FileOperator.OperationAborted And Not DirExists(DelDir) Then
          Node.Delete
        Else
        Begin
          Result := False;
          IF (Win32PlatForm = VER_PLATFORM_WIN32_NT) And Not AllowUndo Then
          Begin
            {WinNT4-Bug: FindFirst still returns the directories search record, even if the
             directory was deleted:}
            ChDir(DelDir);
            IF IOResult <> 0 Then
              Node.Delete;
          End;
        End;
      End
      Else
      Begin
        Node.Delete;
        Result := True;
      End;
    Finally
{$IFNDEF NO_THREADS}
      StartWatchThread;
      createthread
{$ENDIF}
      IF Assigned(DirView) And Assigned(Selected) Then
      DirView.Path := GetDirPathName(Selected);
      FileOperator.Free;
      Screen.Cursor := SaveCursor;
    End;
  End;
End; {DeleteDirectory}


{$IFNDEF NO_THREADS}
Procedure TDriveView.CreateWatchThread(Drive : TDrive);
Begin
  IF (csDesigning in ComponentState) Then
  Exit;
  IF Not Assigned(DriveStatus[Drive].DiscMonitor) And
     FWatchDirectory And
     (DriveInfo[Drive].DriveType <> DRIVE_REMOTE) And
     (Pos(Drive, FNoCheckDrives) = 0) Then
  With DriveStatus[Drive] Do
  Begin
    DiscMonitor := TDiscMonitor.Create(Self);
    DiscMonitor.ChangeDelay := msThreadChangeDelay;
    DiscMonitor.SubTree     := True;
    DiscMonitor.Filters     := [moDirName];
    DiscMonitor.OnChange    := ChangeDetected;
    DiscMonitor.OnInvalid   := ChangeInvalid;
    DiscMonitor.Directory   := Drive + ':\';
    DiscMonitor.Open;
  End;
End; {CreateWatchThread}
{$ENDIF}

Procedure TDriveView.SetWatchDirectory(Watch : Boolean);
Begin
  IF fWatchDirectory <> Watch Then
  Begin
    fWatchDirectory := Watch;
{$IFNDEF NO_THREADS}
    IF Not (csDesigning in ComponentState) And Watch Then
      StartAllWatchThreads
      Else
      StopAllWatchThreads;
{$ENDIF}
  End;
End; {SetAutoScan}


Procedure TDriveView.SetDirView(DV : TDirView);
Begin
  IF Assigned(fDirView) Then
  fDirView.DriveView := NIL;

  fDirView := DV;

  IF Assigned(fDirView) Then
  fDirView.DriveView := Self;
End; {SetDirView}


Procedure TDriveView.SetChangeInterval(Interval : Cardinal);
Var Drive : TDrive;
Begin
  IF Interval > 0 Then
  Begin
    fChangeInterval := Interval;
    For Drive := FirstDrive To LastDrive Do
    With DriveStatus[Drive] Do
    IF Assigned(ChangeTimer) Then
    ChangeTimer.Interval := Interval;
  End;
End; {SetChangeInterval}


Procedure TDriveView.SetDimmHiddenDirs(DimmIt : Boolean);
Begin
  IF DimmIt <> fDimmHiddenDirs Then
  Begin
    fDimmHiddenDirs := DimmIt;
    Self.Invalidate;
  End;
End; {SetDimmHiddenDirs}


Procedure TDriveView.SetNoCheckDrives(Value : String);
Begin
  FNoCheckDrives := UpperCase(Value);
End; {SetNoCheckDrives}


Procedure TDriveView.DeleteSubNodes(Node : TTreeNode);
Begin
  IF Assigned(Node) Then
  Begin
    Node.DeleteChildren;
    IF Node.Level = 0 Then
      DriveStatus[GetDriveToNode(Node)].Scanned := False;
    Node.HasChildren := False;
  End;
End; {DeleteSubNodes}


Function TDriveView.NodeWatched(Node : TTreeNode) : Boolean;
Var Drive : TDrive;
Begin
  Drive := GetDriveToNode(Node);
  Result := Assigned(DriveStatus[Drive].DiscMonitor) And
            DriveStatus[Drive].DiscMonitor.Active;
End; {NodeWatched}


procedure TDriveView.ChangeInvalid(Sender: TObject);
Var Dir     : String;

Begin
  Dir := (Sender as TDiscMonitor).Directory;
  With DriveStatus[Dir[1]] Do
  Begin
    DiscMonitor.Close;
    IF Assigned(fOnChangeInvalid) Then
    fOnChangeInvalid(Self, Dir[1]);
  End;
End; {DirWatchChangeInvalid}


procedure TDriveView.ChangeDetected(Sender: TObject);
Var DirChanged : String;
Begin
  IF (Sender is TDiscMonitor) Then
  Begin
    DirChanged := (Sender as TDiscMonitor).Directory;
    IF Length(DirChanged) > 0 Then
    With DriveStatus[DirChanged[1]] Do
    Begin
      ChangeTimer.Interval := 0;
      ChangeTimer.Interval := fChangeInterval;
      ChangeTimer.Enabled  := True;
    End;
  End;
End; {DirWatchChangeDetected}


Procedure TDriveView.ChangeTimerOnTimer(Sender : TObject);
Var Node  : TTreeNode;
    Drive : TDrive;
Begin
  IF Sender is TTimer Then
  With TTimer(Sender) Do
  Begin
    Drive := Chr(Tag);
    Node := FindNodeToPath(Drive + ':\');
    Interval := 0;
    Enabled  := False;

    IF Assigned(Node) Then
    Begin
      {Check also collapsed (invisible) subdirectories:}
      ValidateDirectory(Node);
      IF Assigned(fOnChangeDetected) Then
        fOnChangeDetected(Self, Drive);
    End;
  End;
End; {ChangeTimerOnTimer}


{$IFNDEF NO_THREADS}
Procedure TDriveView.StartWatchThread;
Var NewWatchedDir  : String;
    Drive          : TDrive;

Begin
    IF (csDesigning in ComponentState) Or
       Not Assigned(Selected) Or
       Not fWatchDirectory Then
       Exit;

    NewWatchedDir  := GetDirPathName(RootNode(Selected));
    Drive          := Upcase(NewWatchedDir[1]);

    With DriveStatus[Drive] Do
    Begin
      IF Not Assigned(DiscMonitor) Then
      CreateWatchThread(Drive);
      IF Assigned(DiscMonitor) And Not DiscMonitor.Active Then
        DiscMonitor.Open;
    End;
End; {StartWatchThread}


Procedure TDriveView.StopWatchThread;
Begin
  IF Assigned(Selected) Then
  With DriveStatus[GetDriveToNode(Selected)] Do
  IF Assigned(DiscMonitor) Then
  DiscMonitor.Close;
End; {StopWatchThread}


Procedure TDriveView.TerminateWatchThread(Drive : TDrive);
Begin
  IF Drive >= FirstDrive Then
  With DriveStatus[Drive] Do
  IF Assigned(DiscMonitor) Then
  Begin
    DiscMonitor.Free;
    DiscMonitor := NIL;
  End;
End; {StopWatchThread}


Procedure TDriveView.StartAllWatchThreads;
Var Drive : TDrive;
Begin
    IF (csDesigning in ComponentState) Or
       Not FWatchDirectory Then
       Exit;

    For Drive := FirstFixedDrive To LastDrive Do
    With DriveStatus[Drive] Do
    IF Scanned Then
    Begin
      IF Not Assigned(DiscMonitor) Then
      CreateWatchThread(Drive);
      IF Assigned(DiscMonitor) And Not DiscMonitor.Active Then
        DiscMonitor.Open;
    End;

    IF Assigned(Selected) And (GetDriveToNode(Selected) < FirstFixedDrive) Then
    StartWatchThread;

End; {StartAllWatchThreads}


Procedure TDriveView.StopAllWatchThreads;
Var Drive : TDrive;
Begin
    For Drive := FirstDrive To LastDrive Do
    With DriveStatus[Drive] Do
    Begin
      IF Assigned(DiscMonitor) Then
        DiscMonitor.Close;
    End;
End; {StopAllWatchThreads}


Function TDriveView.WatchThreadActive(Drive : TDrive) : Boolean;
Begin
  Result := FWatchDirectory        And
            Assigned(DriveStatus[Drive].DiscMonitor) And
            DriveStatus[Drive].DiscMonitor.Active;
End; {WatchThreadActive}


Function TDriveView.WatchThreadActive : Boolean;
Var Drive : TDrive;
Begin
  IF Not Assigned(Selected) Then
  Begin
    Result := False;
    Exit;
  End;

  Drive := GetDriveToNode(Selected);

  Result := FWatchDirectory        And
            Assigned(DriveStatus[Drive].DiscMonitor) And
            DriveStatus[Drive].DiscMonitor.Active;
End; {WatchThreadActive}
{$ENDIF}

Procedure TDriveView.SetFullDriveScan(DoFullDriveScan : Boolean);
Begin
  IF fFullDriveScan <> DoFullDriveScan Then
  Begin
    fFullDriveScan := DoFullDriveScan;
    {IF FullDriveScan And Assigned(Selected) And Not (csDesigning in ComponentState) Then
    ValidateAllDirectories(RootNode(Selected));}
  End;
End; {SetAutoScan}


Function TDriveView.GetDirectory : String;
Begin
  IF Assigned(Selected) Then
  Result := GetDirPathName(Selected)
  Else
  Result := '';
End; {GetDirectory}


Procedure TDriveView.SetDirectory(Path : String);
Var NewSel : TTreeNode;
    Rect   : TRect;
Begin
  FDirectory := Path;
  {Find existing path or parent path of not existing path:}
  Repeat
    NewSel := FindNodeToPath(Path);
    IF Not Assigned(NewSel) Then
    Path := ExtractFilePath(RemoveSlash(Path));
  Until Assigned(NewSel) Or (Length(Path) < 3);

  IF Assigned(NewSel) Then
  Begin
    FCanChange  := True;
    NewSel.MakeVisible;
    Rect := NewSel.DisplayRect(False);

    Selected := NewSel;

    IF (Selected.Level = 0) Then
    Begin
      IF Not DriveStatus[GetDriveToNode(Selected)].Scanned Then
      ScanDrive(GetDriveToNode(Selected));
    End;
  End
  Else
  IF csDesigning in ComponentState Then
    Selected := NIL;
  {Application.ProcessMessages;}
End; {SetDirectory}


Procedure TDriveView.SetDrive(Drive : TDrive);

Begin
  IF GetDrive <> Drive Then
  With DriveStatus[Drive] Do
  IF Assigned(RootNode) Then
  Begin
    IF DefaultDir = EmptyStr Then
    DefaultDir := Drive + ':\';
    IF Not Scanned Then
    RootNode.Expand(False);
    TopItem := RootNode;
    Directory := AddSlash(DefaultDir);
  End;
End; {SetDrive}


Function  TDriveView.GetDrive : TDrive;
Begin
  IF Assigned(Selected) Then
  Result := GetDriveToNode(Selected)
  Else
  Result := #0;
End; {GetDrive}


{Centers the Node vertically in the treeview window:}
Procedure TDriveView.CenterNode(Node : TTreeNode);
Var NodePos    : TRect;
    ScrollInfo : TScrollInfo;

Begin
  IF Not Assigned(Node) Or (Items.Count = 0) Then        
  Exit;
  Node.MakeVisible;

  NodePos  := Node.DisplayRect(False);
  With ScrollInfo Do
  Begin
    cbSize := SizeOf(ScrollInfo);
    fMask := SIF_ALL;
    nMin := 0;
    nMax := 0;
    nPage := 0;
  End;
  GetScrollInfo(Handle, SB_VERT, ScrollInfo);

  IF ScrollInfo.nMin <> ScrollInfo.nMax Then
  Begin
    {Scroll tree up:}
    IF (NodePos.Top < Height Div 4) And (ScrollInfo.nPos > 0) Then
    Begin
      ScrollInfo.fMask := SIF_POS;
      While (ScrollInfo.nPos > 0) And (NodePos.Top < (Height Div 4)) Do
      Begin
        Perform(WM_VSCROLL, SB_LINEUP, 0);
        GetScrollInfo(Handle, SB_VERT, ScrollInfo);
        NodePos := Node.DisplayRect(False);
      End;
    End
    Else

    IF (NodePos.Top > ((Height * 3) Div 4)) Then
    Begin
      {Scroll tree down:}
      ScrollInfo.fMask := SIF_POS;
      While (ScrollInfo.nPos + ABS(ScrollInfo.nPage) < ScrollInfo.nMax) And
            (NodePos.Top > ((Height * 3) Div 4))                        And
            (ScrollInfo.nPage > 0) Do
      Begin
        Perform(WM_VSCROLL, SB_LINEDOWN, 0);
        GetScrollInfo(Handle, SB_VERT, ScrollInfo);
        NodePos := Node.DisplayRect(False);
      End;
    End;
    NodePos := Node.DisplayRect(True);
  End;
  IF (NodePos.Left < 50) Then
  Perform(WM_HSCROLL, SB_PAGELEFT, 0);
End; {CenterNode}


Function TDriveView.GetDirName(Node : TTreeNode) : String;
Begin
    IF Assigned(Node) And Assigned(Node.Data) Then
      Result := TNodeData(Node.Data).Dirname
    Else
      Result := '';
End; {GetDirName}


{GetDirPath: Allways returns the complete path to Node without the trailing backslash:
 C:, C:\WINDOWS, C:\WINDOWS\SYSTEM  }
Function TDriveView.GetDirPath (Node : TTreeNode) : String;
Var T    : TTreeNode;
    PStr : String;

Begin
  IF Not Assigned(Node) Then
  Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['GetDirPath']));

  PStr := GetDirName(Node);
  T    := Node.Parent;

  While (T <> NIL) AND (T.Level >= 0) Do
  Begin
    IF T.Level > 0 Then
    PStr := GetDirName(T) + '\' + PStr
    Else
    PStr := GetDirName(T) + PStr;
    T    := T.Parent;
  End;
  IF Length(PStr) = 3 Then
    Result := Copy(PStr,1, 2)
  Else
    Result := PStr;
End; {GetDirPath}



{GetDirPathName: Returns the complete path to Node with trailing backslash on rootnodes:
 C:\ ,C:\WINDOWS, C:\WINDOWS\SYSTEM  }
Function TDriveView.GetDirPathName(Node: TTreeNode) : String;
Begin
  Result := GetDirPath(Node);
  IF Length(Result) = 2 Then
  Result := Result + '\';
End; {GetDirPathName}


{GetDrive: returns the driveletter of the Node.}
Function TDriveView.GetDriveToNode(Node : TTreeNode) : Char;
Var Path : String;
Begin
  IF Not Assigned (Node) Or Not Assigned(Node.Data) Then
    Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['GetDrive']));
  Path := GetDirPath(Node);
  IF Length(Path) > 0 Then
  Result := Upcase(Path[1])
  Else
  Result := #0;
End; {GetDrive}


{RootNode: returns the rootnode to the Node:}
Function TDriveView.RootNode(Node : TTreeNode) : TTreeNode;
Begin
  Result := Node;
  IF Not Assigned(Node) Then
    Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['RootNode']));
  While Assigned(Result.Parent) Do
  Result := Result.Parent;
End; {RootNode}


{NodeAttr: Returns the directory attributes to the node:}
Function TDriveView.NodeAttr(Node : TTreeNode) : Integer;
Begin
  IF Not Assigned(Node) Then
    Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['NodeAttr']));
  Result := TNodeData(Node.Data).Attr;
End; {NodeAttr}


Function TDriveView.NodeVerified(Node : TTreeNode) : Boolean;
Begin
  IF Not Assigned(Node) Or Not Assigned(Node.Data) Then
    Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['NodeVerified']));
  Result := TNodeData(Node.Data).Scanned;
End; {NodeVerified}



Procedure TDriveView.SetBoldDraw(Node : TTreeNode; BoldDraw : Boolean);
Begin
  IF Not Assigned(Node) Or Not Assigned(Node.Data) Then
     Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['SetBoldDraw']));

  IF TNodeData(Node.Data).DrawBold <> BoldDraw Then
  Begin
    TNodeData(Node.Data).DrawBold := BoldDraw;
    Node.Text := Node.Text; {Force redraw}
  End;
End; {SetBoldDraw}


Function  TDriveView.CallBackExpandLevel(Var Node : TTreeNode; Data: Pointer) : Boolean;
Begin
  Result := True;
  IF Not Assigned(Node) Then
    Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['CallBackExpandLevel']));
  IF (Node.Level <= Integer(Data)) And Not Node.Expanded Then
  Node.Expand(False)
  Else IF (Node.Level > Integer(Data)) And Node.Expanded Then
  Node.Collapse(True);
End; {CallBackExpandLevel}


Procedure TDriveView.ExpandLevel(Node : TTreeNode; Level : Integer);
{Purpose: Expands all subnodes of node up to the given level}
Begin
  IF Not Assigned(Node) Or Not Assigned(Node.Data) Then
    Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['ExpandLevel']));
  Items.BeginUpdate;
  IterateSubTree(Node, CallBackExpandLevel, rsRecursive, coScanStartNode, Pointer(Level));
  Items.EndUpdate;
End; {ExpandLevel}


Function  TDriveView.CallBackDisplayName(Var Node : TTreeNode; Data: Pointer) : Boolean;
Begin
  Result := True;
  IF Not Assigned(Node) Then
    Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['CallBackDisplayName']));
  Node.Text := GetDisplayName(Node);
End; {CallBackDisplayName}


Function  TDriveView.CallBackSetDirSize(Var Node : TTreeNode; Data: Pointer) : Boolean;
Begin
  Result := True;
  IF Assigned(Node) Then
  Begin
    SetDirSize(Node);
    IF fShowDirSize Then
    Node.Text := GetDisplayName(Node);
    IF Assigned(Data) Then
    INC(PInt(Data)^, TNodeData(Node.Data).DirSize);
  End;
  Application.ProcessMessages;
  IF Not FContinue Then
  Exit;
End; {CallBackSetDirSize}




Function TDriveView.FormatDirSize(Size : Cardinal) : String;
Var FSize : Cardinal;
Begin
  FSize := Size;
  IF (Size > 0) And (Size < 1024) Then
  fSize := 1
  Else
  fSize := fSize DIV 1024;
  IF fSize <= 99999 Then
  Result := FormatSize(FSize) + 'K'
  Else
  Result := FormatSize(FSize DIV 1024) + 'M';
End; {FormatDirSize}


Procedure TDriveView.SetShowDirSize(ShowIt : Boolean);
Var Drive    : Char;
    RootNode : TTreeNode;
    SaveCursor: TCursor;

Begin
  IF ShowIt = fShowDirSize Then
  Exit;
  fShowDirSize := ShowIt;
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  Items.BeginUpdate;
  For Drive := FirstFixedDrive To LastDrive Do
  Begin
    IF DriveInfo[Drive].Valid Then
    Begin
      RootNode := DriveStatus[Drive].RootNode;
      IF Assigned(RootNode) Then
        IterateSubTree(RootNode, CallBackDisplayName, rsRecursive, coScanStartNode, NIL);
    End;
  End;
  Items.EndUpdate;
  Screen.Cursor := SaveCursor;
End; {SetShowDirSize}


Procedure TDriveView.RefreshDirSize(Node : TTreeNode);
Begin
  IF Not Assigned(Node) Then
    Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['RefreshDirSize']));
  CallBackSetDirSize(Node, NIL);
End; {RefreshDirSize}


Procedure TDriveView.RefreshDriveDirSize(Drive : TDrive);
Var SaveCursor : TCursor;
Begin
  SaveCursor    := Screen.Cursor;
  Screen.Cursor := crHourglass;
  Items.BeginUpdate;
  With DriveStatus[Drive] Do
  Begin
    IF Assigned(RootNode) Then
    IterateSubTree(RootNode, CallBackSetDirSize, rsRecursive, coScanStartNode, NIL);
  End;
  Items.EndUpdate;
  Screen.Cursor := SaveCursor;
End; {RefreshDriveDirSize}



Function TDriveView.GetDirSize(Node : TTreeNode) : Cardinal;
Begin
   IF Not Assigned(Node) Or Not Assigned(Node.Data) Then
    Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['GetDirSize']));
   IF TNodeData(Node.Data).DirSize = C_InvalidSize Then
   SetDirSize(Node);
   Result := TNodeData(Node.Data).DirSize;
End; {GetDirSize}


Procedure TDriveView.SetDirSize(Node : TTreeNode);
Var SRec : TSearchRec;
    Size : Cardinal;
Begin
   IF Not Assigned(Node) Or Not Assigned(Node.Data) Then
    Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['SetDirSize']));
   Size := 0;
   IF FindFirst(AddSlash(GetDirPath(Node)) + '*.*', faAnyFile, SRec) = 0 Then
   Begin
     Repeat
       IF (Srec.Attr And faDirectory) = 0 Then
       INC(Size, Srec.Size);
     Until FindNext(Srec) <> 0;
   End;
   FindClose(Srec);
   TNodeData(Node.Data).DirSize := Size;
End; {SetDirSize}


Function TDriveView.GetDisplayName(Node : TTreeNode) : String;
Var DirName : String;
Begin
  Result := '';
  IF Not Assigned(Node) OR Not Assigned(Node.Data) Then
    Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['GetDisplayName']));

  IF Node.Level = 0 Then
    Result := GetDriveText(GetDriveToNode(Node))
  Else
  Begin
    DirName := GetDirName(Node);
    Case FFileNameDisplay Of
      fndCap   : Result := UpperCase(DirName);
      fndNoCap : Result := LowerCase(DirName);
      fndNice  : If Length(DirName) <= 8 Then
                  Begin
                    Result := LowerCase(DirName);
                    Result[1] := Upcase(Result[1]);
                  End
                  Else
                    Result := DirName;
      Else
      Result := DirName;
    End; {Case}
  End;

  IF FShowDirSize Then
  Result := Result + ' = ' + FormatDirSize(GetDirSize(Node));
End; {GetDisplayName}


Procedure TDriveView.SetShowVolLabel(ShowIt : Boolean);
Begin
  IF ShowIt = fShowVolLabel Then
  Exit;
  fShowVolLabel := ShowIt;
  RefreshRootNodes(False, dvdsFloppy);
End; {SetShowVolLabel}


Procedure TDriveView.SetVolDisplayStyle(doStyle : TVolumeDisplayStyle);
Var Drive : TDrive;

Begin
  IF doStyle <> fVolDisplayStyle Then
  Begin
    fVolDisplayStyle := doStyle;
    IF Not fCreating Then
    For Drive := FirstDrive To LastDrive Do
    Begin
      IF DriveInfo[Drive].Valid Then
      DriveStatus[Drive].RootNode.Text := GetDisplayName(DriveStatus[Drive].RootNode);
    End;
    {RefreshRootNodes(False, dvdsFloppy);}
  End;
End; {SetVolDisplayStyle}


Procedure TDriveView.SetCompressedColor(Value : TColor);
Begin
  IF Value <> FCompressedColor Then
  Begin
    FCompressedColor := Value;
    Invalidate;
  End;
End; {SetCompressedColor}


Procedure TDriveView.SetFileNameDisplay(Value : TFileNameDisplay);
Var Drive : TDrive;
Begin
  IF Value <> FFileNameDisplay Then
  Begin
    FFileNameDisplay := Value;
    For Drive := FirstDrive To LastDrive Do
    With DriveStatus[Drive] Do
    IF Assigned(RootNode) And DriveStatus[Drive].Scanned Then
      IterateSubTree(RootNode, CallBackDisplayName, rsRecursive, coNoScanStartNode, NIL);
  End;
End; {SetFileNameDisplay}


Procedure TDriveView.DisplayContextMenu(Node : TTreeNode; ScreenPos : TPoint);
Var Verb              : String;

Begin
  IF Not Assigned(Node) Then
    Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['DisplayContextMenu']));
  IF Node <> Selected Then
  DropTarget := Node;

  Verb := EmptyStr;
  IF Assigned(FOnDisplayContextMenu) Then
  FOnDisplayContextMenu(Self);
  ShellDisplayContextMenu(FParentForm.Handle, ScreenPos, GetDirPathName(Node), CanEdit(Node), Verb, False);
  If Verb = shcRename Then
    Node.EditText
  Else If Verb = shcCut Then
  Begin
    LastClipBoardOperation := cboCut;
    LastPathCut := GetDirPathName(Node);
  End
  Else If Verb = shcCopy Then
    LastClipBoardOperation := cboCopy
  Else If Verb = shcPaste Then
    PasteFromClipBoard(GetDirPathName(Node));

  DropTarget := NIL;
End; {DisplayContextMenu (2)}


Procedure TDriveView.DisplayContextMenu(Node : TTreeNode);
Begin
  DisplayContextMenu(Node, Mouse.CursorPos);
End; {DisplayContextMenu (1)}


Procedure TDriveView.DisplayPropertiesMenu(Node : TTreeNode);
Begin
  IF Not Assigned(Node) Then
    Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['DisplayPropertiesMenu']));
  ShellExecuteContextCommand(FParentForm.Handle, shcProperties, GetDirPathName(Node));
End; {ContextMenu}


Function TDriveView.SortChildren(ParentNode : TTreeNode; Recurse : Boolean) : Boolean;
Var Node : TTreeNode;
Begin
  Result := False;
  IF Not Assigned(ParentNode) Then
    Raise ENodeNotAssigned.Create(Format(ErrorNodeNA, ['SortChildren']));
  IF TreeView_SortChildren(Self.Handle, ParentNode.ItemID, 0) Then
  Begin
    Result := True;
    IF Recurse Then
    Begin
      Node := ParentNode.GetFirstChild;
      While Assigned(Node) Do
      Begin
        IF Node.HasChildren Then
        SortChildren(Node, Recurse);
        Node := ParentNode.GetNextChild(Node);
      End;
    End;
  End;
End; {SortChildren}


Procedure TDriveView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  Inherited Notification(AComponent, Operation);
  If (Operation = opRemove) Then
  Begin
    IF AComponent = fDirView then
    fDirView := NIL
    Else
    IF AComponent = FDriveBox then
    FDriveBox := NIL
  End;
end; {Notification}


Procedure TDriveView.SetSelected(Node : TTreeNode);
Begin
  IF Node <> Selected Then
  Begin
    FChangeFlag := False;
    FCanChange := True;
    Inherited Selected := Node;
    IF Not fChangeFlag Then
    Change(Selected);
  End;
End; {SetSelected}


{=================================================================}
{ Drag&Drop handling:                                             }
{=================================================================}

{Called by TFileDeleteThread, when a file deletion was detected by the D&D receiving application:}
Procedure TDriveView.SignalDirDelete(Sender: TObject; Files : TStringList);
Begin
  IF Files.Count > 0 Then
    ValidateDirectory(FindNodeToPath(Files[0]));
End; {SignalDirDelete}


Procedure TDriveView.DDDragEnter(DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: longint; var Accept:boolean);
Var KeyBoardState : TKeyBoardState;
    i             : Integer;

Begin
  IF (FDD.FileList.Count > 0) And (Length(TFDDListItem(FDD.FileList[0]^).Name) > 0) Then
  Begin
    FDragDrive := TFDDListItem(FDD.FileList[0]^).Name[1];
    FExeDrag   := FDDLinkOnExeDrag And ((FDD.AvailableDropEffects And DropEffect_Link) <> 0);
    IF FExeDrag Then
    For i := 0 To FDD.FileList.Count - 1 Do
      If Not isExecutable(TFDDListItem(FDD.FileList[i]^).Name) Then
      Begin
        FExeDrag := False;
        Break;
      End;
  End
  Else
    FDragDrive := #0;

  GetSystemTimeAsFileTime(DragOverTime);
  GetSystemTimeAsFileTime(LastHScrollTime);
  GetSystemTimeAsFileTime(LastVScrollTime);
  VScrollCount := 0;

  IF (GetKeyState(VK_SPACE) <> 0) And GetKeyboardState(KeyBoardState) Then
  Begin
    KeyBoardState[VK_SPACE] := 0;
    SetKeyBoardState(KeyBoardState);
  End;

  IF Assigned(FOnDDDragEnter) Then
  FOnDDDragEnter(Self, DataObj, grfKeyState, Pt, dwEffect, Accept);
End; {DDDragEnter}


Procedure TDriveView.DDDragLeave;
Begin
  IF Assigned(DropTarget) Then
  Begin
    IF GlobalDragImageList.Dragging Then
      GlobalDragImageList.HideDragImage;
    DropTarget := NIL;
    Update;
  End;

  IF Assigned(FOnDDDragLeave) Then
  FOnDDDragLeave(Self);
End; {DragLeave}


Procedure TDriveView.DDDragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: longint);
Var Node         : TTreeNode;
    KnowTime     : FILETIME;
    TempTopItem  : TTreeNode;
    NbPixels     : Integer;
    ScrollInfo   : TScrollInfo;
    KeyBoardState: TKeyBoardState;
    Rect1        : Trect;
    UpdateImage  : Boolean;
    LastDragNode : TTreeNode;
    TargetDrive  : Char;

begin
  IF dwEffect <> DropEffect_None Then
  Begin
    Node := GetNodeAt(Pt.X, Pt.Y);

    IF Assigned(Node) Then
    Begin
      LastDragNode := DropTarget;
      UpdateImage := False;
      IF GlobalDragImageList.Dragging And (LastDragNode <> Node) Then
      Begin
        IF Assigned(LastDragNode) Then
        Begin
          Rect1 := LastDragNode.DisplayRect(True);
          IF Rect1.Right >= Pt.x - GlobalDragImageList.GetHotSpot.X Then
          Begin
            GlobalDragImageList.HideDragImage;
            UpdateImage := True;
          End
          Else
          Begin
            Rect1 := Node.DisplayRect(True);
            IF Rect1.Right >= Pt.x - GlobalDragImageList.GetHotSpot.X Then
            Begin
              GlobalDragImageList.HideDragImage;
              UpdateImage := True;
            End
          End;
        End
        Else
        {LastDragNode not assigned:}
        Begin
          GlobalDragImageList.HideDragImage;
          UpdateImage := True;
        End;
      End;
      DropTarget := Node;
      IF UpdateImage Then
        GlobalDragImageList.ShowDragImage;

      TargetDrive := GetDirPath(Node)[1];

      {Drop-operation allowed at this location?}
      IF Assigned(DragNode) And
         (dwEffect <> DropEffect_Link) And
        ((Node = DragNode) Or Node.HasAsParent(DragNode) Or (DragNode.Parent = Node)) Then
        dwEffect := DropEffect_None;

      GetSystemTimeAsFileTime(KnowTime);

      IF GetKeyState(VK_SPACE) = 0 Then
      Begin
        {Expand node after 2.5 seconds: }
        IF Not Assigned(LastDragNode) Or (LastDragNode <> Node) Then
          GetSystemTimeAsFileTime(DragOverTime)     {not previous droptarget: start timer}
        Else
        Begin
          IF ((INT64(KnowTime) - INT64(DragOverTime)) > DDExpandDelay) Then
          Begin
            TempTopItem := TopItem;
            GlobalDragImageList.HideDragImage;
            Node.Expand(False);
            TopItem := TempTopItem;
            Update;
            GlobalDragImageList.ShowDragImage;
            DragOverTime := KnowTime;
          End;
        End;
      End
      Else
      Begin
        {restart timer}
        GetSystemTimeAsFileTime(DragOverTime);
        IF GetKeyboardState(KeyBoardState) Then
        Begin
          KeyBoardState[VK_Space] := 0;
          SetKeyBoardState(KeyBoardState);
        End;

        TempTopItem := TopItem;
        GlobalDragImageList.HideDragImage;
        IF Not Node.HasChildren Then
        ValidateDirectory(Node);
        IF Node.Expanded Then
        Begin
          IF Not Selected.HasAsParent(Node) Then
          Node.Collapse(False);
        End
        Else
        Node.Expand(False);
        TopItem := TempTopItem;
        Update;
        GlobalDragImageList.ShowDragImage;
      End;

      NbPixels := Abs((Font.Height));

      {Vertical treescrolling:}
      IF ((INT64(KnowTime) - INT64(LastVScrollTime)) > DDVScrollDelay) OR
         ((VScrollCount > 3) And ((INT64(KnowTime) - INT64(LastVScrollTime)) > (DDVScrollDelay Div 4))) Then
      Begin
        {Scroll tree up, if droptarget is topitem:}
        IF Node = TopItem Then
        Begin
          GlobalDragImageList.HideDragImage;
          Perform(WM_VSCROLL, SB_LINEUP, 0);
          GlobalDragImageList.ShowDragImage;
          GetSystemTimeAsFileTime(LastVScrollTime);
          INC(VScrollCount);
        End
        Else
        {Scroll tree down, if next visible item of droptarget is not visible:}
        Begin
          IF PT.Y + 3 * nbPixels > Height Then
          Begin
            GlobalDragImageList.HideDragImage;
            Perform(WM_VSCROLL, SB_LINEDOWN, 0);
            GlobalDragImageList.ShowDragImage;
            GetSystemTimeAsFileTime(LastVScrollTime);
            INC(VScrollCount);
          End
          Else
          Begin
            VScrollCount := 0;
          End;
        End;
      End; {VScrollDelay}

      {Horizontal treescrolling:}
      {Scroll tree Left}
      IF ((INT64(KnowTime) - INT64(LastHScrollTime)) > DDHScrollDelay) Then
      Begin
        GetSystemTimeAsFileTime(LastHScrollTime);
        ScrollInfo.cbSize := SizeOf(ScrollInfo);
        ScrollInfo.FMask := SIF_ALL;
        GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
        if ScrollInfo.nMin <> ScrollInfo.nMax then
        Begin
          if (PT.X < 50 ) then
          Begin
            IF Node.DisplayRect(True).Right + 50 < Width Then
            Begin
              GlobalDragImageList.HideDragImage;
              Perform(WM_HSCROLL, SB_LINELEFT, 0);
              GlobalDragImageList.ShowDragImage;
            End;
          End
          Else
          IF (PT.X > (Width - 50)) Then
          Begin
            IF Node.DisplayRect(True).Left  > 50  Then
            Begin
              GlobalDragImageList.HideDragImage;
              Perform(WM_HSCROLL, SB_LINERIGHT, 0);
              GlobalDragImageList.ShowDragImage;
            End;
          End;
        End;
      End;

    {Set Drop effect:}
    IF (TNodeData(DropTarget.Data).isRecycleBin And FDD.FileNamesAreMapped) Then
      dwEffect := DropEffect_None
    Else
    Begin
      IF TNodeData(DropTarget.Data).isRecycleBin Then
      dwEffect := DropEffect_Move
      Else
      IF (grfKeyState And (MK_CONTROL Or MK_SHIFT) = 0) Then
      Begin
        If FExeDrag And (TargetDrive >= FirstFixedDrive) And (FDragDrive >= FirstFixedDrive) Then
          dwEffect := DropEffect_Link
        Else
        IF (dwEffect = DropEffect_Copy) And
           ((DragDrive = GetDriveToNode(DropTarget)) And
             (FDD.AvailableDropEffects and DropEffect_Move <> 0)) Then
             dwEffect := DropEffect_Move;
      End;
    End;



    End {Assigned(Node)}
    Else
    dwEffect := DropEffect_None;
  End;


  IF Assigned(FOnDDDragOver) Then
  FOnDDDragOver(Self, grfKeyState, Pt, dwEffect);
End; {DDDragOver}


Procedure TDriveView.DDDrop(DataObj: IDataObject; grfKeyState: Longint;  pt: TPoint; var dwEffect: longint);
Begin
  IF GlobalDragImageList.Dragging Then
  GlobalDragImageList.HideDragImage;
  IF dwEffect = DropEffect_None Then
  DropTarget := NIL;
  IF Assigned(FOnDDDrop) Then
  FOnDDDrop(Self, DataObj, grfKeyState, Pt, dwEffect);
End; {DDDrop}


Procedure TDriveView.DDQueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint; var Result: HResult);
Var P        : TPoint;
    ClientP  : TPoint;
    KnowTime : FILETIME;

Begin
  IF Assigned(FOnDDQueryContinueDrag) Then
  FOnDDQueryContinueDrag(Self, fEscapePressed, grfKeyState, Result);

  IF fEscapePressed Then
  Begin
    IF GlobalDragImageList.Dragging Then
      GlobalDragImageList.HideDragImage;
    DropTarget := NIL;
    Exit;
  End;

  IF (Result = DRAGDROP_S_DROP) Then
  Begin
    GetSystemTimeAsFileTime(KnowTime);
    IF ((INT64(KnowTime) - INT64(DragStartTime)) <= DDDragStartDelay) Then
    Result := DRAGDROP_S_CANCEL;
  End;
  
  IF GlobalDragImageList.Dragging Then
  Begin
    GetCursorPos(P);
    {Convert screen coordinates to the parentforms coordinates:}
    ClientP := FParentForm.ScreenToClient(P);
    {Move the drag image to the new position and show it:}
    IF Not CompareMem(@ClientP, @FDragPos, SizeOf(TPoint)) Then
    Begin
      FDragPos := ClientP;
      IF PtInRect(FParentForm.BoundsRect, P) Then
      Begin
        GlobalDragImageList.DragMove(ClientP.X, ClientP.Y);
        GlobalDragImageList.ShowDragImage;
      End
      Else
        GlobalDragImageList.HideDragImage;
    End;
  End;
End; {DDQueryContinueDrag}


Procedure TDriveView.DDGiveFeedback(dwEffect: Longint; var Result: HResult);
Begin
  IF Assigned(FOnDDGiveFeedback) Then
  FOnDDGiveFeedback(Self, dwEffect, Result);
End; {DDGiveFeedback}


Procedure TDriveView.DDSpecifyDropTarget(Sender: TObject; DragDropHandler : boolean; pt: TPoint; var pidlFQ : PItemIDList; var Filename : string);
Begin
  pidlFQ := NIL;
  IF DragDropHandler And Assigned(DropTarget) Then
    FileName := GetDirPathName(DropTarget)
  Else
    FileName := EmptyStr;
End; {DDSpecifyDropTarget}


Procedure TDriveView.DDDragDetect(grfKeyState: Longint; DetectStart, Pt: TPoint; DragStatus:TDragDetectStatus);
Var DDResult       : TDragResult;
    DragPath       : String;
    DragParentPath : String;
    DragNodeLevel  : Integer;
{$IFNDEF NO_THREADS}
    WatchThreadOK  : Boolean;
{$ENDIF}
    P              : TPoint;
    Himl           : HImageList;
    NodeRect       : TRect;

Begin
  IF (DragStatus = ddsDrag) And Not Assigned(DragNode) Then
  Begin
    P := ScreenToClient(FStartPos);
    DragNode := GetNodeAt(P.X, P.Y);
  End;

  IF Assigned(FOnDDDragDetect) Then
  FOnDDDragDetect(Self, grfKeyState, DetectStart, Pt, DragStatus);

  IF (DragStatus = ddsDrag) And Assigned(DragNode) Then
  Begin
    NodeRect := DragNode.DisplayRect(True);
    Dec(NodeRect.Left, 16);

    {Check, wether the mouse cursor was within the nodes display rectangle:}
    IF (NodeRect.Left > P.X) Or (NodeRect.Right < P.X) Then
    Begin
      DragNode := NIL;
      Exit;
    End;

    FDragDrive := #0;
    {Create the dragimage:}
    GlobalDragImageList := FDragImageList;
    IF UseDragImages Then
    Begin
      {Hide the selection mark to get a proper dragimage:}
      IF Selected = DragNode Then
      Selected := NIL;
      HIml := TreeView_CreateDragImage(Handle, DragNode.ItemID);

      {Show the selection mark if it was hidden:}
      IF Not Assigned(Selected) Then
      Selected := DragNode;

      IF Himl <> Invalid_Handle_Value Then
      Begin
         GlobalDragImageList.Handle := Himl;
         GlobalDragImageList.SetDragImage(0, P.X - NodeRect.TopLeft.X, P.Y - NodeRect.TopLeft.Y);
         P := FParentForm.ScreenToClient(Pt);
         GlobalDragImageList.BeginDrag(FParentForm.Handle, P.X, P.Y);
         GlobalDragImageList.HideDragImage;
         ShowCursor(True);
      End;
    End;
    Dragpath := GetDirPathName(DragNode);
    IF Assigned(DragNode.Parent) Then
    DragParentPath := GetDirPathName(DragNode.Parent)
    Else
    DragParentPath := DragPath;

    DragNodeLevel := DragNode.Level;

    FDD.FileList.Clear;
    FDD.CompleteFileList := GetDriveType(DragPath[1]) <> DRIVE_REMOVABLE;
    FDD.FileList.AddItem(nil,DragPath);
    IF DragNodeLevel = 0 Then
      FDD.SourceEffects := FDD.SourceEffects - [deCopy, deMove]
    Else
      FDD.SourceEffects := FDD.SourceEffects + [deCopy, deMove];

{$IFNDEF NO_THREADS}
    WatchThreadOK := WatchThreadActive;
{$ENDIF}
    DropSourceControl := Self;

    GetSystemTimeAsFileTime(DragStartTime);

    {Supress the context menu:}
    fContextMenu := False;

    {Execute the drag&drop-Operation:}
    DDResult := FDD.Execute;

    {the drag&drop operation is finished, so clean up the used drag image:}
    GlobalDragImageList.EndDrag;
    GlobalDragImageList.Clear;

    Application.ProcessMessages;
    FDD.FileList.Clear;

    FDragDrive := #0;
    IF DDResult = drCancelled Then
      DropTarget := NIL;

    IF  (DDResult = drMove)
{$IFNDEF NO_THREADS}
        And Not WatchThreadOK
{$ENDIF}
        Then
    Begin
      IF (DragNodeLevel > 0) OR
        (DragParentPath <> GetDirPathName(Selected.Parent)) Then
        Begin
          DragNode := FindNodeToPath(DragPath);
          IF Assigned(DragNode) Then
          Begin
            DragFileList.Clear;
            DragFileList.Add(DragPath);
{$IFNDEF NO_THREADS}
            TFileDeleteThread.Create(DragFileList, MaxWaitTimeOut, SignalDirDelete);
{$ENDIF}
          End;
        End;
    End;
  DragNode := NIL;
  DropSourceControl := NIL;
  End;
End; {(DDDragDetect}


Procedure TDriveView.DDProcessDropped(Sender: TObject; grfKeyState: Longint;  pt: TPoint; dwEffect: Longint);
Var TargetPath : String;

Begin
  IF Assigned(DropTarget) Then
  Begin
    TargetPath := GetDirPathName(DropTarget);
    IF DirExists(TargetPath) Then
    Begin
      IF Assigned(FOnDDProcessDropped) Then
        FOnDDProcessDropped(Self, grfKeyState, pt, dwEffect);
      PerformDragDropFileOperation(TargetPath, dwEffect, TNodeData(DropTarget.Data).isRecycleBin);
      IF Assigned(FOnDDExecuted) Then
      FOnDDExecuted(Self, dwEffect);
    End
    Else
    Begin
      ValidateDirectory(DropTarget);
      DDError(DDPathNotFoundError);
    End;

    DropTarget := NIL;
    FDD.FileList.Clear;
  End;
End; {ProcessDropped}


Procedure TDriveView.PerformDragDropFileOperation(TargetPath : String; dwEffect: Integer; isRecycleBin : Boolean);
Var i                 : Integer;
    SourcePath        : String;
    SourceParentPath  : String;
    SourceFile        : String;
    SaveCursor        : TCursor;
    DoFileOperation   : Boolean;
    TargetNode        : TTreeNode;
    FileNamesAreMapped: Boolean;

Begin
      {DragDropExec}
  IF FDD.FileList.Count = 0 Then
  Exit;

  SaveCursor    := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  SourcePath    := EmptyStr;

  Try
    IF (dwEffect = DropEffect_Copy) Or
       (dwEffect = DropEffect_Move) Then
    Begin
{$IFNDEF NO_THREADS}
      StopAllWatchThreads;
      If Assigned(FDirView) Then
      FDirView.StopWatchThread;

      IF Assigned(DropSourceControl) And
        (DropSourceControl is TDirView) And
        (DropSourceControl <> FDirView) Then
      TDirView(DropSourceControl).StopWatchThread;
{$ENDIF}

      FileNamesAreMapped := TFDDListItem(FDD.FileList[0]^).MappedName <> '';

      {Set the source directory:}
      For i := 0 to FDD.FileList.Count - 1 Do
      Begin
        FFileOperator.OperandFrom.Add(TFDDListItem(FDD.FileList[i]^).Name);
        IF FileNamesAreMapped Then
        FFileOperator.OperandTo.Add(AddSlash(TargetPath) + TFDDListItem(FDD.FileList[i]^).MappedName);
      End;

      SourcePath := TFDDListItem(FDD.FileList[0]^).Name;
      SourceParentPath := ExtractFilePath(RemoveSlash(SourcePath));

      FDD.FileList.Clear;

      FFileOperator.Flags := [foAllowUndo, foNoConfirmMkDir];

      {Set the target directory or target files:}
      IF FileNamesAreMapped And Not isRecycleBin Then
        FFileOperator.Flags := FFileOperator.Flags + [foMultiDestFiles]
      Else
      Begin
        FFileOperator.Flags := FFileOperator.Flags - [foMultiDestFiles];
        FFileOperator.OperandTo.Clear;
        FFileOperator.OperandTo.Add(TargetPath);
      End;

      IF isRecycleBin Then
        FFileOperator.Operation := foDelete
      Else
      Case dwEffect Of
        DropEffect_Copy : FFileOperator.Operation := foCopy;
        DropEffect_Move : FFileOperator.Operation := foMove;
      End;  {Case}

      IF isRecycleBin Then
      Begin
        IF Not ConfirmDelete Then
        FFileOperator.Flags := FFileOperator.Flags + [foNoConfirmation];
      End
      Else
      IF Not ConfirmOverwrite Then
      FFileOperator.Flags := FFileOperator.Flags + [foNoConfirmation];

      DoFileOperation := True;
      IF Assigned(FOnDDFileOperation) Then
      FOnDDFileOperation(Self, dwEffect, SourcePath, TargetPath, DoFileOperation);

      IF DoFileOperation And (FFileOperator.OperandFrom.Count > 0) Then
      Begin
        FFileOperator.Execute;
        IF Assigned(FOnDDFileOperationExecuted) Then
        FOnDDFileOperationExecuted(Self, dwEffect, SourcePath, TargetPath);
        IF FileNamesAreMapped Then
        FFileOperator.ClearUndo;
      End;
    End
    Else
    IF (dwEffect = DropEffect_Link) Then
    { Create Link requested: }
    Begin
      For i := 0 to FDD.FileList.Count - 1 Do
      Begin
        SourceFile := TFDDListItem(FDD.FileList[i]^).Name;
        IF Length(SourceFile) = 3 Then
        SourcePath :=  Copy(DriveInfo[SourceFile[1]].PrettyName, 4, 255) + '(' + SourceFile[1] + ')'
        Else
        SourcePath := ExtractFileName(SourceFile);

        IF Not CreateFileShortCut(SourceFile, AddSlash(TargetPath) + ChangeFileExt(SourcePath, '.lnk'),
                                  ExtractFileNameOnly(SourceFile)) Then
          DDError(DDCreateShortCutError);
      End;
    End;

    IF dwEffect = DropEffect_Move Then
    Items.BeginUpdate;

    {Update source directory, if move-operation was performed:}
    IF ((dwEffect = DropEffect_Move) OR isRecycleBin) Then
      ValidateDirectory(FindNodeToPath(SourceParentPath));

    {Update subdirectories of target directory:}
    TargetNode := FindNodeToPath(TargetPath);
    IF Assigned(TargetNode) Then
    ValidateDirectory(TargetNode)
    Else
    ValidateDirectory(DriveStatus[TargetPath[1]].RootNode);

    IF dwEffect = DropEffect_Move Then
    Items.EndUpdate;

    {Update linked component TDirView:}
    IF Assigned(FDirView)
{$IFNDEF NO_THREADS}
        And Not FDirView.WatchThreadActive
{$ENDIF}
        Then
      Case dwEffect of
        DropEffect_Copy,
        DropEffect_Link: If (AddSlash(TargetPath) = AddSlash(DirView.Path)) Then
                           FDirView.Reload2;
        DropEffect_Move: If (AddSlash(TargetPath)       = AddSlash(DirView.Path)) Or
                            (AddSlash(SourceParentPath) = AddSlash(DirView.Path)) Then
                         Begin
                           IF FDirView <> DropSourceControl Then
                           FDirView.Reload2;
                         End;
      End; {Case}

    {Update the DropSource control, if files are moved and it is a TDirView:}
    IF (dwEffect = DropEffect_Move) And (DropSourceControl is TDirView) Then
    TDirView(DropSourceControl).ValidateSelectedFiles;

  Finally
    FFileOperator.OperandFrom.Clear;
    FFileOperator.OperandTo.Clear;
{$IFNDEF NO_THREADS}
    StartAllWatchThreads;

    IF Assigned(FDirView) And Not FDirView.WatchThreadActive Then
    FDirView.StartWatchThread;
    IF Assigned(DropSourceControl) And (DropSourceControl is TDirView) And Not TDirView(DropSourceControl).WatchThreadActive Then
    TDirView(DropSourceControl).StartWatchThread;
{$ENDIF}
    Screen.Cursor := SaveCursor;
  End;
End; {PerformDragDropFileOperation}


Procedure TDriveView.DDError(ErrorNo : TDDError);
Begin
  IF Assigned(FOnDDError) Then
  FOnDDError(Self, ErrorNo)
  Else
  Raise EDragDrop.CreateFmt(ENGLISH_DragDropError, [Ord(ErrorNo)]);
End; {DDError}


Function TDriveView.GetCanUndoCopyMove : Boolean;
Begin
  Result := Assigned(FFileOperator) And FFileOperator.CanUndo;
End; {CanUndoCopyMove}


Function TDriveView.UndoCopyMove : Boolean;
Var LastTarget : String;
    LastSource : String;

Begin
  Result := False;
  IF FFileOperator.CanUndo Then
  Begin
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

    IF Assigned(FDirView) Then
    With FDirView Do
{$IFNDEF NO_THREADS}
    IF Not WatchThreadActive Then
{$ENDIF}
    Begin
      IF (AddSlash(ExtractFilePath(LastTarget)) = AddSlash(Path)) Or
         (AddSlash(ExtractFilePath(LastSource)) = AddSlash(Path)) Then
        Reload2;
    End;
  End;
End; {UndoCopyMove}


{Clipboard operations:}
Procedure TDriveView.SetLastPathCut(Path : String);
Var Node : TTreeNode;
Begin
  If FLastPathCut <> Path Then
  Begin
    Node := FindNodeToPath(FLastPathCut);
    IF Assigned(Node) Then
    Begin
      FLastPathCut := Path;
      Node.Cut     := False;
    End;
    Node := FindNodeToPath(Path);
    IF Assigned(Node) Then
    Begin
      FLastPathCut := Path;
      Node.Cut     := True;
    End;
  End;
End; {SetLastNodeCut}


Procedure TDriveView.EmptyClipboard;
Begin
  IF Windows.OpenClipBoard(0) Then
  Begin
    Windows.EmptyClipBoard;
    Windows.CloseClipBoard;
    LastPathCut := '';
    LastClipBoardOperation := cboNone;
    IF Assigned(FDirView) Then
    FDirView.EmptyClipboard;
  End;
End; {EmptyClipBoard}


Function TDriveView.CopyToClipBoard(Node : TTreeNode) : Boolean;
Begin
  Result := Assigned(Selected);
  IF Result Then
  Begin
    EmptyClipBoard;
    FDD.FileList.Clear;
    FDD.FileList.AddItem(NIL, GetDirPathName(Selected));
    Result := FDD.CopyToClipBoard;
    LastClipBoardOperation := cboCopy;
  End;
End; {CopyToClipBoard}


Function TDriveView.CutToClipBoard(Node : TTreeNode)  : Boolean;
Begin
  Result := Assigned(Node) And (Node.Level > 0) And CopyToClipBoard(Node);
  IF Result Then
  Begin
    LastPathCut := GetDirPathName(Node);
    LastClipBoardOperation := cboCut;
  End;
End; {CutToClipBoard}


Function TDriveView.CanPasteFromClipBoard : Boolean;
Begin
  Result := False;
  IF Assigned(Selected) And Windows.OpenClipboard(0) Then
  Begin
    Result := IsClipboardFormatAvailable(CF_HDROP);
    Windows.CloseClipBoard;
  End;
End; {CanPasteFromClipBoard}


Function TDriveView.PasteFromClipBoard(TargetPath : String = '')  : Boolean;
Begin
  FDD.FileList.Clear;
  Result := False;
  IF CanPasteFromClipBoard And
    {MP}{$IFDEF OLD_DND} FDD.GetFromClipBoard {$ELSE} FDD.PasteFromClipboard {$ENDIF}{/MP}
    Then
  Begin
    IF TargetPath = '' Then
    TargetPath := GetDirPathName(Selected);
    Case LastClipBoardOperation Of
      cboCopy,
      cboNone: Begin
                 PerformDragDropFileOperation(TargetPath, DropEffect_Copy, TNodeData(Selected.Data).isRecycleBin);
                 IF Assigned(FOnDDExecuted) Then
                 FOnDDExecuted(Self, DropEffect_Copy);
               End;
      cboCut : Begin
                 PerformDragDropFileOperation(TargetPath, DropEffect_Move, TNodeData(Selected.Data).isRecycleBin);
                 IF Assigned(FOnDDExecuted) Then
                 FOnDDExecuted(Self, DropEffect_Move);
                 EmptyClipBoard;
               End;
    End;
    Result := True;
  End;
End; {PasteFromClipBoard}



Procedure TDriveView.SetTargetPopUpMenu(PopMe : Boolean);
Begin
  IF PopMe <> FTargetPopUpMenu Then
  Begin

   FTargetPopUpMenu := PopMe;
    IF Assigned(FDD) Then
    FDD.TargetPopupMenu := PopMe;
  End;
end; {SetTargetPopUpMenu}

{$ENDIF}

initialization
{$IFDEF USE_DRIVEVIEW}
  ErrorInvalidDirName := English_ErrorInvalidDirName;
{$ENDIF}
end.
