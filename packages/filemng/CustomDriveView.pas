unit CustomDriveView;

interface

uses
  Classes, ComCtrls, CommCtrl, Windows, Controls, Forms, ShlObj, Messages,
    Graphics,
  DragDrop, CustomDirView, IEDriveInfo, DragDropFilesEx;

type
  {Types uses by the function IterateSubTree:}
  {TRecursiveScan: determines, wich nodes are scanned by the function IterateSubTree:
  rsNoRecursive:       Scan startnode only.
  rsRecursive:         Scan all subnodes of the startnode.
  rsRecursiveExisting: Scan all subnodes of the startnode but not new created subnodes.
  rsRecursiveExpanded: Scan all expanded subnodes of the startnode.}
  TRecursiveScan = (rsNoRecursive, rsRecursive, rsRecursiveExisting, rsRecursiveExpanded);

  {TScanStartnode: determines, wether the startnode should also be scanned:}
  TScanStartNode = (coNoScanStartNode, coScanStartNode);

  TCallBackFunc = function(var Node: TTreeNode; Data: Pointer): Boolean of object;

type
  TCustomDriveView = class(TCustomTreeView)
  protected
    FParentForm: TCustomForm;
    FDragFileList: TStringList;
    FUseDragImages: Boolean;
    FDragDropFilesEx: TCustomizableDragDropFilesEx;
    FDragImageList: TDragImageList;
    FDragDrive: TDrive;
    FExeDrag: Boolean;
    FDDLinkOnExeDrag: Boolean;
    FDragOverTime: FILETIME;
    FLastVScrollTime: FILETIME;
    FLastHScrollTime: FILETIME;
    FVScrollCount: Integer;
    FDragNode: TTreeNode;
    FDragStartTime: FILETIME;
    FDragPos: TPoint;
    FStartPos: TPoint;
    FContextMenu: Boolean;
    FCanChange: Boolean;
    FUseSystemContextMenu: Boolean;
    FDimmHiddenDirs: Boolean;
    FShowHiddenDirs: Boolean;
    FContinue: Boolean;
    FImageList: TImageList;

    FOnDDDragEnter: TDDOnDragEnter;
    FOnDDDragLeave: TDDOnDragLeave;
    FOnDDDragOver: TDDOnDragOver;
    FOnDDDrop: TDDOnDrop;
    FOnDDQueryContinueDrag: TDDOnQueryContinueDrag;
    FOnDDChooseEffect: TDDOnChooseEffect;
    FOnDDGiveFeedback: TDDOnGiveFeedback;
    FOnDDDragDetect: TDDOnDragDetect;
    FOnDDMenuPopup: TOnMenuPopup;
    FOnDDProcessDropped: TOnProcessDropped;
    FOnDDError: TDDErrorEvent;
    FOnDDExecuted: TDDExecutedEvent;
    FOnDDFileOperation: TDDFileOperationEvent;
    FOnDDFileOperationExecuted: TDDFileOperationExecutedEvent;
    FOnDDCreateDragFileList: TDDOnCreateDragFileList;
    FOnDDEnd: TNotifyEvent;
    FOnDDCreateDataObject: TDDOnCreateDataObject;
    FLastDDResult: TDragResult;

    function GetTargetPopupMenu: Boolean;
    procedure SetTargetPopUpMenu(Value: Boolean);
    procedure SetDimmHiddenDirs(Value: Boolean);
    procedure SetShowHiddenDirs(Value: Boolean);

    function GetDirectory: string; virtual;
    procedure SetDirectory(Value: string); virtual;

    function GetCustomDirView: TCustomDirView; virtual; abstract;
    procedure SetCustomDirView(Value: TCustomDirView); virtual; abstract;

    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetNodeFromHItem(Item: TTVItem): TTreeNode;
    function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean; override;
    function CustomDrawItem(Node: TTreeNode; State: TCustomDrawState;
      Stage: TCustomDrawStage; var PaintImages: Boolean): Boolean; override;

    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg: TWMLButtonDown); message WM_LBUTTONUP;
    procedure WMRButtonDown(var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMContextMenu(var Msg: TWMContextMenu); message WM_CONTEXTMENU;

    procedure Delete(Node: TTreeNode); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    procedure InternalOnDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);

    procedure DDDragEnter(DataObj: IDataObject; KeyState: Longint;
      Point: TPoint; var Effect: Longint; var Accept: Boolean);
    procedure DDDragLeave;
    procedure DDDragOver(KeyState: Longint; Point: TPoint; var Effect: Longint);
    procedure DDDrop(DataObj: IDataObject; KeyState: Longint; Point: TPoint;
      var Effect: Longint);
    procedure DDQueryContinueDrag(EscapePressed: BOOL; KeyState: Longint;
      var Result: HResult);
    procedure DDDropHandlerSucceeded(Sender: TObject; KeyState: Longint;
      Point: TPoint; Effect: Longint);
    procedure DDGiveFeedback(Effect: Longint; var Result: HResult);
    procedure DDMenuPopup(Sender: TObject; AMenu: HMenu; DataObj: IDataObject;
      AMinCustCmd: Integer; grfKeyState: Longint; Point: TPoint);
    procedure DDMenuDone(Sender: TObject; AMenu: HMenu); 
    procedure DDProcessDropped(Sender: TObject; KeyState: Longint;
      Point: TPoint; Effect: Longint);
    procedure DDError(Error: TDDError); virtual;
    procedure DDSpecifyDropTarget(Sender: TObject; DragDropHandler: Boolean;
      Point: TPoint; var PIDL: PItemIDList; var Filename: string);
    procedure DDDragDetect(KeyState: Longint; DetectStart, Point: TPoint;
      DragStatus: TDragDetectStatus); virtual;
    procedure PerformDragDropFileOperation(Node: TTreeNode; Effect: Integer); virtual; abstract;

    procedure DDChooseEffect(KeyState: Integer; var Effect: Integer); virtual; 
    function DragCompleteFileList: Boolean; virtual; abstract;
    function DDExecute: TDragResult; virtual;
    function DDSourceEffects: TDropEffectSet; virtual; abstract;

    function NodePath(Node: TTreeNode): string; virtual; abstract;
    function NodeIsRecycleBin(Node: TTreeNode): Boolean; virtual;
    function NodePathExists(Node: TTreeNode): Boolean; virtual;
    function NodeColor(Node: TTreeNode): TColor; virtual; abstract;
    function NodeCanDrag(Node: TTreeNode): Boolean; virtual;
    function NodeOverlayIndexes(Node: TTreeNode): Word; virtual;
    function FindPathNode(Path: string): TTreeNode; virtual; abstract;
    procedure ClearDragFileList(FileList: TFileList); virtual;
    procedure AddToDragFileList(FileList: TFileList; Node: TTreeNode); virtual;

    procedure ValidateDirectoryEx(Node: TTreeNode; Recurse: TRecursiveScan;
      NewDirs: Boolean); virtual; abstract;
    procedure ValidateVisibleDirectories(Node: TTreeNode);
    procedure ValidateAllDirectories(Node: TTreeNode);
    procedure RebuildTree; virtual; abstract;

    procedure DisplayContextMenu(Node: TTreeNode; ScreenPos: TPoint); virtual; abstract;
    procedure DisplayPropertiesMenu(Node: TTreeNode); virtual; abstract;

    property ImageList: TImageList read FImageList;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ValidateDirectory(Node: TTreeNode);
    procedure CenterNode(Node: TTreeNode); virtual;
    function SortChildren(ParentNode: TTreeNode; Recurse: Boolean): Boolean;
    function IterateSubTree(var StartNode : TTreeNode;
      CallBackFunc: TCallBackFunc; Recurse: TRecursiveScan;
      ScanStartNode: TScanStartNode; Data: Pointer): Boolean;
    function NodePathName(Node: TTreeNode): string; virtual; abstract;

    property DragDropFilesEx: TCustomizableDragDropFilesEx read FDragDropFilesEx;
    property UseSystemContextMenu: Boolean read FUseSystemContextMenu
      write FUseSystemContextMenu default True;
    property DimmHiddenDirs: Boolean read FDimmHiddenDirs
      write SetDimmHiddenDirs default False;
    property ShowHiddenDirs: Boolean read FShowHiddenDirs
      write SetShowHiddenDirs default False;

    property DDLinkOnExeDrag: Boolean read FDDLinkOnExeDrag write FDDLinkOnExeDrag default True;

    {The mouse has entered the component window as a target of a drag&drop operation:}
    property OnDDDragEnter: TDDOnDragEnter read FOnDDDragEnter write FOnDDDragEnter;
    {The mouse has leaved the component window as a target of a drag&drop operation:}
    property OnDDDragLeave: TDDOnDragLeave read FOnDDDragLeave write FOnDDDragLeave;
    {The mouse is dragging in the component window as a target of a drag&drop operation:}
    property OnDDDragOver: TDDOnDragOver read FOnDDDragOver write FOnDDDragOver;
    {The Drag&drop operation is about to be executed:}
    property OnDDDrop: TDDOnDrop read FOnDDDrop write FOnDDDrop;
    property OnDDQueryContinueDrag: TDDOnQueryContinueDrag read FOnDDQueryContinueDrag write FOnDDQueryContinueDrag;
    property OnDDChooseEffect: TDDOnChooseEffect read FOnDDChooseEffect write FOnDDChooseEffect;
    property OnDDGiveFeedback: TDDOnGiveFeedback read FOnDDGiveFeedback write FOnDDGiveFeedback;
    {A drag&drop operation is about to be initiated whith the components window as the source:}
    property OnDDDragDetect: TDDOnDragDetect read FOnDDDragDetect write FOnDDDragDetect;
    {The component window is the target of a drag&drop operation:}
    property OnDDProcessDropped: TOnProcessDropped read FOnDDProcessDropped write FOnDDProcessDropped;
    {An error has occured during a drag&drop operation:}
    property OnDDError: TDDErrorEvent read FOnDDError write FOnDDError;
    {The drag&drop operation has been executed:}
    property OnDDExecuted: TDDExecutedEvent read FOnDDExecuted write FOnDDExecuted;
    {Event is fired just before executing the fileoperation. This event is also fired when
     files are pasted from the clipboard:}
    property OnDDFileOperation: TDDFileOperationEvent read FOnDDFileOperation write FOnDDFileOperation;
    {Event is fired after executing the fileoperation. This event is also fired when
     files are pasted from the clipboard:}
    property OnDDFileOperationExecuted: TDDFileOperationExecutedEvent read FOnDDFileOperationExecuted write FOnDDFileOperationExecuted;
    property OnDDCreateDragFileList: TDDOnCreateDragFileList
      read FOnDDCreateDragFileList write FOnDDCreateDragFileList;
    property OnDDEnd: TNotifyEvent
      read FOnDDEnd write FOnDDEnd;
    property OnDDCreateDataObject: TDDOnCreateDataObject
      read FOnDDCreateDataObject write FOnDDCreateDataObject;
    property OnDDMenuPopup: TOnMenuPopup read FOnDDMenuPopup write FOnDDMenuPopup;

    { Show drag images during a drag&drop operation }
    property UseDragImages: Boolean read FUseDragImages write FUseDragImages default True;

    { Show popupmenu when dropping a file with the right mouse button }
    property TargetPopUpMenu: Boolean read GetTargetPopUpMenu write SetTargetPopUpMenu default True;

    {Current selected directory:}
    property Directory: string read GetDirectory write SetDirectory;
    property DragNode: TTreeNode read FDragNode;

    property Continue: Boolean read FContinue write FContinue;
    property LastDDResult: TDragResult read FLastDDResult;
  end;

resourcestring
  SDragDropError = 'Drag&drop error: %d';

implementation

uses
  SysUtils, ShellApi, ImgList,
  IEListView, BaseUtils;

const
  DDExpandDelay = 25000000;

constructor TCustomDriveView.Create(AOwner: TComponent);
var
  WinVer: TOSVersionInfo;
begin
  inherited;

  WinVer.dwOSVersionInfoSize := SizeOf(WinVer);
  GetVersionEx(WinVer);

  DragMode := dmAutomatic;
  FDragFileList := TStringList.Create;
  FUseDragImages := (Win32PlatForm = VER_PLATFORM_WIN32_NT) or (WinVer.dwMinorVersion > 0);
  FDragDrive := #0;
  FExeDrag := False;
  FDDLinkOnExeDrag := True;
  FContextMenu := False;
  FCanChange := True;
  FUseSystemContextMenu := True;
  FContinue := True;

  FDragDropFilesEx := TCustomizableDragDropFilesEx.Create(Self);
  with FDragDropFilesEx do
  begin
    AcceptOwnDnd := True;
    {MP}
    {$IFDEF OLD_DND}
    AutoDetectDnD := False;
    {$ELSE}
    DragDetect.Automatic := False;
    {$ENDIF}
    {/MP}
    BringToFront := True;
    CompleteFileList := True;
    NeedValid := [nvFileName];
    RenderDataOn := rdoEnterAndDropSync;
    TargetPopUpMenu := True;

    OnDragEnter := DDDragEnter;
    OnDragLeave := DDDragLeave;
    OnDragOver := DDDragOver;
    OnDrop := DDDrop;
    OnQueryContinueDrag := DDQueryContinueDrag;
    OnSpecifyDropTarget := DDSpecifyDropTarget;
    OnMenuPopup := DDMenuPopup;
    OnMenuDestroy := DDMenuDone;
    OnDropHandlerSucceeded := DDDropHandlerSucceeded;
    OnGiveFeedback := DDGiveFeedback;
    OnProcessDropped := DDProcessDropped;
    OnDragDetect := DDDragDetect;
  end;

  OnCustomDrawItem := InternalOnDrawItem;
end;

destructor TCustomDriveView.Destroy;
begin
  FreeAndNil(FImageList);

  if Assigned(Images) then
    Images.Free;

  if Assigned(StateImages) then
    StateImages.Free;

  if Assigned(FDragImageList) then
  begin
    if GlobalDragImageList = FDragImageList then
      GlobalDragImageList := nil;
    FDragImageList.Free;
  end;

  FDragFileList.Destroy;

  if Assigned(FDragDropFilesEx) then
    FDragDropFilesEx.Free;

  inherited Destroy;
end;

procedure TCustomDriveView.CreateWnd;
begin
  inherited;

  if not Assigned(Images) then
    Images := ShellImageList(Self, SHGFI_SMALLICON);
  if not Assigned(StateImages) then
    StateImages := ShellImageList(Self, SHGFI_OPENICON);

  if not (csDesigning in ComponentState) then
    FDragImageList := TDragImageList.Create(Self);
  if not Assigned(GlobalDragImageList) then
    GlobalDragImageList := FDragImageList;

  FDragDropFilesEx.DragDropControl := Self;
  FParentForm := GetParentForm(Self);

  FImageList := OverlayImageList(16);
end;

procedure TCustomDriveView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = GetCustomDirView then SetCustomDirView(nil);
  end;
end;

procedure TCustomDriveView.InternalOnDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
  State: TCustomDrawState; var DefaultDraw: Boolean);
var
  FItemColor: TColor;
begin
  if Assigned(Node) and Assigned(Node.Data) and (Node <> DropTarget) then
  begin
    if not Node.Selected then
    begin
      FItemColor := NodeColor(Node);
      if (FItemColor <> clDefaultItemColor) and
         (Canvas.Font.Color <> FItemColor) then
           Canvas.Font.Color := FItemColor;
    end
      else
    if (not Self.Focused) and HideSelection then
    begin
      Canvas.Brush.Color := clBtnFace;
      Canvas.Font.Color := clBtnText;
    end;
  end;
end; {InternalOnDrawItem}

procedure TCustomDriveView.DDDragEnter(DataObj: IDataObject; KeyState: Longint;
  Point: TPoint; var Effect: Longint; var Accept: Boolean);
var
  KeyBoardState : TKeyBoardState;
  i: Integer;
begin
  if (FDragDropFilesEx.FileList.Count > 0) and
     (Length(TFDDListItem(FDragDropFilesEx.FileList[0]^).Name) > 0) Then
  begin
    FDragDrive := TFDDListItem(FDragDropFilesEx.FileList[0]^).Name[1];
    FExeDrag := FDDLinkOnExeDrag and ((FDragDropFilesEx.AvailableDropEffects and DropEffect_Link) <> 0);
    if FExeDrag then
    begin
      for i := 0 to FDragDropFilesEx.FileList.Count - 1 do
        if not IsExecutable(TFDDListItem(FDragDropFilesEx.FileList[i]^).Name) then
        begin
          FExeDrag := False;
          Break;
        end;
    end;
  end
    else
  begin
    FDragDrive := #0;
  end;

  GetSystemTimeAsFileTime(FDragOverTime);
  GetSystemTimeAsFileTime(FLastHScrollTime);
  GetSystemTimeAsFileTime(FLastVScrollTime);
  FVScrollCount := 0;

  if (GetKeyState(VK_SPACE) <> 0) And GetKeyboardState(KeyBoardState) then
  begin
    KeyBoardState[VK_SPACE] := 0;
    SetKeyBoardState(KeyBoardState);
  end;

  if Assigned(FOnDDDragEnter) then
    FOnDDDragEnter(Self, DataObj, KeyState, Point, Effect, Accept);
end; {DDDragEnter}

procedure TCustomDriveView.DDDragLeave;
begin
  if Assigned(DropTarget) then
  begin
    if GlobalDragImageList.Dragging then
      GlobalDragImageList.HideDragImage;
    DropTarget := nil;
    Update;
  end;

  if Assigned(FOnDDDragLeave) then
    FOnDDDragLeave(Self);
end; {DragLeave}

procedure TCustomDriveView.DDDragOver(KeyState: Longint; Point: TPoint; var Effect: Longint);
var
  Node: TTreeNode;
  KnowTime: FILETIME;
  TempTopItem: TTreeNode;
  NbPixels: Integer;
  ScrollInfo: TScrollInfo;
  KeyBoardState: TKeyBoardState;
  Rect1: TRect;
  UpdateImage: Boolean;
  LastDragNode: TTreeNode;
begin
  if Effect <> DropEffect_None then
  begin
    Node := GetNodeAt(Point.X, Point.Y);

    if Assigned(Node) then
    begin
      LastDragNode := DropTarget;
      UpdateImage := False;
      if GlobalDragImageList.Dragging and (LastDragNode <> Node) then
      begin
        if Assigned(LastDragNode) then
        begin
          Rect1 := LastDragNode.DisplayRect(True);
          if Rect1.Right >= Point.x - GlobalDragImageList.GetHotSpot.X then
          begin
            GlobalDragImageList.HideDragImage;
            UpdateImage := True;
          end
            else
          begin
            Rect1 := Node.DisplayRect(True);
            if Rect1.Right >= Point.x - GlobalDragImageList.GetHotSpot.X then
            begin
              GlobalDragImageList.HideDragImage;
              UpdateImage := True;
            end
          end;
        end
          else
        begin
          {LastDragNode not assigned:}
          GlobalDragImageList.HideDragImage;
          UpdateImage := True;
        end;
      end;

      DropTarget := Node;
      if UpdateImage then
        GlobalDragImageList.ShowDragImage;

      {Drop-operation allowed at this location?}
      if Assigned(FDragNode) and
         (Effect <> DropEffect_Link) and
         ((Node = FDragNode) or Node.HasAsParent(FDragNode) or (FDragNode.Parent = Node)) then
         Effect := DropEffect_None;

      GetSystemTimeAsFileTime(KnowTime);

      if GetKeyState(VK_SPACE) = 0 then
      begin
        {Expand node after 2.5 seconds: }
        if not Assigned(LastDragNode) or (LastDragNode <> Node) then
            GetSystemTimeAsFileTime(FDragOverTime)     {not previous droptarget: start timer}
          else
        begin
          if ((Int64(KnowTime) - Int64(FDragOverTime)) > DDExpandDelay) then
          begin
            TempTopItem := TopItem;
            GlobalDragImageList.HideDragImage;
            Node.Expand(False);
            TopItem := TempTopItem;
            Update;
            GlobalDragImageList.ShowDragImage;
            FDragOverTime := KnowTime;
          end;
        end;
      end
        else
      begin
        {restart timer}
        GetSystemTimeAsFileTime(FDragOverTime);
        if GetKeyboardState(KeyBoardState) then
        begin
          KeyBoardState[VK_Space] := 0;
          SetKeyBoardState(KeyBoardState);
        end;

        TempTopItem := TopItem;
        GlobalDragImageList.HideDragImage;
        if not Node.HasChildren then
          ValidateDirectory(Node);
        if Node.Expanded then
        begin
          if not Selected.HasAsParent(Node) then
            Node.Collapse(False);
        end
          else Node.Expand(False);
        TopItem := TempTopItem;
        Update;
        GlobalDragImageList.ShowDragImage;
      end;

      NbPixels := Abs((Font.Height));

      {Vertical treescrolling:}
      if ((Int64(KnowTime) - Int64(FLastVScrollTime)) > DDVScrollDelay) or
         ((FVScrollCount > 3) and
          ((Int64(KnowTime) - Int64(FLastVScrollTime)) > (DDVScrollDelay Div 4))) then
      begin
        {Scroll tree up, if droptarget is topitem:}
        if Node = TopItem then
        begin
          GlobalDragImageList.HideDragImage;
          Perform(WM_VSCROLL, SB_LINEUP, 0);
          GlobalDragImageList.ShowDragImage;
          GetSystemTimeAsFileTime(FLastVScrollTime);
          Inc(FVScrollCount);
        end
          else
        {Scroll tree down, if next visible item of droptarget is not visible:}
        begin
          if Point.Y + 3 * nbPixels > Height then
          begin
            GlobalDragImageList.HideDragImage;
            Perform(WM_VSCROLL, SB_LINEDOWN, 0);
            GlobalDragImageList.ShowDragImage;
            GetSystemTimeAsFileTime(FLastVScrollTime);
            Inc(FVScrollCount);
          end
            else
          begin
            FVScrollCount := 0;
          end;
        end;
      end; {VScrollDelay}

      {Horizontal treescrolling:}
      {Scroll tree Left}
      if ((Int64(KnowTime) - Int64(FLastHScrollTime)) > DDHScrollDelay) then
      begin
        GetSystemTimeAsFileTime(FLastHScrollTime);
        ScrollInfo.cbSize := SizeOf(ScrollInfo);
        ScrollInfo.FMask := SIF_ALL;
        GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
        if ScrollInfo.nMin <> ScrollInfo.nMax then
        begin
          if Point.X < 50 then
          begin
            if Node.DisplayRect(True).Right + 50 < Width then
            begin
              GlobalDragImageList.HideDragImage;
              Perform(WM_HSCROLL, SB_LINELEFT, 0);
              GlobalDragImageList.ShowDragImage;
            end;
          end
            else
          if Point.X > (Width - 50) then
          begin
            if Node.DisplayRect(True).Left > 50 then
            begin
              GlobalDragImageList.HideDragImage;
              Perform(WM_HSCROLL, SB_LINERIGHT, 0);
              GlobalDragImageList.ShowDragImage;
            end;
          end;
        end;
      end;
    end {Assigned(Node)}
      else
    begin
      DropTarget := nil;
    end;
  end;

  DDChooseEffect(KeyState, Effect);

  if Assigned(FOnDDDragOver) then
    FOnDDDragOver(Self, KeyState, Point, Effect);

  if not Assigned(DropTarget) then Effect := DropEffect_None
    else
  if NodeIsRecycleBin(DropTarget) then
  begin
    if FDragDropFilesEx.FileNamesAreMapped then Effect := DropEffect_None
      else Effect := DropEffect_Move;
  end;
end; {DDDragOver}

procedure TCustomDriveView.DDDrop(DataObj: IDataObject; KeyState: Longint;
  Point: TPoint; var Effect: Longint);
begin
  if GlobalDragImageList.Dragging then
    GlobalDragImageList.HideDragImage;

  if Effect = DropEffect_None then
    DropTarget := nil;

  if Assigned(FOnDDDrop) then
    FOnDDDrop(Self, DataObj, KeyState, Point, Effect);
end; {DDDrop}

procedure TCustomDriveView.DDQueryContinueDrag(EscapePressed: BOOL; KeyState: Longint;
  var Result: HResult);
var
  Point: TPoint;
  ClientPoint: TPoint;
  KnowTime: FILETIME;
begin
  if Result = DRAGDROP_S_DROP then
  begin
    GetSystemTimeAsFileTime(KnowTime);
    if ((Int64(KnowTime) - Int64(FDragStartTime)) <= DDDragStartDelay) then
      Result := DRAGDROP_S_CANCEL;
  end;

  if Assigned(FOnDDQueryContinueDrag) then
    FOnDDQueryContinueDrag(Self, EscapePressed, KeyState, Result);

  if EscapePressed then
  begin
    if GlobalDragImageList.Dragging then
      GlobalDragImageList.HideDragImage;
    DropTarget := nil;
  end
    else
  begin
    if GlobalDragImageList.Dragging then
    begin
      GetCursorPos(Point);
      {Convert screen coordinates to the parentforms coordinates:}
      ClientPoint := FParentForm.ScreenToClient(Point);
      {Move the drag image to the new position and show it:}
      if not CompareMem(@ClientPoint, @FDragPos, SizeOf(TPoint)) then
      begin
        FDragPos := ClientPoint;
        if PtInRect(FParentForm.BoundsRect, Point) then
        begin
          GlobalDragImageList.DragMove(ClientPoint.X, ClientPoint.Y);
          GlobalDragImageList.ShowDragImage;
        end
          else GlobalDragImageList.HideDragImage;
      end;
    end;
  end;
end; {DDQueryContinueDrag}

procedure TCustomDriveView.DDMenuPopup(Sender: TObject; AMenu: HMenu;
  DataObj: IDataObject; AMinCustCmd: Integer; grfKeyState: Longint; Point: TPoint);
begin
  if Assigned(OnDDMenuPopup) then
  begin
    OnDDMenuPopup(Self, AMenu, DataObj, AMinCustCmd, grfKeyState, Point);
  end;
end;

procedure TCustomDriveView.DDMenuDone(Sender: TObject; AMenu: HMenu);
begin
end;

procedure TCustomDriveView.DDDropHandlerSucceeded(Sender: TObject;
  KeyState: Integer; Point: TPoint; Effect: Integer);
begin
  DropTarget := nil;
end;

procedure TCustomDriveView.DDChooseEffect(KeyState: Integer; var Effect: Integer);
begin
  if Assigned(FOnDDChooseEffect) then
    FOnDDChooseEffect(Self, KeyState, Effect);
end;

procedure TCustomDriveView.DDGiveFeedback(Effect: Longint; var Result: HResult);
begin
  if Assigned(FOnDDGiveFeedback) then
    FOnDDGiveFeedback(Self, Effect, Result);
end; {DDGiveFeedback}

procedure TCustomDriveView.DDProcessDropped(Sender: TObject; KeyState: Longint;
  Point: TPoint; Effect: Longint);
begin
  try
    if Assigned(DropTarget) then
    try
      if NodePathExists(DropTarget) then
      begin
        if Assigned(FOnDDProcessDropped) then
          FOnDDProcessDropped(Self, KeyState, Point, Effect);
        if Effect <> DropEffect_None then
        begin
          PerformDragDropFileOperation(DropTarget, Effect);
          if Assigned(FOnDDExecuted) then
            FOnDDExecuted(Self, Effect);
        end;
      end
        else
      begin
        ValidateDirectory(DropTarget);
        DDError(DDPathNotFoundError);
      end;

    finally
      DropTarget := nil;
      ClearDragFileList(FDragDropFilesEx.FileList);
    end;
  except
    Application.HandleException(Self);
  end;
end; {ProcessDropped}

procedure TCustomDriveView.DDError(Error: TDDError);
begin
  if Assigned(FOnDDError) then FOnDDError(Self, Error)
    else raise Exception.CreateFmt(SDragDropError, [Ord(Error)]);
end; {DDError}

procedure TCustomDriveView.DDSpecifyDropTarget(Sender: TObject;
  DragDropHandler: Boolean; Point: TPoint; var PIDL: PItemIDList; var Filename: string);
begin
  PIDL := nil;
  if DragDropHandler and Assigned(DropTarget) then FileName := NodePathName(DropTarget)
    else FileName := EmptyStr;
end; {DDSpecifyDropTarget}

procedure TCustomDriveView.DDDragDetect(KeyState: Longint; DetectStart, Point: TPoint;
  DragStatus: TDragDetectStatus);
var
  P: TPoint;
  ImageList: HImageList;
  NodeRect: TRect;
  FileListCreated: Boolean;
  AvoidDragImage: Boolean;
begin
  if (DragStatus = ddsDrag) and (not Assigned(FDragNode)) then
  begin
    P := ScreenToClient(FStartPos);
    FDragNode := GetNodeAt(P.X, P.Y);
  end;

  if Assigned(FOnDDDragDetect) then
    FOnDDDragDetect(Self, KeyState, DetectStart, Point, DragStatus);

  if (DragStatus = ddsDrag) and Assigned(FDragNode) then
  begin
    NodeRect := FDragNode.DisplayRect(True);
    Dec(NodeRect.Left, 16);

    {Check, wether the mouse cursor was within the nodes display rectangle:}
    if (NodeRect.Left > P.X) or (NodeRect.Right < P.X) or
       (not NodeCanDrag(FDragNode)) then
    begin
      FDragNode := nil;
      Exit;
    end;

    FDragDrive := #0;

    ClearDragFileList(FDragDropFilesEx.FileList);
    FDragDropFilesEx.CompleteFileList := DragCompleteFileList;
    FileListCreated := False;
    AvoidDragImage := False;

    if Assigned(OnDDCreateDragFileList) then
    begin
      OnDDCreateDragFileList(Self, FDragDropFilesEx.FileList, FileListCreated);
      if FileListCreated then
        AvoidDragImage := True;
    end;

    if not FileListCreated then
    begin
      AddToDragFileList(FDragDropFilesEx.FileList, FDragNode);
    end;

    FDragDropFilesEx.SourceEffects := DDSourceEffects;

    if FDragDropFilesEx.FileList.Count > 0 then
    try
      {Create the dragimage:}
      GlobalDragImageList := FDragImageList;
      if UseDragImages and (not AvoidDragImage) then
      begin
        {Hide the selection mark to get a proper dragimage:}
        if Selected = FDragNode then
          Selected := nil;
        ImageList := TreeView_CreateDragImage(Handle, FDragNode.ItemID);

        {Show the selection mark if it was hidden:}
        if not Assigned(Selected) then
          Selected := FDragNode;

        if ImageList <> Invalid_Handle_Value then
        begin
          GlobalDragImageList.Handle := ImageList;
          GlobalDragImageList.SetDragImage(0, P.X - NodeRect.TopLeft.X, P.Y - NodeRect.TopLeft.Y);
          P := FParentForm.ScreenToClient(Point);
          GlobalDragImageList.BeginDrag(FParentForm.Handle, P.X, P.Y);
          GlobalDragImageList.HideDragImage;
          ShowCursor(True);
        end;
      end;

      DropSourceControl := Self;

      GetSystemTimeAsFileTime(FDragStartTime);

      {Supress the context menu:}
      FContextMenu := False;

      {Execute the drag&drop-Operation:}
      FLastDDResult := DDExecute;

      {the drag&drop operation is finished, so clean up the used drag image:}
      GlobalDragImageList.EndDrag;
      GlobalDragImageList.Clear;

      Application.ProcessMessages;
      
    finally
      ClearDragFileList(FDragDropFilesEx.FileList);

      FDragDrive := #0;
      DropTarget := nil;

      try
        if Assigned(OnDDEnd) then
          OnDDEnd(Self);
      finally
        DropSourceControl := nil;
        FDragNode := nil;
      end;
    end;
  end;
end; {(DDDragDetect}

function TCustomDriveView.DDExecute: TDragResult;
var
  DataObject: TDataObject;
begin
  DataObject := nil;
  if Assigned(OnDDCreateDataObject) then
    OnDDCreateDataObject(Self, DataObject);
    
  Result := FDragDropFilesEx.Execute(nil);
end;

function TCustomDriveView.GetNodeFromHItem(Item: TTVItem): TTreeNode;
begin
  Result := nil;
  if Items <> nil then
    with Item do
      if (state and TVIF_PARAM) <> 0 then
        Result := Pointer(lParam)
      else
        Result := Items.GetNode(hItem);
end; {GetNodeFromItem}

function TCustomDriveView.IsCustomDrawn(Target: TCustomDrawTarget;
  Stage: TCustomDrawStage): Boolean;
begin
  Result := inherited IsCustomDrawn(Target, Stage) or
    ((Target = dtItem) and (Stage = cdPostPaint));
end;

function TCustomDriveView.CustomDrawItem(Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages: Boolean): Boolean;
var
  Point: TPoint;
  Index: Integer;
  OverlayIndexes: Word;
  OverlayIndex: Word;
  Image: Word;
begin
  Result := inherited CustomDrawItem(Node, State, Stage, PaintImages);

  if Result and (Stage = cdPostPaint) and HasExtendedCOMCTL32 then
  begin
    Assert(Assigned(Node));
    OverlayIndexes := NodeOverlayIndexes(Node);
    OverlayIndex := 1;
    while OverlayIndexes > 0 do
    begin
      if (OverlayIndex and OverlayIndexes) <> 0 then
      begin
        Index := 0;
        Image := OverlayIndex;
        while Image > 1 do
        begin
          Inc(Index);
          Image := Image shr 1;
        end;

        Point := Node.DisplayRect(True).TopLeft;
        Dec(Point.X, Indent);

        ImageList_Draw(ImageList.Handle, Index, Self.Canvas.Handle,
          Point.X, Point.Y, ILD_TRANSPARENT);

        Dec(OverlayIndexes, OverlayIndex);
      end;
      OverlayIndex := OverlayIndex shl 1;
    end;
  end;
end;

procedure TCustomDriveView.CNNotify(var Msg: TWMNotify);
begin
  case Msg.NMHdr.code of
    TVN_BEGINDRAG:  DDDragDetect(MK_LBUTTON, FStartPos, Mouse.CursorPos, ddsDrag);
    TVN_BEGINRDRAG: DDDragDetect(MK_RBUTTON, FStartPos, Mouse.CursorPos, ddsDrag);
  else
    inherited;
  end;
end; {CNNotify}

procedure TCustomDriveView.WMLButtonDown(var Msg: TWMLButtonDown);
begin
  FCanChange := False;
  GetCursorPos(FStartPos);
  inherited;
end; {WMLButtonDown}

procedure TCustomDriveView.WMLButtonUp(var Msg: TWMLButtonDown);
begin
  FCanChange := True;
  if Assigned(DropTarget) and Assigned(DropTarget.Data) then
    Selected := DropTarget;
  DropTarget := nil;
  inherited;
end; {WMLButtonUp}

procedure TCustomDriveView.WMRButtonDown(var Msg: TWMRButtonDown);
begin
  GetCursorPos(FStartPos);
  if FDragDropFilesEx.DragDetectStatus <> ddsDrag then
    FContextMenu := True;
  inherited;
end; {WMRButtonDown}

procedure TCustomDriveView.WMContextMenu(var Msg: TWMContextMenu);
var
  Node: TTreeNode;
  Point: TPoint;
  PrevAutoPopup: Boolean;
begin
  PrevAutoPopup := False;
  try
    if Assigned(PopupMenu) then
    begin
      PrevAutoPopup := PopupMenu.AutoPopup;
      PopupMenu.AutoPopup := False;
    end;

    inherited;
  finally
    if Assigned(PopupMenu) then
      PopupMenu.AutoPopup := PrevAutoPopup;
  end;

  FStartPos.X := -1;
  FStartPos.Y := -1;

  try
    if FContextMenu then
    begin
      Point.X := Msg.XPos;
      Point.Y := Msg.YPos;
      Point := ScreenToClient(Point);
      Node := GetNodeAt(Point.X, Point.Y);
      if FUseSystemContextMenu and Assigned(Node) then
      begin
        if Assigned(OnMouseDown) then
          OnMouseDown(Self, mbRight, [], Msg.XPos, Msg.YPos);

        DisplayContextMenu(Node, Mouse.CursorPos);
      end
        else
      begin
        if Assigned(PopupMenu) then
          PopupMenu.Popup(Msg.XPos, Msg.YPos);
      end;
    end;
    FContextMenu := False;
  finally
    DropTarget := nil;
  end;
end; {WMContextMenu}

procedure TCustomDriveView.Delete(Node: TTreeNode);
begin
  if Node = FDragNode then
    FDragNode := nil;

  if Node = DropTarget then
  begin
    DropTarget := nil;
    Update;
  end;

  inherited;
end; {OnDelete}

procedure TCustomDriveView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (ssAlt in Shift) and (not IsEditing) and
     Assigned(Selected) then
  begin
    DisplayPropertiesMenu(Selected);
    Key := 0;
  end;
  inherited;
end; {KeyDown}

procedure TCustomDriveView.KeyPress(var Key : Char);
begin
  if Assigned(Selected) then
  begin
    if not IsEditing then
    begin
      case Key of
        #13, ' ':
          begin
            Selected.Expanded := not Selected.Expanded;
            Key := #0;
          end;
        '/':
          begin
            Selected.Collapse(True);
            Selected.MakeVisible;
            Key := #0;
          end;
        '*':
          Selected.MakeVisible;
      end {Case}
    end
  end;

  inherited;
end; {KeyPress}

procedure TCustomDriveView.KeyUp(var Key: Word; Shift: TShiftState);
var
  Point: TPoint;
begin
  inherited;

  if (Key = VK_APPS) and Assigned(Selected) then
  begin
    Point := ClientToScreen(Selected.DisplayRect(True).TopLeft);
    Inc(Point.Y, 20);
    DisplayContextMenu(Selected, Point);
  end;
end; {KeyUp}

procedure TCustomDriveView.ValidateDirectory(Node: TTreeNode);
begin
  ValidateDirectoryEx(Node, rsRecursiveExisting, False);
end;  {ValidateDirectory}

procedure TCustomDriveView.ValidateVisibleDirectories(Node: TTreeNode);
begin
  ValidateDirectoryEx(Node, rsRecursiveExpanded, False);
end; {ValidateVisibleDirectories}

procedure TCustomDriveView.ValidateAllDirectories(Node: TTreeNode);
begin
  ValidateDirectoryEx(Node, rsRecursive, True);
end; {ValidateAllDirectories}

procedure TCustomDriveView.CenterNode(Node: TTreeNode);
var
  NodePos: TRect;
  ScrollInfo: TScrollInfo;
begin
  if Assigned(Node) and (Items.Count > 0) then
  begin
    Node.MakeVisible;

    NodePos  := Node.DisplayRect(False);
    with ScrollInfo do
    begin
      cbSize := SizeOf(ScrollInfo);
      fMask := SIF_ALL;
      nMin := 0;
      nMax := 0;
      nPage := 0;
    end;
    GetScrollInfo(Handle, SB_VERT, ScrollInfo);

    if ScrollInfo.nMin <> ScrollInfo.nMax then
    begin
      {Scroll tree up:}
      if (NodePos.Top < Height div 4) and (ScrollInfo.nPos > 0) then
      begin
        ScrollInfo.fMask := SIF_POS;
        while (ScrollInfo.nPos > 0) and (NodePos.Top < (Height div 4)) do
        begin
          Perform(WM_VSCROLL, SB_LINEUP, 0);
          GetScrollInfo(Handle, SB_VERT, ScrollInfo);
          NodePos := Node.DisplayRect(False);
        end;
      end
        else
      if (NodePos.Top > ((Height * 3) div 4)) then
      begin
        {Scroll tree down:}
        ScrollInfo.fMask := SIF_POS;
        while (ScrollInfo.nPos + ABS(ScrollInfo.nPage) < ScrollInfo.nMax) and
          (NodePos.Top > ((Height * 3) div 4)) and
          (ScrollInfo.nPage > 0) do
        begin
          Perform(WM_VSCROLL, SB_LINEDOWN, 0);
          GetScrollInfo(Handle, SB_VERT, ScrollInfo);
          NodePos := Node.DisplayRect(False);
        end;
      end;
      NodePos := Node.DisplayRect(True);
    end;

    if NodePos.Left < 50 then
      Perform(WM_HSCROLL, SB_PAGELEFT, 0);
  end;
end; {CenterNode}

function TCustomDriveView.SortChildren(ParentNode: TTreeNode; Recurse: Boolean): Boolean;
var
  Node: TTreeNode;
begin
  Result := False;
  if Assigned(ParentNode) and
     TreeView_SortChildren(Self.Handle, ParentNode.ItemID, 0) then
  begin
    Result := True;
    if Recurse then
    begin
      Node := ParentNode.GetFirstChild;
      while Assigned(Node) do
      begin
        if Node.HasChildren then
          SortChildren(Node, Recurse);
        Node := ParentNode.GetNextChild(Node);
      end;
    end;
  end;
end; {SortChildren}

function TCustomDriveView.IterateSubTree(var StartNode : TTreeNode;
  CallBackFunc: TCallBackFunc; Recurse: TRecursiveScan;
  ScanStartNode: TScanStartNode; Data: Pointer): Boolean;

  function ScanSubTree(var StartNode: TTreeNode): Boolean;
  var
    Node: TTreeNode;
    NextNode: TTreeNode;
    NodeHasChilds: Boolean;
  begin
    Result := False;
    if not Assigned(StartNode) then Exit;

    Node := StartNode.GetFirstChild;

    while Assigned(Node) and FContinue do
    begin
      NextNode := StartNode.GetNextChild(Node);
      NodeHasChilds := Node.HasChildren;

      if (not FContinue) or (not CallBackFunc(Node, Data)) then Exit;

      if Assigned(Node) and ((Recurse = rsRecursive) or
         ((Recurse = rsRecursiveExpanded) and Node.Expanded) or
         ((Recurse = rsRecursiveExisting) and NodeHasChilds)) then
      begin
        if (not ScanSubTree(Node)) or (not FContinue) then Exit;
      end;

      Node := NextNode;
    end;
    Result := True;
  end; {ScanSubTree}

begin {IterateSubTree}
  Result := False;
  FContinue := True;
  if not Assigned(CallBackFunc) then Exit;

  if ScanStartNode = coScanStartNode then
    CallBackFunc(StartNode, Data);

  if Assigned(StartNode) then
    if (not FContinue) or (not ScanSubTree(StartNode)) then Exit;

  Result := True;
end; {IterateSubTree}

procedure TCustomDriveView.ClearDragFileList(FileList: TFileList);
begin
  FileList.Clear;
end;

procedure TCustomDriveView.AddToDragFileList(FileList: TFileList; Node: TTreeNode);
begin
  FileList.AddItem(nil, NodePathName(Node));
end;

function TCustomDriveView.NodeCanDrag(Node: TTreeNode): Boolean;
begin
  Result := True;
end;

function TCustomDriveView.NodeOverlayIndexes(Node: TTreeNode): Word;
begin
  Result := oiNoOverlay;
end;

function TCustomDriveView.NodeIsRecycleBin(Node: TTreeNode): Boolean;
begin
  Result := False;
end;

function TCustomDriveView.NodePathExists(Node: TTreeNode): Boolean;
begin
  Result := True;
end;

procedure TCustomDriveView.SetDimmHiddenDirs(Value: Boolean);
begin
  if Value <> FDimmHiddenDirs then
  begin
    FDimmHiddenDirs := Value;
    Self.Invalidate;
  end;
end; {SetDimmHiddenDirs}

procedure TCustomDriveView.SetShowHiddenDirs(Value: Boolean);
begin
  if Value <> FShowHiddenDirs then
  begin
    FShowHiddenDirs := Value;
    RebuildTree;
  end;
end; {SetDimmHiddenDirs}

function TCustomDriveView.GetTargetPopupMenu: Boolean;
begin
  if Assigned(FDragDropFilesEx) then Result := FDragDropFilesEx.TargetPopupMenu
    else Result := True;
end;

procedure TCustomDriveView.SetTargetPopUpMenu(Value: Boolean);
begin
  if Assigned(FDragDropFilesEx) then
    FDragDropFilesEx.TargetPopupMenu := Value;
end; {SetTargetPopUpMenu}

function TCustomDriveView.GetDirectory: string;
begin
  if Assigned(Selected) then Result := NodePathName(Selected)
    else Result := '';
end; {GetDirectory}

procedure TCustomDriveView.SetDirectory(Value: string);
var
  NewSelected: TTreeNode;
  Rect: TRect;
begin
  NewSelected := FindPathNode(Value);

  if Assigned(NewSelected) and (NewSelected <> Selected) then
  begin
    FCanChange := True;
    NewSelected.MakeVisible;
    Rect := NewSelected.DisplayRect(False);

    Selected := NewSelected;
  end
    else
  if csDesigning in ComponentState then
    Selected := nil;
end; {SetDirectory}

end.
