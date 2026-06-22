unit CustomDriveView;

interface

uses
  Classes, ComCtrls, CommCtrl, Windows, Controls, Forms, ShlObj, Messages,
    Graphics,
  DragDrop, CustomDirView, IEDriveInfo, DragDropFilesEx, PasTools;

type
  {Types uses by the function IterateSubTree:}
  {TRecursiveScan: determines, wich nodes are scanned by the function IterateSubTree:
  rsNoRecursive:       Scan startnode only.
  rsRecursiveExisting: Scan all subnodes of the startnode but not new created subnodes.}
  TRecursiveScan = (rsNoRecursive, rsRecursiveExisting);

  {TScanStartnode: determines, wether the startnode should also be scanned:}
  TScanStartNode = (coNoScanStartNode, coScanStartNode);

  TCallBackFunc = function(var Node: TTreeNode; Data: Pointer): Boolean of object;

type
  TCustomDriveView = class(TCustomTreeView)
  protected
    FParentForm: TCustomForm;
    FDragFileList: TStringList;
    FDragDropFilesEx: TCustomizableDragDropFilesEx;
    FDragImageList: TDragImageList;
    FDragDrive: string;
    FExeDrag: Boolean;
    FDDLinkOnExeDrag: Boolean;
    FDragNode: TTreeNode;
    FDragStartTime: FILETIME;
    FDragPos: TPoint;
    FStartPos: TPoint;
    FContextMenu: Boolean;
    FCanChange: Boolean;
    FUseSystemContextMenu: Boolean;
    FDimmHiddenDirs: Boolean;
    FShowHiddenDirs: Boolean;
    FNaturalOrderNumericalSorting: Boolean;
    FDarkMode: Boolean;
    FImageList: TImageList;
    FScrollOnDragOver: TTreeViewScrollOnDragOver;
    FRecreatingHandle: Boolean;

    FOnDDDragEnter: TDDOnDragEnter;
    FOnDDDragLeave: TDDOnDragLeave;
    FOnDDDragOver: TDDOnDragOver;
    FOnDDDrop: TDDOnDrop;
    FOnDDQueryContinueDrag: TDDOnQueryContinueDrag;
    FOnDDChooseEffect: TDDOnChooseEffect;
    FOnDDGiveFeedback: TDDOnGiveFeedback;
    FOnDDDragDetect: TDDOnDragDetect;
    FOnDDProcessDropped: TOnProcessDropped;
    FOnDDError: TDDErrorEvent;
    FOnDDExecuted: TDDExecutedEvent;
    FOnDDFileOperation: TDDFileOperationEvent;
    FOnDDFileOperationExecuted: TDDFileOperationExecutedEvent;
    FOnDDCreateDragFileList: TDDOnCreateDragFileList;
    FOnDDEnd: TNotifyEvent;
    FOnDDCreateDataObject: TDDOnCreateDataObject;
    FLastDDResult: TDragResult;
    FOnBusy: TDirViewBusy;

    function GetTargetPopupMenu: Boolean;
    procedure SetTargetPopUpMenu(Value: Boolean);
    procedure SetDimmHiddenDirs(Value: Boolean);
    procedure SetShowHiddenDirs(Value: Boolean);
    procedure SetNaturalOrderNumericalSorting(Value: Boolean);
    procedure SetDarkMode(Value: Boolean);

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
    procedure NeedImageLists;
    procedure DoCompare(Sender: TObject; Node1, Node2: TTreeNode; Data: Integer; var Compare: Integer);
    function DoCompareText(Text1, Text2: string): Integer;
    procedure UpdateItemHeight;

    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;
    procedure CMColorChanged(var Msg: TMessage); message CM_COLORCHANGED;
    procedure CMRecreateWnd(var Msg: TMessage); message CM_RECREATEWND;
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg: TWMLButtonDown); message WM_LBUTTONUP;
    procedure WMRButtonDown(var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMContextMenu(var Msg: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure CMDPIChanged(var Message: TMessage); message CM_DPICHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;

    procedure Delete(Node: TTreeNode); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    procedure InternalOnDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);

    procedure DDDragEnter(DataObj: IDataObject; KeyState: Longint;
      Point: TPoint; var Effect: Longint; var Accept: Boolean);
    procedure DDDragLeave(Dummy: Integer);
    procedure DDDragOver(KeyState: Longint; Point: TPoint; var Effect: Longint; PreferredEffect: LongInt);
    procedure DDDrop(DataObj: IDataObject; KeyState: Longint; Point: TPoint;
      var Effect: Longint);
    procedure DDQueryContinueDrag(EscapePressed: BOOL; KeyState: Longint;
      var Result: HResult);
    procedure DDDropHandlerSucceeded(Sender: TObject; KeyState: Longint;
      Point: TPoint; Effect: Longint);
    procedure DDGiveFeedback(Effect: Longint; var Result: HResult);
    procedure DDProcessDropped(Sender: TObject; KeyState: Longint;
      Point: TPoint; Effect: Longint);
    procedure DDError(Error: TDDError); virtual;
    procedure DDSpecifyDropTarget(Sender: TObject; DragDropHandler: Boolean;
      Point: TPoint; var PIDL: PItemIDList; var Filename: string);
    procedure DDDragDetect(KeyState: Longint; DetectStart, Point: TPoint;
      DragStatus: TDragDetectStatus); virtual;
    procedure PerformDragDropFileOperation(Node: TTreeNode; Effect: Integer); virtual; abstract;

    procedure DDChooseEffect(KeyState: Integer; var Effect: Integer; PreferredEffect: Integer); virtual;
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
    procedure RebuildTree; virtual; abstract;

    procedure DisplayContextMenu(Node: TTreeNode; ScreenPos: TPoint); virtual; abstract;
    procedure DisplayPropertiesMenu(Node: TTreeNode); virtual; abstract;

    procedure ScrollOnDragOverBeforeUpdate(ObjectToValidate: TObject);
    procedure ScrollOnDragOverAfterUpdate;

    function DoBusy(Busy: Integer): Boolean;
    function StartBusy: Boolean;
    procedure EndBusy;
    function IsBusy: Boolean;

    property ImageList: TImageList read FImageList;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ValidateDirectory(Node: TTreeNode);
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
    property NaturalOrderNumericalSorting: Boolean read FNaturalOrderNumericalSorting write SetNaturalOrderNumericalSorting;
    property DarkMode: Boolean read FDarkMode write SetDarkMode;

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
    {An error has occurred during a drag&drop operation:}
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

    property OnBusy: TDirViewBusy read FOnBusy write FOnBusy;

    { Show popupmenu when dropping a file with the right mouse button }
    property TargetPopUpMenu: Boolean read GetTargetPopUpMenu write SetTargetPopUpMenu default True;

    {Current selected directory:}
    property Directory: string read GetDirectory write SetDirectory;
    property DragNode: TTreeNode read FDragNode;

    property LastDDResult: TDragResult read FLastDDResult;
  end;

implementation

uses
  SysUtils, ShellApi, ImgList, ActiveX, Math,
  IEListView, BaseUtils;

constructor TCustomDriveView.Create(AOwner: TComponent);
begin
  inherited;

  DragMode := dmAutomatic;
  FDragFileList := TStringList.Create;
  FDragDrive := '';
  FExeDrag := False;
  FDDLinkOnExeDrag := True;
  FContextMenu := False;
  FCanChange := True;
  FUseSystemContextMenu := True;
  FNaturalOrderNumericalSorting := True;
  FDarkMode := False;
  FRecreatingHandle := False;
  OnCompare := DoCompare;

  FDragDropFilesEx := TCustomizableDragDropFilesEx.Create(Self);
  with FDragDropFilesEx do
  begin
    AcceptOwnDnd := True;
    {MP}
    AutoDetectDnD := False;
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
    OnDropHandlerSucceeded := DDDropHandlerSucceeded;
    OnGiveFeedback := DDGiveFeedback;
    OnProcessDropped := DDProcessDropped;
    OnDragDetect := DDDragDetect;
  end;

  OnCustomDrawItem := InternalOnDrawItem;

  FScrollOnDragOver := TTreeViewScrollOnDragOver.Create(Self, False);
  FScrollOnDragOver.OnBeforeUpdate := ScrollOnDragOverBeforeUpdate;
  FScrollOnDragOver.OnAfterUpdate := ScrollOnDragOverAfterUpdate;
end;

destructor TCustomDriveView.Destroy;
begin
  FreeAndNil(FScrollOnDragOver);
  FreeAndNil(FImageList);

  if Assigned(Images) then
    Images.Free;

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

procedure TCustomDriveView.UpdateItemHeight;
var
  ImageHeight: Integer;
  TextHeight: Integer;
begin
  // Particularly when called from CMFontChanged because we are changing (reverting)
  // the default form font, the Images might not be set up yet
  if Assigned(Images) then
    ImageHeight := (Images.Width * 9) div 8
  else
    ImageHeight := 0;
  // 16 seems to be the system default tree view item height
  TextHeight := ScaleByControlTextHeightRunTime(Canvas, 16);
  TreeView_SetItemHeight(Handle, Max(ImageHeight, TextHeight));
end;

procedure TCustomDriveView.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateItemHeight;
end;

procedure TCustomDriveView.NeedImageLists;
var
  AImages: TImageList;
begin
  if not Assigned(Images) then
  begin
    Images := TImageList.Create(Self);
  end;

  AImages := ShellImageListForControl(Self, ilsSmall);
  if Images.Handle <> AImages.Handle then
  begin
    // When assigned directly (as in TCustomDirView), when moving from low to high DPI display,
    // the images are resized vertically two times (thoguh originally, this approach was likely taken
    // for different reasons)
    Images.Handle := AImages.Handle;
    Images.ShareImages := AImages.ShareImages;
    Images.DrawingStyle := AImages.DrawingStyle;

    if Assigned(FImageList) then
      FImageList.Free;

    FImageList := OverlayImageList(Images.Width);
  end;

  UpdateItemHeight;
end;

procedure TCustomDriveView.CMDPIChanged(var Message: TMessage);
begin
  inherited;
  NeedImageLists;
end;

procedure TCustomDriveView.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  // WORKAROUND
  // The Indent seems to be scaled by Windows.
  // The TCustomTreeView.ChangeScale redundantly scales it again when Images.IsScaled
  // (and we need Images.IsScaled, otherwise TCustomTreeView enables DPI [pixel] scaling)
  // But we cannot just revert the scaling, because it is needed when DPI changes on runtime.
  // (strangelly for plain tree view [e.g. navigation tree on preferences dialog, it works correctly,
  // so it seems that Windows scales Ident on runtime only when the tree have images -
  // what is confirmed by double scaling on Login dialog - so there we should do the same trick)
  Indent := ScaleByCurrentPPI(19, Self);
end;

procedure TCustomDriveView.CreateWnd;
begin
  inherited;

  if DarkMode then AllowDarkModeForWindow(Self, DarkMode);

  NeedImageLists;

  if not (csDesigning in ComponentState) then
    FDragImageList := TDragImageList.Create(Self);
  if not Assigned(GlobalDragImageList) then
    GlobalDragImageList := FDragImageList;

  FDragDropFilesEx.DragDropControl := Self;
  FParentForm := GetParentForm(Self);

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
    end
      else
    if Node.Selected and (not Self.Focused) and DarkMode then
    begin
      Canvas.Font.Color := Font.Color;
    end;
  end;
end; {InternalOnDrawItem}

procedure TCustomDriveView.ScrollOnDragOverBeforeUpdate(ObjectToValidate: TObject);
var
  NodeToValidate: TTreeNode;
begin
  GlobalDragImageList.HideDragImage;
  if Assigned(ObjectToValidate) then
  begin
    NodeToValidate := (ObjectToValidate as TTreeNode);
    if not NodeToValidate.HasChildren then
      ValidateDirectory(NodeToValidate);
  end;
end;

procedure TCustomDriveView.ScrollOnDragOverAfterUpdate;
begin
  GlobalDragImageList.ShowDragImage;
end;

procedure TCustomDriveView.DDDragEnter(DataObj: IDataObject; KeyState: Longint;
  Point: TPoint; var Effect: Longint; var Accept: Boolean);
var
  Index: Integer;
begin
  if (FDragDropFilesEx.FileList.Count > 0) and
     (Length(TFDDListItem(FDragDropFilesEx.FileList[0]^).Name) > 0) Then
  begin
    try
      FDragDrive := DriveInfo.GetDriveKey(TFDDListItem(FDragDropFilesEx.FileList[0]^).Name);
    except
      // WinRAR gives us only filename on "enter", we get a full path only on "drop".
      FDragDrive := '';
    end;
    FExeDrag := FDDLinkOnExeDrag and
      (deLink in DragDropFilesEx.TargetEffects) and
      ((DragDropFilesEx.AvailableDropEffects and DROPEFFECT_LINK) <> 0);

    if FExeDrag then
    begin
      for Index := 0 to FDragDropFilesEx.FileList.Count - 1 do
        if not IsExecutable(TFDDListItem(FDragDropFilesEx.FileList[Index]^).Name) then
        begin
          FExeDrag := False;
          Break;
        end;
    end;
  end
    else
  begin
    FDragDrive := '';
  end;

  FScrollOnDragOver.StartDrag;

  if Assigned(FOnDDDragEnter) then
    FOnDDDragEnter(Self, DataObj, KeyState, Point, Effect, Accept);
end; {DDDragEnter}

procedure TCustomDriveView.DDDragLeave(Dummy: Integer);
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

procedure TCustomDriveView.DDDragOver(KeyState: Longint; Point: TPoint; var Effect: Longint; PreferredEffect: Longint);
var
  Node: TTreeNode;
  Rect1: TRect;
  UpdateImage: Boolean;
  LastDragNode: TTreeNode;
begin
  if Effect <> DROPEFFECT_NONE then
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
         (Effect <> DROPEFFECT_LINK) and
         ((Node = FDragNode) or Node.HasAsParent(FDragNode) or (FDragNode.Parent = Node)) then
         Effect := DROPEFFECT_NONE;

      FScrollOnDragOver.DragOver(Point);
    end {Assigned(Node)}
      else
    begin
      DropTarget := nil;
    end;
  end;

  DDChooseEffect(KeyState, Effect, PreferredEffect);

  if Assigned(FOnDDDragOver) then
    FOnDDDragOver(Self, KeyState, Point, Effect);

  if not Assigned(DropTarget) then Effect := DROPEFFECT_NONE
    else
  if NodeIsRecycleBin(DropTarget) then
  begin
    if FDragDropFilesEx.FileNamesAreMapped then Effect := DROPEFFECT_NONE
      else Effect := DROPEFFECT_MOVE;
  end;
end; {DDDragOver}

procedure TCustomDriveView.DDDrop(DataObj: IDataObject; KeyState: Longint;
  Point: TPoint; var Effect: Longint);
begin
  if GlobalDragImageList.Dragging then
    GlobalDragImageList.HideDragImage;

  if Effect = DROPEFFECT_NONE then
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

procedure TCustomDriveView.DDDropHandlerSucceeded(Sender: TObject;
  KeyState: Integer; Point: TPoint; Effect: Integer);
begin
  DropTarget := nil;
end;

procedure TCustomDriveView.DDChooseEffect(KeyState: Integer; var Effect: Integer; PreferredEffect: Integer);
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
        if Effect <> DROPEFFECT_NONE then
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

    FDragDrive := '';

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
      if not AvoidDragImage then
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

      FDragDrive := '';
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

  Result := FDragDropFilesEx.Execute(DataObject);
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

  if Result and (Stage = cdPostPaint) then
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

procedure TCustomDriveView.CMColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(Images) then
    Images.BkColor := Color;
  ForceColorChange(Self);
end;

procedure TCustomDriveView.WMLButtonDown(var Msg: TWMLButtonDown);
begin
  if not IsBusy then
  begin
    FCanChange := False;
    GetCursorPos(FStartPos);
    inherited;
  end;
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
  if not IsBusy then
  begin
    GetCursorPos(FStartPos);
    if FDragDropFilesEx.DragDetectStatus <> ddsDrag then
      FContextMenu := True;
    inherited;
  end;
end; {WMRButtonDown}

procedure TCustomDriveView.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if not IsBusy then
  begin
    inherited;
  end;
end;

procedure TCustomDriveView.WMContextMenu(var Msg: TWMContextMenu);
var
  Node: TTreeNode;
  Point: TPoint;
  PrevAutoPopup: Boolean;
begin
  // Not sure what is this exactly for, as without AutoPopup, the inherited WMContextMenu is almost noop.
  // In general it would be better to override DoContextPopup
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

// Might not be always called, see comment in TDriveView.DestroyWnd
procedure TCustomDriveView.CMRecreateWnd(var Msg: TMessage);
var
  HadHandle: Boolean;
begin
  HadHandle := HandleAllocated;
  inherited;
  // If the control is not showing (e.g. because the machine is locked), the handle is not recreated.
  // If contents is reloaded (LoadPath) without handle allocated, it crashes
  // (as the handle is implicitly created somewhere in the middle of the reload and chaos ensures).
  if HadHandle then
  begin
    Assert(not FRecreatingHandle);
    FRecreatingHandle := True;
    try
      HandleNeeded;
    finally
      FRecreatingHandle := False;
    end;
  end;
end;

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

procedure TCustomDriveView.WMKeyDown(var Message: TWMKeyDown);
begin
  if not IsBusy then
  begin
    inherited;
  end;
end;

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

function TCustomDriveView.DoCompareText(Text1, Text2: string): Integer;
begin
  Result := CompareLogicalTextPas(Text1, Text2, NaturalOrderNumericalSorting);
end;

procedure TCustomDriveView.DoCompare(Sender: TObject; Node1, Node2: TTreeNode; Data: Integer; var Compare: Integer);
begin
  Compare := DoCompareText(Node1.Text, Node2.Text);
end;

function TCustomDriveView.SortChildren(ParentNode: TTreeNode; Recurse: Boolean): Boolean;
begin
  Result := Assigned(ParentNode) and ParentNode.AlphaSort(Recurse);
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

    while Assigned(Node) do
    begin
      NextNode := StartNode.GetNextChild(Node);
      NodeHasChilds := Node.HasChildren;

      if not CallBackFunc(Node, Data) then Exit;

      if Assigned(Node) and
         (Recurse = rsRecursiveExisting) and NodeHasChilds then
      begin
        if not ScanSubTree(Node) then Exit;
      end;

      Node := NextNode;
    end;
    Result := True;
  end; {ScanSubTree}

begin {IterateSubTree}
  Result := False;
  if Assigned(CallBackFunc) then
  begin
    if ScanStartNode = coScanStartNode then
    begin
      CallBackFunc(StartNode, Data);
    end;

    if (not Assigned(StartNode)) or
       ScanSubTree(StartNode) then
    begin
      Result := True;
    end;
  end;
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

procedure TCustomDriveView.SetNaturalOrderNumericalSorting(Value: Boolean);
begin
  if NaturalOrderNumericalSorting <> Value then
  begin
    FNaturalOrderNumericalSorting := Value;
    AlphaSort;
  end;
end;

procedure TCustomDriveView.SetDarkMode(Value: Boolean);
begin
  if DarkMode <> Value then
  begin
    FDarkMode := Value;
    // See TCustomDirView.SetDarkMode
    if HandleAllocated then AllowDarkModeForWindow(Self, DarkMode);
  end;
end;

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
begin
  NewSelected := FindPathNode(Value);

  if Assigned(NewSelected) and (NewSelected <> Selected) then
  begin
    FCanChange := True;
    NewSelected.MakeVisible;

    Selected := NewSelected;
  end
    else
  if csDesigning in ComponentState then
    Selected := nil;
end; {SetDirectory}

function TCustomDriveView.DoBusy(Busy: Integer): Boolean;
begin
  Result := True;
  if Assigned(OnBusy) then
  begin
    OnBusy(Self, Busy, Result);
  end;
end;

function TCustomDriveView.StartBusy: Boolean;
begin
  Result := DoBusy(1);
end;

function TCustomDriveView.IsBusy: Boolean;
begin
  Result := DoBusy(0);
end;

procedure TCustomDriveView.EndBusy;
begin
  DoBusy(-1);
end;

end.
