unit PasTools;

interface

uses
  Windows, Types, Classes, ComCtrls, ExtCtrls, Controls, Dialogs, Forms;

function Construct(ComponentClass: TComponentClass; Owner: TComponent): TComponent;

function IsVistaHard: Boolean;

function IsVista: Boolean;

function IsWin7: Boolean;

function IsWin8: Boolean;

function CutToChar(var Str: string; Ch: Char; Trim: Boolean): string;

procedure FilterToFileTypes(Filter: string; FileTypes: TFileTypeItems);

// Note that while we based our scaling on pixels-per-inch,
// VCL actually scales based on font size
function LoadDimension(Dimension: Integer; PixelsPerInch: Integer): Integer;
function StrToDimensionDef(Str: string; PixelsPerInch: Integer; Default: Integer): Integer;
function SaveDimension(Dimension: Integer): Integer;
function DimensionToDefaultPixelsPerInch(Dimension: Integer): Integer;

function LoadPixelsPerInch(S: string): Integer;
function SavePixelsPerInch: string;
function SaveDefaultPixelsPerInch: string;

function ScaleByTextHeight(Control: TControl; Dimension: Integer): Integer;
function ScaleByTextHeightRunTime(Control: TControl; Dimension: Integer): Integer;

function ControlHasRecreationPersistenceData(Control: TControl): Boolean;

function IsAppIconic: Boolean;
procedure SetAppIconic(Value: Boolean);

type
  TControlScrollBeforeUpdate = procedure(ObjectToValidate: TObject) of object;
  TControlScrollAfterUpdate = procedure of object;

  TCustomControlScrollOnDragOver = class
  private
    FOnBeforeUpdate: TControlScrollBeforeUpdate;
    FOnAfterUpdate: TControlScrollAfterUpdate;
    FDragOverTimer: TTimer;
    FControl: TControl;
    FDragOverTime: FILETIME;
    FLastVScrollTime: FILETIME;
    FVScrollCount: Integer;

    procedure DragOverTimer(Sender: TObject);
    procedure BeforeUpdate(ObjectToValidate: TObject);
    procedure AfterUpdate;

  public
    constructor Create(Control: TControl; ScheduleDragOver: Boolean);
    destructor Destroy; override;
    procedure StartDrag; virtual;
    procedure EndDrag; virtual;
    procedure DragOver(Point: TPoint); virtual; abstract;

    property OnBeforeUpdate: TControlScrollBeforeUpdate read FOnBeforeUpdate write FOnBeforeUpdate;
    property OnAfterUpdate: TControlScrollAfterUpdate read FOnAfterUpdate write FOnAfterUpdate;
  end;

  TTreeViewScrollOnDragOver = class(TCustomControlScrollOnDragOver)
  private
    FLastDragNode: TTreeNode;
    FLastHScrollTime: FILETIME;
  public
    procedure StartDrag; override;
    procedure DragOver(Point: TPoint); override;
  end;

  TListViewScrollOnDragOver = class(TCustomControlScrollOnDragOver)
  public
    procedure DragOver(Point: TPoint); override;
  end;

  TListBoxScrollOnDragOver = class(TCustomControlScrollOnDragOver)
  public
    procedure DragOver(Point: TPoint); override;
  end;

implementation

uses
  SysUtils, Messages, StdCtrls;

const
  DDExpandDelay = 15000000;
  DDMaxSlowCount = 3;
  DDVScrollDelay   = 2000000;
  DDHScrollDelay   = 2000000;
  DDDragStartDelay = 500000;

function Construct(ComponentClass: TComponentClass; Owner: TComponent): TComponent;
begin
  Result := ComponentClass.Create(Owner);
end;

// detects vista, even in compatibility mode
// (GetLocaleInfoEx is available since Vista only)
function IsVistaHard: Boolean;
begin
  Result := (GetProcAddress(GetModuleHandle(Kernel32), 'GetLocaleInfoEx') <> nil);
end;

function IsVista: Boolean;
begin
  Result := CheckWin32Version(6, 0);
end;

function IsWin7: Boolean;
begin
  Result := CheckWin32Version(6, 1);
end;

function IsWin8: Boolean;
begin
  Result := CheckWin32Version(6, 2);
end;

function CutToChar(var Str: string; Ch: Char; Trim: Boolean): string;
var
  P: Integer;
begin
  P := Pos(Ch, Str);
  if P > 0 then
  begin
    Result := Copy(Str, 1, P-1);
    Delete(Str, 1, P);
  end
    else
  begin
    Result := Str;
    Str := '';
  end;
  if Trim then Result := SysUtils.Trim(Result);
end;

procedure FilterToFileTypes(Filter: string; FileTypes: TFileTypeItems);
var
  Item: TFileTypeItem;
begin
  while Filter <> '' do
  begin
    Item := FileTypes.Add();
    Item.DisplayName := CutToChar(Filter, '|', True);
    Item.FileMask := CutToChar(Filter, '|', True);
  end;
end;

function LoadDimension(Dimension: Integer; PixelsPerInch: Integer): Integer;
begin
  Result := MulDiv(Dimension, Screen.PixelsPerInch, PixelsPerInch);
end;

function StrToDimensionDef(Str: string; PixelsPerInch: Integer; Default: Integer): Integer;
begin
  if TryStrToInt(Str, Result) then
  begin
    Result := LoadDimension(Result, PixelsPerInch);
  end
    else
  begin
    Result := Default;
  end;
end;

function SaveDimension(Dimension: Integer): Integer;
begin
  // noop
  Result := Dimension;
end;

function DimensionToDefaultPixelsPerInch(Dimension: Integer): Integer;
begin
  Result := MulDiv(Dimension, USER_DEFAULT_SCREEN_DPI, Screen.PixelsPerInch);
end;

function LoadPixelsPerInch(S: string): Integer;
begin
  // for backward compatibility with version that did not save the DPI,
  // make reasonable assumption that the configuration was saved with
  // the same DPI as we run now
  Result := StrToIntDef(S, Screen.PixelsPerInch);
end;

function SavePixelsPerInch: string;
begin
  Result := IntToStr(Screen.PixelsPerInch);
end;

function SaveDefaultPixelsPerInch: string;
begin
  Result := IntToStr(USER_DEFAULT_SCREEN_DPI);
end;

// WORKAROUND
// http://stackoverflow.com/questions/9410485/how-do-i-use-class-helpers-to-access-strict-private-members-of-a-class

type
  TFormHelper = class helper for TCustomForm
  public
    function RetrieveTextHeight: Integer;
    function CalculateTextHeight: Integer;
  end;

function TFormHelper.RetrieveTextHeight: Integer;
begin
  Result := Self.FTextHeight;
end;

function TFormHelper.CalculateTextHeight: Integer;
begin
  Result := Self.GetTextHeight;
end;

function ScaleByTextHeightImpl(Control: TControl; Dimension: Integer; TextHeight: Integer): Integer;
var
  Form: TCustomForm;
  NewTextHeight: Integer;
begin
  // RTL_COPY (TCustomForm.ReadState)
  Form := ValidParentForm(Control);
  NewTextHeight := Form.CalculateTextHeight;
  if TextHeight <> NewTextHeight then
  begin
    Dimension := MulDiv(Dimension, NewTextHeight, TextHeight);
  end;
  Result := Dimension;
end;

const
  OurDesignTimeTextHeight = 13;

function ScaleByTextHeight(Control: TControl; Dimension: Integer): Integer;
var
  Form: TCustomForm;
  TextHeight: Integer;
begin
  // RTL_COPY (TCustomForm.ReadState)
  Form := ValidParentForm(Control);
  TextHeight := Form.RetrieveTextHeight;
  // that's our design text-size, we do not expect any other value
  Assert(TextHeight = OurDesignTimeTextHeight);
  Result := ScaleByTextHeightImpl(Control, Dimension, TextHeight);
end;

// this differs from ScaleByTextHeight only by enforcing
// constant design-time text height
function ScaleByTextHeightRunTime(Control: TControl; Dimension: Integer): Integer;
begin
  Result := ScaleByTextHeightImpl(Control, Dimension, OurDesignTimeTextHeight);
end;

type
  TListViewHelper = class helper for TCustomListView
  public
    function HasMemStream: Boolean;
  end;

function TListViewHelper.HasMemStream: Boolean;
begin
  Result := Assigned(Self.FMemStream);
end;

type
  TTreeViewHelper = class helper for TCustomTreeView
  public
    function HasMemStream: Boolean;
  end;

function TTreeViewHelper.HasMemStream: Boolean;
begin
  Result := Assigned(Self.FMemStream);
end;

type
  TRichEditHelper = class helper for TCustomRichEdit
  public
    function HasMemStream: Boolean;
  end;

function TRichEditHelper.HasMemStream: Boolean;
begin
  Result := Assigned(Self.FMemStream);
end;

function ControlHasRecreationPersistenceData(Control: TControl): Boolean;
begin
  // not implemented for this class as we do not use it as of now
  Assert(not (Control is TCustomComboBoxEx));
  Result :=
    ((Control is TCustomListView) and (Control as TCustomListView).HasMemStream) or
    ((Control is TCustomTreeView) and (Control as TCustomTreeView).HasMemStream) or
    ((Control is TCustomRichEdit) and (Control as TCustomRichEdit).HasMemStream);
end;

type
  TApplicationHelper = class helper for TApplication
  public
    function IsAppIconic: Boolean;
    procedure SetAppIconic(Value: Boolean);
  end;

function TApplicationHelper.IsAppIconic: Boolean;
begin
  Result := Self.FAppIconic;
end;

procedure TApplicationHelper.SetAppIconic(Value: Boolean);
begin
  Self.FAppIconic := Value;
end;

function IsAppIconic: Boolean;
begin
  Result := Application.IsAppIconic;
end;

procedure SetAppIconic(Value: Boolean);
begin
  Application.SetAppIconic(Value);
end;


  { TCustomControlScrollOnDragOver }

constructor TCustomControlScrollOnDragOver.Create(Control: TControl;
  ScheduleDragOver: Boolean);
begin
  FControl := Control;
  FOnBeforeUpdate := nil;
  FOnAfterUpdate := nil;

  if ScheduleDragOver then
  begin
    FDragOverTimer := TTimer.Create(Control);
    FDragOverTimer.Enabled := False;
    FDragOverTimer.Interval := 50;
    FDragOverTimer.OnTimer := DragOverTimer;
  end
    else FDragOverTimer := nil;
end;

destructor TCustomControlScrollOnDragOver.Destroy;
begin
  FreeAndNil(FDragOverTimer);
end;

procedure TCustomControlScrollOnDragOver.DragOverTimer(Sender: TObject);
var
  P: TPoint;
begin
  P := FControl.ScreenToClient(Mouse.CursorPos);
  if (P.X >= 0) and (P.X < FControl.Width) and
     (P.Y >= 0) and (P.Y < FControl.Height) then
  begin
    DragOver(P);
  end;
end;

procedure TCustomControlScrollOnDragOver.StartDrag;
begin
  GetSystemTimeAsFileTime(FDragOverTime);
  GetSystemTimeAsFileTime(FLastVScrollTime);
  FVScrollCount := 0;

  if Assigned(FDragOverTimer) then
    FDragOverTimer.Enabled := True;
end;

procedure TCustomControlScrollOnDragOver.EndDrag;
begin
  if Assigned(FDragOverTimer) then
    FDragOverTimer.Enabled := False;
end;

type
  TPublicControl = class(TControl);

procedure TCustomControlScrollOnDragOver.BeforeUpdate(ObjectToValidate: TObject);
var
  DragImages: TDragImageList;
begin
  if Assigned(FOnBeforeUpdate) then
    FOnBeforeUpdate(ObjectToValidate);
  DragImages := TPublicControl(FControl).GetDragImages;
  if Assigned(DragImages) then
    DragImages.HideDragImage;
end;

procedure TCustomControlScrollOnDragOver.AfterUpdate;
var
  DragImages: TDragImageList;
begin
  if Assigned(FOnAfterUpdate) then
    FOnAfterUpdate;
  DragImages := TPublicControl(FControl).GetDragImages;
  if Assigned(DragImages) then
    DragImages.ShowDragImage;
end;

procedure TTreeViewScrollOnDragOver.StartDrag;
var
  KeyBoardState : TKeyBoardState;
begin
  inherited;

  FLastDragNode := nil;

  if (GetKeyState(VK_SPACE) <> 0) and GetKeyboardState(KeyBoardState) then
  begin
    KeyBoardState[VK_SPACE] := 0;
    SetKeyBoardState(KeyBoardState);
  end;

  GetSystemTimeAsFileTime(FLastHScrollTime);
end;

  { TTreeViewScrollOnDragOver }

procedure TTreeViewScrollOnDragOver.DragOver(Point: TPoint);
var
  TreeView: TCustomTreeView;
  NbPixels: Integer;
  KnowTime: TFileTime;
  Node: TTreeNode;
  TempTopItem: TTreeNode;
  ScrollInfo: TScrollInfo;
  KeyBoardState : TKeyBoardState;
begin
  TreeView := (FControl as TCustomTreeView);
  Node := TreeView.GetNodeAt(Point.X, Point.Y);
  if Assigned(Node) then
  begin
    GetSystemTimeAsFileTime(KnowTime);
    if GetKeyState(VK_SPACE) = 0 then
    begin
      {Expand node after 2.5 seconds: }
      if not Assigned(FLastDragNode) or (FLastDragNode <> Node) then
      begin
        {not previous droptarget: start timer}
        GetSystemTimeAsFileTime(FDragOverTime);
        FLastDragNode := Node
      end
        else
      begin
        if ((Int64(KnowTime) - Int64(FDragOverTime)) > DDExpandDelay) then
        begin
          TempTopItem := TreeView.TopItem;
          BeforeUpdate(nil);
          Node.Expand(False);
          TreeView.TopItem := TempTopItem;
          TreeView.Update;
          AfterUpdate;
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

      TempTopItem := TreeView.TopItem;
      BeforeUpdate(Node);
      if Node.Expanded then
      begin
        if not TreeView.Selected.HasAsParent(Node) then
          Node.Collapse(False);
      end
        else Node.Expand(False);
      TreeView.TopItem := TempTopItem;
      TreeView.Update;
      AfterUpdate;
    end;

    NbPixels := Abs(TTreeView(FControl).Font.Height);

    {Vertical treescrolling:}
    if ((Int64(KnowTime) - Int64(FLastVScrollTime)) > DDVScrollDelay) or
       ((FVScrollCount > 3) and
        ((Int64(KnowTime) - Int64(FLastVScrollTime)) > (DDVScrollDelay Div 4))) then
    begin
      {Scroll tree up, if droptarget is topitem:}
      if Node = TreeView.TopItem then
      begin
        BeforeUpdate(nil);
        TreeView.Perform(WM_VSCROLL, SB_LINEUP, 0);
        AfterUpdate;
        GetSystemTimeAsFileTime(FLastVScrollTime);
        Inc(FVScrollCount);
      end
        else
      {Scroll tree down, if next visible item of droptarget is not visible:}
      begin
        if Point.Y + 3 * nbPixels > TreeView.Height then
        begin
          BeforeUpdate(nil);
          TreeView.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
          AfterUpdate;
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
      GetScrollInfo(TreeView.Handle, SB_HORZ, ScrollInfo);
      if ScrollInfo.nMin <> ScrollInfo.nMax then
      begin
        if Point.X < 50 then
        begin
          if Node.DisplayRect(True).Right + 50 < TreeView.Width then
          begin
            BeforeUpdate(nil);
            TreeView.Perform(WM_HSCROLL, SB_LINELEFT, 0);
            AfterUpdate;
          end;
        end
          else
        if Point.X > (TreeView.Width - 50) then
        begin
          if Node.DisplayRect(True).Left > 50 then
          begin
            BeforeUpdate(nil);
            TreeView.Perform(WM_HSCROLL, SB_LINERIGHT, 0);
            AfterUpdate;
          end;
        end;
      end;
    end;
  end;
end;

  { TListViewScrollOnDragOver }

procedure TListViewScrollOnDragOver.DragOver(Point: TPoint);
var
  ListView: TCustomListView;
  KnowTime: TFileTime;
  NbPixels: Integer;
  WParam: LongInt;
begin
  ListView := (FControl as TCustomListView);
  GetSystemTimeAsFileTime(KnowTime);
  NbPixels := Abs(TListView(ListView).Font.Height);
  {Vertical scrolling, if viewstyle = vsReport:}
  if (TListView(ListView).ViewStyle = vsReport) and Assigned(ListView.TopItem) and
     (((Int64(KnowTime) - Int64(FLastVScrollTime)) > DDVScrollDelay) or
      ((FVScrollCount > DDMaxSlowCount) and
        ((Int64(KnowTime) - Int64(FLastVScrollTime)) > (DDVScrollDelay div 4)))) then
  begin
    if (Point.Y - 3 * nbPixels <= 0) and (ListView.TopItem.Index > 0) then WParam := SB_LINEUP
      else
    if (Point.Y + 3 * nbPixels > ListView.Height) then WParam := SB_LINEDOWN
      else WParam := -1;
    if WParam >= 0 then
    begin
      BeforeUpdate(nil);
      ListView.Perform(WM_VSCROLL, WParam, 0);
      if FVScrollCount > DDMaxSlowCount then
        ListView.Perform(WM_VSCROLL, WParam, 0);
      if FVScrollCount > DDMaxSlowCount * 3 then
        ListView.Perform(WM_VSCROLL, WParam, 0);
      ListView.Update;
      AfterUpdate;

      GetSystemTimeAsFileTime(FLastVScrollTime);
      Inc(FVScrollCount);
    end
      else FVScrollCount := 0;
  end;
end;

  { TListBoxScrollOnDragOver }

procedure TListBoxScrollOnDragOver.DragOver(Point: TPoint);
var
  ListBox: TListBox;
  KnowTime: TFileTime;
  NbPixels: Integer;
  WParam: LongInt;
begin
  ListBox := (FControl as TListBox);
  GetSystemTimeAsFileTime(KnowTime);
  NbPixels := Abs(ListBox.Font.Height);
  {Vertical scrolling, if viewstyle = vsReport:}
  if (ListBox.Items.Count > 0) and
     (((Int64(KnowTime) - Int64(FLastVScrollTime)) > DDVScrollDelay) or
      ((FVScrollCount > DDMaxSlowCount) and
        ((Int64(KnowTime) - Int64(FLastVScrollTime)) > (DDVScrollDelay div 4)))) then
  begin
    if (Point.Y - 3 * nbPixels <= 0) and (ListBox.TopIndex > 0) then WParam := SB_LINEUP
      else
    if (Point.Y + 3 * nbPixels > ListBox.Height) then WParam := SB_LINEDOWN
      else WParam := -1;
    if WParam >= 0 then
    begin
      BeforeUpdate(nil);
      ListBox.Perform(WM_VSCROLL, WParam, 0);
      if FVScrollCount > DDMaxSlowCount then
        ListBox.Perform(WM_VSCROLL, WParam, 0);
      if FVScrollCount > DDMaxSlowCount * 3 then
        ListBox.Perform(WM_VSCROLL, WParam, 0);
      ListBox.Update;
      AfterUpdate;

      GetSystemTimeAsFileTime(FLastVScrollTime);
      Inc(FVScrollCount);
    end
      else FVScrollCount := 0;
  end;
end;

end.
