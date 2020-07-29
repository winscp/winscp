unit PasTools;

interface

uses
  Windows, Types, Classes, ComCtrls, ExtCtrls, Controls, Dialogs, Forms, Messages;

function Construct(ComponentClass: TComponentClass; Owner: TComponent): TComponent;

function IsVistaHard: Boolean;

function IsVista: Boolean;

// Prevent name conflict with C++ IsWin8.
{$HPPEMIT '#define IsWin7 IsWin7Pas'}
{$HPPEMIT END '#undef IsWin7'}
function IsWin7: Boolean;

// Prevent name conflict with C++ IsWin8.
{$HPPEMIT '#define IsWin8 IsWin8Pas'}
{$HPPEMIT END '#undef IsWin8'}
function IsWin8: Boolean;

// Prevent name conflict with C++ CutToChar.
{$HPPEMIT '#define CutToChar CutToCharPas'}
{$HPPEMIT END '#undef CutToChar'}
function CutToChar(var Str: string; Ch: Char; Trim: Boolean): string;

procedure FilterToFileTypes(Filter: string; FileTypes: TFileTypeItems);

// Note that while we based our scaling on pixels-per-inch,
// VCL actually scales based on font size

const
  CM_DPICHANGED = WM_USER + $2000 + 10;
  WM_DPICHANGED_BEFOREPARENT = $02E2;
  WM_DPICHANGED_AFTERPARENT = $02E3;

function HasSystemParametersInfoForPixelsPerInch: Boolean;
function SystemParametersInfoForPixelsPerInch(
  uiAction, uiParam: UINT; pvParam: Pointer; fWinIni: UINT; dpi: UINT): BOOL;

procedure GetFormScaleRatio(Form: TForm; var M, D: Integer);

function GetMonitorFromControl(Control: TControl): TMonitor;
function GetMonitorPixelsPerInch(Monitor: TMonitor): Integer;
function GetControlPixelsPerInch(Control: TControl): Integer;
function GetComponentPixelsPerInch(Component: TComponent): Integer;
function LoadDimension(Dimension: Integer; PixelsPerInch: Integer; Control: TControl): Integer;
function StrToDimensionDef(Str: string; PixelsPerInch: Integer; Control: TControl; Default: Integer): Integer;
function SaveDimension(Dimension: Integer): Integer;
function DimensionToDefaultPixelsPerInch(Dimension: Integer): Integer;
function ScaleByPixelsPerInch(Dimension: Integer; Monitor: TMonitor): Integer; overload;
function ScaleByPixelsPerInch(Dimension: Integer; Control: TControl): Integer; overload;
function ScaleByPixelsPerInchFromSystem(Dimension: Integer; Control: TControl): Integer;

function LoadPixelsPerInch(S: string; Control: TControl): Integer;
function SavePixelsPerInch(Control: TControl): string;
function SaveDefaultPixelsPerInch: string;

function ScaleByTextHeight(Control: TControl; Dimension: Integer): Integer;
function ScaleByTextHeightRunTime(Control: TControl; Dimension: Integer): Integer;

function GetSystemMetricsForControl(Control: TControl; nIndex: Integer): Integer;

type
  TImageListSize = (ilsSmall, ilsLarge);

procedure NeedShellImageLists;
function ShellImageListForControl(Control: TControl; Size: TImageListSize): TImageList;

function ControlHasRecreationPersistenceData(Control: TControl): Boolean;

function IsAppIconic: Boolean;
procedure SetAppIconic(Value: Boolean);
procedure SetAppMainForm(Value: TForm);
procedure SetAppTerminated(Value: Boolean);

procedure ForceColorChange(Control: TWinControl);

function IsUncPath(Path: string): Boolean;

function SupportsDarkMode: Boolean;
procedure AllowDarkModeForWindow(Control: TWinControl; Allow: Boolean); overload;
procedure AllowDarkModeForWindow(Handle: THandle; Allow: Boolean); overload;
procedure RefreshColorMode;
procedure ResetSysDarkTheme;
function GetSysDarkTheme: Boolean;

type
  TApiPathEvent = function(Path: string): string;

var
  OnApiPath: TApiPathEvent = nil;

// Prevent name conflict with C++ ApiPath.
// We would not want to call this implementation in any case anyway.
{$HPPEMIT '#define ApiPath ApiPathPas'}
{$HPPEMIT END '#undef ApiPath'}
function ApiPath(Path: string): string;

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
  SysUtils, StdCtrls, Graphics, MultiMon, ShellAPI, Generics.Collections, CommCtrl, ImgList, Registry;

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

type
  TGetDpiForMonitorFunc =
    function (hMonitor: HMONITOR; MonitorType: Integer; out DpiX, DpiY: Cardinal): HRESULT; stdcall;
  TGetSystemMetricsForDpiFunc =
    function (nIndex: Integer; Dpi: Cardinal): Integer; stdcall;
  TSystemParametersInfoForDpiFunc =
    function (uiAction, uiParam: UINT; pvParam: Pointer; fWinIni: UINT; dpi: UINT): BOOL; stdcall;

const
  MDT_EFFECTIVE_DPI = 0;

var
  GetDpiForMonitor: TGetDpiForMonitorFunc = nil;
  GetSystemMetricsForDpi: TGetSystemMetricsForDpiFunc = nil;
  SystemParametersInfoForDpi: TSystemParametersInfoForDpiFunc = nil;

function HasSystemParametersInfoForPixelsPerInch: Boolean;
begin
  Result := Assigned(SystemParametersInfoForDpi);
end;

function SystemParametersInfoForPixelsPerInch(
  uiAction, uiParam: UINT; pvParam: Pointer; fWinIni: UINT; dpi: UINT): BOOL;
begin
  if HasSystemParametersInfoForPixelsPerInch then
  begin
    Result := SystemParametersInfoForDpi(uiAction, uiParam, pvParam, fWinIni, dpi);
  end
    else
  begin
    Result := SystemParametersInfo(uiAction, uiParam, pvParam, fWinIni);
  end;
end;

function GetMonitorPixelsPerInch(Monitor: TMonitor): Integer;
var
  DpiX, DpiY: Cardinal;
begin
  if Assigned(GetDpiForMonitor) and
     (GetDpiForMonitor(Monitor.Handle, MDT_EFFECTIVE_DPI, DpiX, DpiY) = S_OK) then
  begin
    Result := DpiX;
  end
    else
  begin
    Result := Screen.PixelsPerInch;
  end;
end;

function GetMonitorFromControl(Control: TControl): TMonitor;
begin
  if Control.Parent <> nil then
  begin
    Result := GetMonitorFromControl(Control.Parent);
  end
    else
  if Control is TCustomForm then
  begin
    Result := TCustomForm(Control).Monitor;
  end
    else
  if (Control is TWinControl) and TWinControl(Control).HandleAllocated then
  begin
    Result := Screen.MonitorFromWindow(TWinControl(Control).Handle);
  end
    else
  begin
    Result := nil;
  end;
end;

function GetControlPixelsPerInch(Control: TControl): Integer;
var
  Form: TCustomForm;
  Monitor: TMonitor;
begin
  if Assigned(GetDpiForMonitor) then // optimization
  begin
    Form := GetParentForm(Control);
    if Assigned(Form) then
    begin
      // By default, scale according to what the form is so far rendered on.
      // If the monitor perceived DPI does not match its monitor DPI, it's because the WM_DPICHANGED is still pending.
      Result := TForm(Form).PixelsPerInch;
    end
      else
    begin
      Monitor := GetMonitorFromControl(Control);
      if Monitor = nil then
      begin
        Assert(False);
        Monitor := Screen.PrimaryMonitor;
      end;
      Result := GetMonitorPixelsPerInch(Monitor);
    end;
  end
    else
  begin
    Result := Screen.PixelsPerInch;
  end;
end;

function GetComponentPixelsPerInch(Component: TComponent): Integer;
begin
  Result := GetControlPixelsPerInch(TControl(Component.Owner));
end;

function LoadDimension(Dimension: Integer; PixelsPerInch: Integer; Control: TControl): Integer;
begin
  Result := MulDiv(Dimension, GetControlPixelsPerInch(Control), PixelsPerInch);
end;

function StrToDimensionDef(Str: string; PixelsPerInch: Integer; Control: TControl; Default: Integer): Integer;
begin
  if TryStrToInt(Str, Result) then
  begin
    Result := LoadDimension(Result, PixelsPerInch, Control);
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

function ScaleByPixelsPerInch(Dimension: Integer; Monitor: TMonitor): Integer;
begin
  Result := MulDiv(Dimension, GetMonitorPixelsPerInch(Monitor), USER_DEFAULT_SCREEN_DPI);
end;

function ScaleByPixelsPerInch(Dimension: Integer; Control: TControl): Integer;
begin
  Result := MulDiv(Dimension, GetControlPixelsPerInch(Control), USER_DEFAULT_SCREEN_DPI);
end;

function ScaleByPixelsPerInchFromSystem(Dimension: Integer; Control: TControl): Integer;
begin
  Result := MulDiv(Dimension, GetControlPixelsPerInch(Control), Screen.PixelsPerInch);
end;

function LoadPixelsPerInch(S: string; Control: TControl): Integer;
begin
  // for backward compatibility with version that did not save the DPI,
  // make reasonable assumption that the configuration was saved with
  // the same DPI as we run now
  Result := StrToIntDef(S, GetControlPixelsPerInch(Control));
end;

function SavePixelsPerInch(Control: TControl): string;
begin
  Result := IntToStr(GetControlPixelsPerInch(Control));
end;

function SaveDefaultPixelsPerInch: string;
begin
  Result := IntToStr(USER_DEFAULT_SCREEN_DPI);
end;

// WORKAROUND
// https://stackoverflow.com/q/9410485/850848

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
  Form := GetParentForm(Control);
  if Form = nil then
  begin
    // This should happen only for screen tip over dropped down menu.
    // The other condition is a temporary fix is for TCustomComboEdit on TCopyParamsFrame.
    Assert((Control.ClassName = 'TTBXPopupWindow') or ((Control.Parent <> nil) and (Control.Parent.ClassName = 'TCopyParamsFrame')) or ((Control.Parent.Parent <> nil) and (Control.Parent.Parent.ClassName = 'TCopyParamsFrame')));
    Result := ScaleByPixelsPerInch(Dimension, Control);
  end
    else
  begin
    TextHeight := Form.RetrieveTextHeight;
    // runtime form (such as TTBFloatingWindowParent)
    if TextHeight = 0 then
    begin
      Result := ScaleByTextHeightRunTime(Control, Dimension);
    end
      else
    begin
      // that's our design text-size, we do not expect any other value
      Assert(TextHeight = OurDesignTimeTextHeight);
      Result := ScaleByTextHeightImpl(Control, Dimension, TextHeight);
    end;
  end;
end;

procedure GetFormScaleRatio(Form: TForm; var M, D: Integer);
begin
  M := Form.CalculateTextHeight;
  D := Form.RetrieveTextHeight;
end;

// this differs from ScaleByTextHeight only by enforcing
// constant design-time text height
function ScaleByTextHeightRunTime(Control: TControl; Dimension: Integer): Integer;
begin
  Result := ScaleByTextHeightImpl(Control, Dimension, OurDesignTimeTextHeight);
end;

function GetSystemMetricsForControl(Control: TControl; nIndex: Integer): Integer;
begin
  if Assigned(GetSystemMetricsForDpi) then
  begin
    Result := GetSystemMetricsForDpi(nIndex, GetControlPixelsPerInch(Control))
  end
    else
  begin
    Result := GetSystemMetrics(nIndex);
  end;
end;

var
  ShellImageLists: TDictionary<Integer, TImageList> = nil;

procedure InitializeShellImageLists;
type
  TSHGetImageList = function (iImageList: integer; const riid: TGUID; var ppv: Pointer): hResult; stdcall;
const
  IID_IImageList: TGUID = '{46EB5926-582E-4017-9FDF-E8998DAA0950}';
var
  Lib: THandle;
  ImageList: Integer;
  Handle: THandle;
  Height, Width: Integer;
  ShellImageList: TImageList;
  SHGetImageList: TSHGetImageList;
  HR: HRESULT;
begin
  Lib := LoadLibrary('shell32');
  SHGetImageList := GetProcAddress(Lib, 'SHGetImageList');
  ShellImageLists := TDictionary<Integer, TImageList>.Create;
  for ImageList := 0 to SHIL_LAST do
  begin
    // VCL have declaration for SHGetImageList in ShellAPI, but it does not link
    HR := SHGetImageList(ImageList, IID_IImageList, Pointer(Handle));
    if (HR = S_OK) and
       ImageList_GetIconSize(Handle, Width, Height) then
    begin

      // We could use AddOrSetValue instead, but to be on a safe siz, we prefer e.g. SHIL_SMALL over SHIL_SYSSMALL,
      // while they actually can be the same
      if not ShellImageLists.ContainsKey(Width) then
      begin
        ShellImageList := TImageList.Create(Application);
        ShellImageList.Handle := Handle;
        ShellImageList.ShareImages := True;
        ShellImageList.DrawingStyle := dsTransparent;
        ShellImageLists.Add(Width, ShellImageList);
      end;
    end;
  end;
end;

procedure NeedShellImageLists;
begin
  if ShellImageLists = nil then
  begin
    InitializeShellImageLists;
  end;
end;

function ShellImageListForControl(Control: TControl; Size: TImageListSize): TImageList;
var
  ImageListPair: TPair<Integer, TImageList>;
  Width, ImageListWidth: Integer;
  Diff, BestDiff: Integer;
begin
  // Delay load image lists, not to waste resources in console/scripting mode
  NeedShellImageLists;

  case Size of
    ilsSmall: Width := 16;
    ilsLarge: Width := 32;
    else Width := 0; Assert(False);
  end;

  Width := ScaleByPixelsPerInch(Width, Control);

  Result := nil;
  BestDiff := -1;
  for ImageListPair in ShellImageLists do
  begin
    ImageListWidth := ImageListPair.Key;
    if ImageListWidth <= Width then
    begin
      Diff := Width - ImageListWidth;
    end
      else
    begin
      // Prefer smaller images over larger, so for 150%, we use 100% images, not 200%
      Diff := ImageListWidth - Width + 1;
    end;

    if (BestDiff < 0) or (BestDiff > Diff) then
    begin
      BestDiff := Diff;
      Result := ImageListPair.Value;
    end;
  end;
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
    procedure SetMainForm(Value: TForm);
    procedure SetTerminated(Value: Boolean);
  end;

function TApplicationHelper.IsAppIconic: Boolean;
begin
  Result := Self.FAppIconic;
end;

procedure TApplicationHelper.SetAppIconic(Value: Boolean);
begin
  Self.FAppIconic := Value;
end;

procedure TApplicationHelper.SetMainForm(Value: TForm);
begin
  Self.FMainForm := Value;
end;

procedure TApplicationHelper.SetTerminated(Value: Boolean);
begin
  Self.FTerminate := Value;
end;

function IsAppIconic: Boolean;
begin
  Result := Application.IsAppIconic;
end;

procedure SetAppIconic(Value: Boolean);
begin
  Application.SetAppIconic(Value);
end;

procedure SetAppMainForm(Value: TForm);
begin
  Application.SetMainForm(Value);
end;

procedure SetAppTerminated(Value: Boolean);
begin
  Application.SetTerminated(Value);
end;

function ApiPath(Path: string): string;
begin
  Result := Path;
  if Assigned(OnApiPath) then
  begin
    Result := OnApiPath(Result);
  end;
end;

procedure ForceColorChange(Control: TWinControl);
begin
  // particularly when changing color back to default (clWindow),
  // non-client area (border line) is not redrawn,
  // keeping previous color. force redraw here
  if Control.HandleAllocated then
  begin
    RedrawWindow(Control.Handle, nil, 0, RDW_INVALIDATE or RDW_FRAME);
  end;
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

function IsUncPath(Path: string): Boolean;
begin
  Result := (Copy(Path, 1, 2) = '\\') or (Copy(Path, 1, 2) = '//');
end;

type TPreferredAppMode = (pamDefault, pamAllowDark, pamForceDark, pamForceLight, pamMax);

var
  AAllowDarkModeForWindow: function(hWnd: HWND; Allow: BOOL): BOOL; stdcall;
  ARefreshImmersiveColorPolicyState: procedure; stdcall;
  ASetPreferredAppMode: function(AppMode: TPreferredAppMode): TPreferredAppMode; stdcall;

function SupportsDarkMode: Boolean;
begin
  Result := Assigned(AAllowDarkModeForWindow) and Assigned(ARefreshImmersiveColorPolicyState);
end;

procedure AllowDarkModeForWindow(Control: TWinControl; Allow: Boolean);
begin
  Assert(Control.HandleAllocated);
  if SupportsDarkMode and Control.HandleAllocated then
  begin
    AAllowDarkModeForWindow(Control.Handle, Allow);
  end;
end;

procedure AllowDarkModeForWindow(Handle: THandle; Allow: Boolean);
begin
  if SupportsDarkMode then
  begin
    AAllowDarkModeForWindow(Handle, Allow);
  end;
end;

procedure RefreshColorMode;
begin
  if SupportsDarkMode then
  begin
    ARefreshImmersiveColorPolicyState;
  end;
end;

var
  SysDarkTheme: Integer;

procedure ResetSysDarkTheme;
begin
  SysDarkTheme := -1;
end;

function DoGetSysDarkTheme(RootKey: HKEY): Integer;
const
  ThemesPersonalizeKey = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize';
  AppsUseLightThemeValue = 'AppsUseLightTheme';
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := RootKey;
    Result := -1;
    if Registry.OpenKeyReadOnly(ThemesPersonalizeKey) and
       Registry.ValueExists(AppsUseLightThemeValue) then
    begin
      if Registry.ReadBool(AppsUseLightThemeValue) then Result := 0
        else Result := 1;
    end;
  finally
    Registry.Free;
  end;
end;

function GetSysDarkTheme: Boolean;
begin
  if SysDarkTheme < 0 then
  begin
    SysDarkTheme := DoGetSysDarkTheme(HKEY_CURRENT_USER);
    if SysDarkTheme < 0 then
    begin
      SysDarkTheme := DoGetSysDarkTheme(HKEY_LOCAL_MACHINE);
      if SysDarkTheme < 0 then
      begin
        SysDarkTheme := 0;
      end;
    end;
  end;

  Result := (SysDarkTheme > 0);
end;

var
  Lib: THandle;
  OSVersionInfo: TOSVersionInfoEx;
initialization
  Lib := LoadLibrary('shcore');
  if Lib <> 0 then
  begin
    GetDpiForMonitor := GetProcAddress(Lib, 'GetDpiForMonitor');
  end;

  Lib := LoadLibrary('user32');
  if Lib <> 0 then
  begin
    GetSystemMetricsForDpi := GetProcAddress(Lib, 'GetSystemMetricsForDpi');
    SystemParametersInfoForDpi := GetProcAddress(Lib, 'SystemParametersInfoForDpi');
  end;

  AAllowDarkModeForWindow := nil;
  ARefreshImmersiveColorPolicyState := nil;
  ASetPreferredAppMode := nil;

  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  if GetVersionEx(OSVersionInfo) and (OSVersionInfo.dwBuildNumber >= 17763) then
  begin
    Lib := GetModuleHandle('uxtheme');
    if Lib <> 0 then
    begin
      AAllowDarkModeForWindow := GetProcAddress(Lib, MakeIntResource(133));
      ARefreshImmersiveColorPolicyState := GetProcAddress(Lib, MakeIntResource(104));
      if OSVersionInfo.dwBuildNumber >= 18334 then
      begin
        ASetPreferredAppMode := GetProcAddress(Lib, MakeIntResource(135));
      end;

      if SupportsDarkMode then
      begin
        // Both SetPreferredAppMode and RefreshImmersiveColorPolicyState is needed for
        // dark list view headers and dark list view and tree view scrollbars
        if Assigned(ASetPreferredAppMode) then
        begin
          ASetPreferredAppMode(pamAllowDark);
        end;
        ARefreshImmersiveColorPolicyState;
      end;
    end;
  end;

  ResetSysDarkTheme;

finalization
  // No need to release individual image lists as they are owned by Application object.
  FreeAndNil(ShellImageLists);
end.
