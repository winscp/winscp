unit DragDrop;
{
  Description
  ===========
    TDragDrop is a component for OLE drag-and-drop operations. The component
    is able to make successor components of TWinControl to the source AND
    target of drag-and-drop operations.


  Disclaimer
  ==========
    The author disclaims all warranties, expressed or implied, including,
    without limitation, the warranties of merchantability and of fitness
    for any purpose. The author assumes no liability for damages, direct or
    consequential, which may result from the use of this component/unit.


  Restrictions on Using the Unit / Component
  ==========================================
    This unit/component is copyright 1998 by Dieter Steinwedel. ALL RIGHTS
    ARE RESERVED BY DIETER STEINWEDEL. You are allowed to use it freely
    subject to the following restrictions:

    • You are not allowed delete or alter the author's name and
      copyright in any manner

    • You are not allowed to publish a copy, modified version or
      compilation neither for payment in any kind nor freely

    • You are allowed to create a link to the download in the WWW

    • These restrictions and terms apply to you as long as until
      I alter them. Changes can found on my homepage
}

{$ALIGN ON}
{$ASSERTIONS OFF}
{$BOOLEVAL OFF}
{$DENYPACKAGEUNIT OFF}
{$EXTENDEDSYNTAX ON}
{$HINTS ON}
{$IMPORTEDDATA ON}
{$LONGSTRINGS ON}
{$OPTIMIZATION ON}
{$TYPEDADDRESS OFF}
{$TYPEINFO OFF}
{$WARNINGS ON}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  SysUtils, Windows, Classes, Controls, Forms, ShellApi,
  Menus, Messages, Graphics, ActiveX, ExtCtrls, Grids;
{MP}(*$HPPEMIT '#include <oleidl.h>'*)

// Available drop effects by the system:
// (redefined, so need not to type "ActiveX" in the uses clause of your units )

const DROPEFFECT_None=ActiveX.DROPEFFECT_None;
      DROPEFFECT_Copy=ActiveX.DROPEFFECT_Copy;
      DROPEFFECT_Move=ActiveX.DROPEFFECT_Move;
      DROPEFFECT_Link=ActiveX.DROPEFFECT_Link;
      DROPEFFECT_Scroll=ActiveX.DROPEFFECT_Scroll;

      TYMED_HGLOBAL=ActiveX.TYMED_HGLOBAL;
      TYMED_FILE=ActiveX.TYMED_FILE;
      TYMED_ISTREAM=ActiveX.TYMED_ISTREAM;
      TYMED_ISTORAGE=ActiveX.TYMED_ISTORAGE;
      TYMED_GDI=ActiveX.TYMED_GDI;
      TYMED_MFPICT=ActiveX.TYMED_MFPICT;
      TYMED_ENHMF=ActiveX.TYMED_ENHMF;
      TYMED_NULL=ActiveX.TYMED_NULL;
      DefaultCursor=0;

type
  IEnumFormatEtc = ActiveX.IEnumFormatEtc;
  IDataObject = ActiveX.IDataObject;
  TFormatEtc = ActiveX.TFormatEtc;
  TStgMedium = ActiveX.TStgMedium;

  TDropEffect=(deCopy, deMove, deLink);
  TDragResult=(drInvalid, drCancelled, drCopy, drMove, drLink);
  TDropEffectSet = set of TDropEffect;
  TDragDetectStatus = (ddsNone, ddsLeft, ddsRight, ddsCancelled, ddsDrag);
  TRenderDataOn = (rdoEnter, rdoEnterAndDropSync, rdoEnterAndDropAsync, rdoDropSync, rdoDropAsync, rdoNever);
  TSrcCompatibilityCheck = (CheckLindex, CheckdwAspect);
  TSrcCompatibilityCheckSet = set of TSrcCompatibilityCheck;
  TScrollInterval=1..10000;
  TScrollDirection=(sdUp, sdDown, sdLeft, sdRight);

// event handlers ...

  TOnDragEnter = procedure(DataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
     var dwEffect: longint; var Accept:boolean) of object;
  TOnDragLeave = procedure of object;
  TOnDragOver = procedure(grfKeyState: Longint; pt: TPoint;
     var dwEffect: longint) of object;
  TOnDrop = procedure(DataObj: IDataObject; grfKeyState: Longint;  pt: TPoint;
     var dwEffect: longint) of object;
  TOnQueryContinueDrag = procedure(fEscapePressed: BOOL; grfKeyState: Longint; var Result: HResult) of object;
  TOnGiveFeedback = procedure(dwEffect: Longint; var Result: HResult) of object;
  TOnDragDetect = procedure(grfKeyState: Longint; DetectStart, pt: TPoint; DragDetectStatus:TDragDetectStatus) of object;
  TOnProcessDropped = procedure(Sender: TObject; grfKeyState: Longint;  pt: TPoint; dwEffect: Longint) of object;
  TOnBeforeScrolling = procedure(Sender: TObject; pt: TPoint; var Interval: TScrollInterval;
     ScrollDirection: TScrollDirection; var ScrollPage:boolean) of object;
  TOnMenuPopup = procedure(Sender: TObject; AMenu: HMenu; DataObj:IDataObject;
     AMinCustCmd:integer; grfKeyState: Longint; pt: TPoint) of object;
  TOnMenuExecCmd = procedure(Sender: TObject; AMenu: HMenu; DataObj:IDataObject;
     Command:integer; var dwEffect: longint; var Succeeded:boolean) of object;
  TOnMenuDestroy = procedure(Sender: TObject; AMenu: HMenu) of object;

  TFormatEtcArray = array of TFormatEtc;

  TDetectRec = record
  end;

  // list classes ...

  TFormatEtcList = class
  private
     FCount:integer;
     FList:TFormatEtcArray;
     function Get(Index: Integer): TFormatEtc;
     procedure Put(Index: Integer; Item: TFormatEtc);
  public
     constructor Create;
     destructor Destroy; override;
     function Add(Item: TFormatEtc):integer;
     procedure Clear;
     procedure Delete(Index: Integer);
     function Clone:TFormatEtcList;
     property Count:integer read FCount;
     property Items[Index:integer]:TFormatEtc read get write put;
  end;

  // inherited classes ...
  TDDInterfacedObject = class(TInterfacedObject)
  public
     function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
     function _AddRef: Integer; stdcall;
     function _Release: Integer; stdcall;
  end;

  TEnumFormatEtc = class(TDDInterfacedObject, IEnumFormatEtc)
  protected
    FFormatEtcList:TFormatEtcList;
    FIndex: integer;
  public
    constructor Create(FormatEtcList:TFormatEtcList);
    destructor Destroy; override;
    function Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumFormatEtc): HResult; stdcall;
  end;

  TDataObject = class(TDDInterfacedObject, IDataObject)
  protected
    FFormatEtcList:TFormatEtcList;
    FCheckLindex:boolean;
    FCheckdwAspect:boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium):
      HResult; stdcall;
    function GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium):
      HResult; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HResult;
      stdcall;
    function GetCanonicalFormatEtc(const formatetc: TFormatEtc;
      out formatetcOut: TFormatEtc): HResult; stdcall;
    function SetData(const formatetc: TFormatEtc; var medium: TStgMedium;
      fRelease: BOOL): HResult; stdcall;
    function EnumFormatEtc(dwDirection: Longint; out enumFormatEtc:
      IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const formatetc: TFormatEtc; advf: Longint;
      const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
    function DUnadvise(dwConnection: Longint): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
      stdcall;
    function RenderData(FormatEtc:TFormatEtc;
       var StgMedium: TStgMedium): HResult; virtual; abstract;
  protected
    function AllowData(FormatEtc: TFormatEtc): Boolean; virtual;
  end;

  // forward declaration, because TDropSource and TDropTarget uses this class ...
  TDragDrop = class;

  TDropSource = class(TDDInterfacedObject, IDropSource)
  private
    FOwner: TDragDrop;
  public
    constructor Create(AOwner: TDragDrop);
    destructor Destroy; override;
    function QueryContinueDrag(fEscapePressed: BOOL;
      grfKeyState: Longint): HResult; stdcall;
    function GiveFeedback(dwEffect: Longint): HResult; stdcall;
  end;

  TDropTarget = class(TDDInterfacedObject, IDropTarget)
  private
    FAccept:boolean;
    HorzStartTimer:TTimer;
    HorzScrollTimer:TTimer;
    VertStartTimer:TTimer;
    VertScrollTimer:TTimer;
    FVScrollCode:integer;
    FHScrollCode:integer;
    procedure InitScroll(VerticalScroll:boolean; ScrollCode:integer);
    procedure TermScroll(VerticalScroll:boolean);
    procedure DetermineScrollDir(VertScrolling:boolean; var ScrollCode:integer);
    procedure OnStartTimer(Sender: TObject);
    procedure OnScrollTimer(Sender: TObject);
  protected
    FOwner: TDragDrop;
    procedure SuggestDropEffect(grfKeyState: Longint; var dwEffect: longint); virtual;
    procedure AcceptDataObject(DataObj: IDataObject; var Accept:boolean); virtual;
    procedure RenderDropped(DataObj: IDataObject; grfKeyState: Longint;  pt: TPoint;
       var dwEffect: longint); virtual;
  public
    constructor Create(AOwner: TDragDrop);
    destructor Destroy; override;
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
  end;

  // custom properties

  TScrollDetectArea = class(TPersistent)
  private
    FControl: TPersistent;
    FMargin: word;
    FRange: word;
    FOnChange: TNotifyEvent;
    procedure SetValue(Index: Integer; Value: word);
  protected
    procedure Change; dynamic;
    procedure AssignTo(Dest: TPersistent); override;
    property Control: TPersistent read FControl;
  public
    constructor Create(Control: TPersistent);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Margin: word index 0 read FMargin write SetValue default 0;
    property Range: word index 1 read FRange write SetValue default 10;
  end;

  TScrollDetectOptions = class(TPersistent)
  private
    FControl: TDragDrop;
    FScrollDelay: TScrollInterval;
    FStartDelay: TScrollInterval;
    FLeft: TScrollDetectArea;
    FTop: TScrollDetectArea;
    FRight: TScrollDetectArea;
    FBottom: TScrollDetectArea;
    FOnChange: TNotifyEvent;
    FHorzScrolling:boolean;
    FVertScrolling:boolean;
    FHorzPageScroll:boolean;
    FVertPageScroll:boolean;
    procedure SetValue(index:integer; Value: TScrollInterval);
  protected
    procedure Change; dynamic;
    procedure AssignTo(Dest: TPersistent); override;
    property Control: TDragDrop read FControl;
  public
    constructor Create(Control: TDragDrop);
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property ScrollDelay: TScrollInterval index 0 read FScrollDelay write SetValue default 100;
    property StartDelay: TScrollInterval index 1 read FStartDelay write SetValue default 750;
    property AreaLeft: TScrollDetectArea read FLeft write FLeft;
    property AreaTop: TScrollDetectArea read FTop write FTop;
    property AreaRight: TScrollDetectArea read FRight write FRight;
    property AreaBottom: TScrollDetectArea read FBottom write FBottom;
    property HorzScrolling:boolean read FHorzScrolling write FHorzScrolling default false;
    property VertScrolling:boolean read FVertScrolling write FVertScrolling default false;
    property HorzPageScroll:boolean read FHorzPageScroll write FHorzPageScroll default false;
    property VertPageScroll:boolean read FVertPageScroll write FVertPageScroll default false;
  end;

  // *THE* pseudo-visual Component
  TDragDrop = class(TComponent)
  private
    FAutoDetectDnD:boolean;
    FDragDetectDelta:byte;
    FAcceptOwnDnD:boolean;
    FBTF:Boolean;
    FContextMenu: boolean;
    FDragDropControl: TWinControl;
    FRegistered: Boolean;
    FOwnerIsSource:boolean;
    FShowPopUpMenu: boolean;
    FTargetEffectsSet: TDropEffectSet;
    FTargetEffects: longint;
    FOnQueryContinueDrag: TOnQueryContinueDrag;
    FOnGiveFeedback: TOnGiveFeedback;
    FOnDragEnter: TOnDragEnter;
    FOnDragLeave: TOnDragLeave;
    FOnDragOver: TOnDragOver;
    FOnDrop: TOnDrop;
    FSourceEffectsSet: TDropEffectSet;
    FSourceEffects: longint;
    FOnProcessDropped: TOnProcessDropped;
    OldWndProc:Pointer;
    WndProcPtr:Pointer;
    FOnDragDetect:TOnDragDetect;
    FDragDetectStatus:TDragDetectStatus;
    FDragDetectStart:TPoint;
    FRenderDataOn: TRenderDataOn;
    FDataObj:IDataObject;
    FgrfKeyState: Longint;
    Fpt: TPoint;
    FdwEffect: Longint;
    FCHCopy: HCursor;
    FCHMove: HCursor;
    FCHLink: HCursor;
    FCHScrollCopy: HCursor;
    FCHScrollMove: HCursor;
    FCHScrollLink: HCursor;
    FMessageHooked:boolean;
    FAvailableDropEffects:Longint;
    FTargetScrolling:integer;
    FSrcCompatibilityCheck:TSrcCompatibilityCheckSet;
    FScrollDetectOptions: TScrollDetectOptions;
    FOnBeforeScrolling: TOnBeforeScrolling;
    FOnAfterScrolling: TNotifyEvent;
    FPressedButton:integer;
    FInternalSource:TDragDrop;
    FOnMenuPopup:TOnMenuPopup;
    FOnMenuExecCmd:TOnMenuExecCmd;
    FOnMenuDestroy:TOnMenuDestroy;
    FOnMenuSucceeded:TOnProcessDropped;
    FOnDropHandlerSucceeded:TOnProcessDropped;
    procedure WndMethod(var Msg: TMessage);
    procedure SetDragDropControl(WinControl: TWinControl);
    procedure SetSourceEffects(Values:TDropEffectSet);
    procedure SetTargetEffects(Values:TDropEffectSet);
  protected
    FDropTarget: TDropTarget;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CreateDataObject:TDataObject; virtual; abstract;
    procedure DoMenuPopup(Sender: TObject; AMenu: HMenu; DataObj: IDataObject;
       AMinCustCmd:integer; grfKeyState: Longint; pt: TPoint); virtual;
    function DoMenuExecCmd(Sender: TObject; AMenu: HMenu; DataObj:IDataObject;
       Command:integer; var dwEffect: longint):boolean; virtual;
    procedure DoMenuDestroy(Sender:TObject; AMenu: HMenu); virtual;
    function DropHandler(const dataObj: IDataObject; grfKeyState: Longint;
       pt: TPoint; var dwEffect: Longint): boolean; virtual;
    property OnDropHandlerSucceeded:TOnProcessDropped read FOnDropHandlerSucceeded
       write FOnDropHandlerSucceeded;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RegisterTarget: Boolean;
    function UnRegisterTarget: Boolean;
    procedure HookMessageHandler;
    procedure UnhookMessageHandler(ForceUnhook:boolean);
    function ExecuteOperation(DataObject:TDataObject): TDragResult;
    function Execute: TDragResult;
    function CopyToClipboard:boolean; virtual;
    function GetFromClipboard:boolean; virtual;
    procedure StartDnDDetection(Button: TMouseButton); virtual;
    property OwnerIsSource:boolean read FOwnerIsSource;
    property Registered: Boolean read FRegistered default False;
    property CHCopy: HCursor read FCHCopy write FCHCopy default DefaultCursor;
    property CHMove: HCursor read FCHMove write FCHMove default DefaultCursor;
    property CHLink: HCursor read FCHLink write FCHLink default DefaultCursor;
    property CHScrollCopy: HCursor read FCHScrollCopy write FCHScrollCopy default DefaultCursor;
    property CHScrollMove: HCursor read FCHScrollMove write FCHScrollMove default DefaultCursor;
    property CHScrollLink: HCursor read FCHScrollLink write FCHScrollLink default DefaultCursor;
    property DragDetectStatus: TDragDetectStatus read FDragDetectStatus;
    property AvailableDropEffects: Longint read FAvailableDropEffects;
    property InternalSource:TDragDrop read FInternalSource;
  published
    property AcceptOwnDnD:boolean read FAcceptOwnDnD write FAcceptOwnDnD;
    property AutoDetectDnD:boolean read FAutoDetectDnD write FAutoDetectDnD;
    property BringToFront:Boolean read FBTF write FBTF;
    property DragDetectDelta:byte read FDragDetectDelta write FDragDetectDelta default 10;
    property DragDropControl: TWinControl read FDragDropControl write SetDragDropControl;
    property RenderDataOn: TRenderDataOn read FRenderDataOn write FRenderDataOn default rdoDropSync;
    property ScrollDetectOptions: TScrollDetectOptions read FScrollDetectOptions
             write FScrollDetectOptions;
    property SourceCompatibility:TSrcCompatibilityCheckSet read FSrcCompatibilityCheck
             write FSrcCompatibilityCheck;
    property SourceEffects: TDropEffectSet read FSourceEffectsSet write SetSourceEffects;
    property TargetPopupMenu: boolean read FShowPopUpMenu write FShowPopUpMenu;
    property TargetEffects: TDropEffectSet read FTargetEffectsSet write SetTargetEffects;
    property OnAfterScrolling: TNotifyEvent read FOnAfterScrolling write FOnAfterScrolling;
    property OnBeforeScrolling: TOnBeforeScrolling read FOnBeforeScrolling write FOnBeforeScrolling;
    property OnDragDetect: TOnDragDetect read FOnDragDetect write FOnDragDetect;
    property OnDragEnter: TOnDragEnter read FOnDragEnter write FOnDragEnter;
    property OnDragLeave: TOnDragLeave read FOnDragLeave write FOnDragLeave;
    property OnDragOver: TOnDragOver read FOnDragOver write FOnDragOver;
    property OnDrop: TOnDrop read FOnDrop write FOnDrop;
    property OnQueryContinueDrag: TOnQueryContinueDrag read FOnQueryContinueDrag
             write FOnQueryContinueDrag;
    property OnGiveFeedback: TOnGiveFeedback read FOnGiveFeedback
             write FOnGiveFeedback;
    property OnProcessDropped: TOnProcessDropped read FOnProcessDropped write FOnProcessDropped;
    property OnMenuPopup:TOnMenuPopup read FOnMenuPopup write FOnMenuPopup;
    property OnMenuExecCmd:TOnMenuExecCmd read FOnMenuExecCmd write FOnMenuExecCmd;
    property OnMenuDestroy:TOnMenuDestroy read FOnMenuDestroy write FOnMenuDestroy;
    property OnMenuSucceeded:TOnProcessDropped read FOnMenuSucceeded write FOnMenuSucceeded;
  end;

procedure Register;

resourcestring
  MICopyStr = '&Copy Here';
  MIMoveStr = '&Move Here';
  MILinkStr = '&Shortcut(s) Create Here';
  MIAbortStr = '&Abort';

implementation

const CmdAbort = 0;
      CmdMove = 1;
      CmdCopy = 2;
      CmdLink = 3;
      CmdSeparator = 4;
      MinCustCmd = 10;

var DDM_ProcessDropped:DWord; // Never change its value
    MouseHookHandle:HHook;
    MouseHookDragDrop:TDragDrop;
    GInternalSource:TDragDrop;

function MouseHookProc(code: Integer; wparam: WPARAM; lparam: LPARAM): LRESULT; stdcall;
var MouseHookStruct:TMouseHookStruct;
    grfKeyState:Longint;
begin
     Result:=CallNextHookEx(MouseHookHandle,Code,wParam,lParam);
     if assigned(MouseHookDragDrop)=false then
     begin
          UnHookWindowsHookEx(MouseHookHandle);
          MouseHookHandle:=0;
          exit;
     end;
     with MouseHookDragDrop do
     begin
          MouseHookStruct:=TMouseHookStruct(pointer(lparam)^);
          if ((FDragDetectStatus=ddsRight) and (wParam=WM_LBUTTONDOWN)) or
             ((FDragDetectStatus=ddsLeft) and (wParam=WM_RBUTTONDOWN)) then
          begin
               FPressedButton:=2;
               FDragDetectStatus:=ddsCancelled;
               if assigned(FOnDragDetect) then
               begin
                    if HiWord(DWord(GetKeyState(VK_SHIFT)))<>0 then
                       grfKeyState:=MK_SHIFT
                    else grfKeyState:=0;
                    if HiWord(DWord(GetKeyState(VK_CONTROL)))<>0 then
                       grfKeyState:=grfKeyState or MK_CONTROL;
                    FOnDragDetect(grfKeyState,
                       FDragDropControl.ScreenToClient(FDragDetectStart),
                       FDragDropControl.ScreenToClient(MouseHookStruct.pt),
                       FDragDetectStatus);
               end;
               exit;
          end;
          if ((wParam=WM_LBUTTONDOWN) or (wParam=WM_RBUTTONDOWN)) and
             (FDragDetectStatus=ddsCancelled) then
          begin
               FPressedButton:=2;
               exit;
          end;
          if (FDragDetectStatus=ddsCancelled) and
             ((wParam=WM_LBUTTONUP) or (wParam=WM_RBUTTONUP)) then
          begin
               dec(FPressedButton);
               if FPressedButton<=0 then
               begin
                    UnHookWindowsHookEx(MouseHookHandle);
                    MouseHookHandle:=0;
                    FDragDetectStatus:=ddsNone;
                    if assigned(FOnDragDetect) then
                    begin
                         if HiWord(DWord(GetKeyState(VK_SHIFT)))<>0 then
                            grfKeyState:=MK_SHIFT
                         else grfKeyState:=0;
                         if HiWord(DWord(GetKeyState(VK_CONTROL)))<>0 then
                            grfKeyState:=grfKeyState or MK_CONTROL;
                         FOnDragDetect(grfKeyState,
                            FDragDropControl.ScreenToClient(FDragDetectStart),
                            FDragDropControl.ScreenToClient(MouseHookStruct.pt),
                            FDragDetectStatus);
                    end;
               end;
               exit;
          end;
          if ((FDragDetectStatus=ddsRight) and (wParam=WM_RBUTTONUP)) or
             ((FDragDetectStatus=ddsLeft) and (wParam=WM_LBUTTONUP)) then
          begin
               UnHookWindowsHookEx(MouseHookHandle);
               MouseHookHandle:=0;
               FDragDetectStatus:=ddsNone;
               if assigned(FOnDragDetect) then
               begin
                    if HiWord(DWord(GetKeyState(VK_SHIFT)))<>0 then
                       grfKeyState:=MK_SHIFT
                    else grfKeyState:=0;
                    if HiWord(DWord(GetKeyState(VK_CONTROL)))<>0 then
                       grfKeyState:=grfKeyState or MK_CONTROL;
                    FOnDragDetect(grfKeyState,
                       FDragDropControl.ScreenToClient(FDragDetectStart),
                       FDragDropControl.ScreenToClient(MouseHookStruct.pt),
                       FDragDetectStatus);
               end;
               exit;
          end;
          if ((abs(FDragDetectStart.X-MouseHookStruct.pt.x)>DragDetectDelta) or
              (abs(FDragDetectStart.Y-MouseHookStruct.pt.y)>DragDetectDelta)) and
             ((FDragDetectStatus=ddsRight) or (FDragDetectStatus=ddsLeft)) then
          begin
               FDragDetectStatus:=ddsDrag;
               UnHookWindowsHookEx(MouseHookHandle);
               MouseHookHandle:=0;
               if assigned(FOnDragDetect) then
               begin
                    if HiWord(DWord(GetKeyState(VK_SHIFT)))<>0 then
                       grfKeyState:=MK_SHIFT
                    else grfKeyState:=0;
                    if HiWord(DWord(GetKeyState(VK_CONTROL)))<>0 then
                       grfKeyState:=grfKeyState or MK_CONTROL;
                    FOnDragDetect(grfKeyState,
                       FDragDropControl.ScreenToClient(FDragDetectStart),
                       FDragDropControl.ScreenToClient(MouseHookStruct.pt),
                       FDragDetectStatus);
               end;
               if (FDragDetectStatus<>ddsNone) then
               begin
                    FDragDetectStatus:=ddsNone;
                    if assigned(FOnDragDetect) then
                    begin
                         if HiWord(DWord(GetKeyState(VK_SHIFT)))<>0 then
                            grfKeyState:=MK_SHIFT
                         else grfKeyState:=0;
                         if HiWord(DWord(GetKeyState(VK_CONTROL)))<>0 then
                            grfKeyState:=grfKeyState or MK_CONTROL;
                         FOnDragDetect(grfKeyState,
                            FDragDropControl.ScreenToClient(FDragDetectStart),
                            FDragDropControl.ScreenToClient(MouseHookStruct.pt),
                            FDragDetectStatus);
                    end;
               end;
          end;
     end;
end;

// TFormatEtcList --------------------------------------------------------------

constructor TFormatEtcList.Create;
begin
     inherited Create;
     FCount:=0;
     SetLength(FList, 0);
end;

destructor TFormatEtcList.Destroy;
begin
     if (FCount>0) and (FList<>nil) then SetLength(FList, 0);
     inherited Destroy;
end;

function TFormatEtcList.Get(Index: Integer): TFormatEtc;
begin
     if (Index>=FCount) or (FList=nil) then
        raise EListError.Create('Invalid item index')
     else Result:=FList[Index];
end;

procedure TFormatEtcList.Put(Index: Integer; Item: TFormatEtc);
begin
     if (Index>=FCount) or (FList=nil) then
        raise EListError.Create('Invalid item index')
     else FList[Index]:=Item;
end;

function TFormatEtcList.Add(Item: TFormatEtc):integer;
begin
     SetLength(FList, Succ(FCount));
     FList[FCount]:=Item;
     Result:=FCount;
     inc(FCount);
end;

procedure TFormatEtcList.Clear;
begin
     SetLength(Flist, 0);
     FCount:=0;
end;

function TFormatEtcList.Clone:TFormatEtcList;
var FEL:TFormatEtcList;
begin
     FEL:=TFormatEtcList.Create;
     if FList<>nil then
     begin
          SetLength(FEL.FList, FCount);
          CopyMemory(FEL.FList,FList,FCount*SizeOf(TFormatEtc));
          FEL.FCount:=FCount;
     end;
     Result:=FEL;
end;

procedure TFormatEtcList.Delete(Index: Integer);
var movecount:integer;
begin
     if (Index>=FCount) or (FList=nil) then
        raise EListError.Create('Invalid item index')
     else
     begin
          movecount:=FCount-Index-1;
          System.move(FList[Index+1],FList[Index],movecount*sizeof(TFormatEtc));
          dec(FCount);
          SetLength(FList, FCount);
     end;
end;

// TDDInterfacedObject ---------------------------------------------------------

function TDDInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
     Result:=inherited QueryInterface(IID,Obj);
end;

function TDDInterfacedObject._AddRef: Integer;
begin
     Result:=inherited _AddRef;
end;

function TDDInterfacedObject._Release: Integer;
begin
     Result:=inherited _Release;
end;

// TEnumFormatEtc --------------------------------------------------------------

constructor TEnumFormatEtc.Create(FormatEtcList:TFormatEtcList);
begin
     inherited Create;
     _AddRef;
     FFormatEtcList:=FormatEtcList;
end;

destructor TEnumFormatEtc.Destroy;
begin
     if Assigned(FFormatEtcList) then FFormatEtcList.Free;
     inherited Destroy;
end;

function TEnumFormatEtc.Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult;
var copycount:integer;
begin
     Result:=S_False;
     if pceltFetched<>nil then pceltFetched^:=0;
     if (celt<=0) or (FFormatEtcList.Count=0) or (FIndex>=FFormatEtcList.Count) or
        ((pceltFetched=nil) and (celt<>1)) then exit;
     copycount:=FFormatEtcList.Count-FIndex;
     if celt<copycount then copycount:=celt;
     if pceltFetched<>nil then pceltFetched^:=copycount;
     CopyMemory(@TFormatEtc(elt),@TFormatEtc(FFormatEtcList.FList[FIndex]),
        copycount*sizeof(TFormatEtc));
     inc(FIndex,copycount);
     Result:=S_OK;
end;

function TEnumFormatEtc.Skip(celt: Longint): HResult;
begin
     if (FIndex+celt<=FFormatEtcList.Count) then
     begin
          inc(FIndex,celt);
          Result:=S_Ok;
     end
     else Result:=S_False;
end;

function TEnumFormatEtc.Reset: HResult;
begin
     FIndex:=0;
     Result:=S_OK;
end;

function TEnumFormatEtc.Clone(out Enum: IEnumFormatEtc): HResult;
begin
     Result:=S_OK;
     try
        Enum:=TEnumFormatEtc.Create(FFormatEtcList);
        TEnumFormatEtc(Enum).FIndex := FIndex;
     except
        Result:=E_Fail;
     end;
end;

// TDataObject -----------------------------------------------------------------

constructor TDataObject.Create;
begin
     inherited Create;
     _AddRef;
     FFormatEtcList:=TFormatEtcList.Create;
end;

destructor TDataObject.Destroy;
begin
     FFormatEtcList.Free;
     inherited Destroy;
end;

function TDataObject.GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium):
      HResult;
var i:integer;
    Cursor:TCursor;
begin
     try
        if FFormatEtcList.Count>0 then
           for i:=0 to FFormatEtcList.Count-1 do
               if (formatetcIn.tymed and FFormatEtcList.Items[i].tymed<>0) and
                  ((FCheckLindex=false) or (FCheckLindex and
                   (formatetcIn.lindex=FFormatEtcList.Items[i].lindex))) and
                  ((FCheckdwAspect=false) or (FCheckdwAspect and
                   (formatetcIn.dwAspect=FFormatEtcList.Items[i].dwAspect))) and
                  (formatetcIn.cfFormat=FFormatEtcList.Items[i].cfFormat) then
               begin
                    Cursor:=Screen.Cursor;
                    try
                       Screen.Cursor:=crHourglass;
                       Result:=RenderData(formatetcIn,medium);
                    finally
                       Screen.Cursor:=Cursor;
                    end;
                    exit;
               end;
        Result:=DV_E_FormatEtc;
     except
        medium.HGlobal:=0;
        Result:=E_Fail;
     end;
end;

function TDataObject.GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium):
      HResult;
begin
     Result:=E_NOTIMPL;
end;

function TDataObject.GetCanonicalFormatEtc(const formatetc: TFormatEtc;
      out formatetcOut: TFormatEtc): HResult;
begin
     Result:=E_NOTIMPL;
end;

function TDataObject.QueryGetData(const formatetc: TFormatEtc): HResult;
const DVError:array[0..3] of HResult=(DV_E_FORMATETC,DV_E_TYMED,DV_E_DVASPECT,DV_E_LINDEX);
var i,j:integer;
begin
     j:=0;
     if (FFormatEtcList.Count>0) and AllowData(FormatEtc) then
        for i:=0 to FFormatEtcList.Count-1 do
            if FormatEtc.cfFormat=FFormatEtcList.Items[i].cfFormat then
            begin
                 if FormatEtc.tymed and FFormatEtcList.Items[i].tymed<>0 then
                 begin
                      if FormatEtc.dwAspect=FFormatEtcList.Items[i].dwAspect then
                      begin
                           if FormatEtc.lindex=FFormatEtcList.Items[i].lindex then
                           begin
                                Result:=S_OK;
                                exit;
                           end
                           else if j<3 then j:=3;
                      end
                      else if j<2 then j:=2;
                 end
                 else if j<1 then j:=1;
            end;
     Result:=DVError[j];
end;

function TDataObject.AllowData(FormatEtc: TFormatEtc): Boolean;
begin
  Result := True;
end;

function TDataObject.EnumFormatEtc(dwDirection: Longint; out enumFormatEtc:
      IEnumFormatEtc): HResult;
begin
     Result:=E_Fail;
     if dwDirection=DATADIR_GET then
     begin
          EnumFormatEtc:=TEnumFormatEtc.Create(FFormatEtcList.Clone);
          Result:=S_OK;
     end
     else EnumFormatEtc:=nil;
     if EnumFormatEtc=nil then Result:=OLE_S_USEREG;
end;

function TDataObject.SetData(const formatetc: TFormatEtc; var medium: TStgMedium;
      fRelease: BOOL): HResult;
var i:integer;
    AddData:boolean;
begin
     Result:=E_Fail;
     if FRelease then exit;
     AddData:=true;
     if FFormatEtcList.Count>0 then
        for i:=0 to FFormatEtcList.Count-1 do
            if FFormatEtcList.Items[i].cfFormat=FormatEtc.cfFormat then
            begin
                 AddData:=false;
                 FFormatEtcList.Items[i]:=FormatEtc;
            end;
     if AddData then
        FFormatEtcList.Add(FormatEtc);
end;

function  TDataObject.DAdvise(const formatetc: TFormatEtc; advf: Longint;
      const advSink: IAdviseSink; out dwConnection: Longint): HResult;
begin
     Result:=E_NOTIMPL;
end;

function TDataObject.DUnadvise(dwConnection: longint): HResult; stdcall;
begin
     Result:=E_NOTIMPL;
end;

function TDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
begin
     Result:=OLE_E_AdviseNotSupported;
end;

// TDropSource methods ---------------------------------------------------------

constructor TDropSource.Create(AOwner: TDragDrop);
begin
     inherited Create;
     _AddRef;
     FOwner:=AOwner;
end;

destructor TDropSource.Destroy;
begin
     inherited Destroy;
end;

function TDropSource.QueryContinueDrag(fEscapePressed: BOOL;
      grfKeyState: Longint): HResult; stdcall;
// Determines whether a drag-and-drop operation should be continued, cancelled,
// or completed. You do not call this method directly. The OLE DoDragDrop function
// calls this method during a drag-and-drop operation.
begin
     // Abort drag-and-drop?
     if (((grfKeyState and MK_LBUTTON)<>0) and
         ((grfKeyState and MK_RBUTTON)<>0)) or fEscapePressed then
     begin
          Result:=DRAGDROP_S_CANCEL;
          FOwner.FOwnerIsSource:=false;
     end
     // Finish drag-and-drop?
     else if (((grfKeyState and MK_LBUTTON)=0) and
              ((grfKeyState and MK_RBUTTON)=0)) then Result:=DRAGDROP_S_DROP
          else Result:=S_OK;
     if assigned(FOwner.FOnQueryContinueDrag) then
        FOwner.FOnQueryContinueDrag(fEscapePressed,grfKeyState,Result);
end;

function TDropSource.GiveFeedback(dwEffect: Longint): HResult; stdcall;
// Enables a source application to give visual feedback to its end user
// during a drag-and-drop operation by providing the DoDragDrop function
// with an enumeration value specifying the visual effect.
var HC: HCursor;
begin
     if Assigned(FOwner.FOnGiveFeedback) then FOwner.FOnGiveFeedback(dwEffect,Result);
     if dwEffect and DROPEFFECT_SCROLL<>0 then
     begin
          if dwEffect and DROPEFFECT_LINK<>0 then HC:=FOwner.FCHScrollLink
          else if dwEffect and DROPEFFECT_Move<>0 then HC:=FOwner.FCHScrollMove
               else if dwEffect and DROPEFFECT_COPY<>0 then HC:=FOwner.FCHScrollCopy
                    else HC:=DefaultCursor;
     end
     else if dwEffect and DROPEFFECT_LINK<>0 then HC:=FOwner.FCHLink
          else if dwEffect and DROPEFFECT_Move<>0 then HC:=FOwner.FCHMove
               else if dwEffect and DROPEFFECT_COPY<>0 then HC:=FOwner.FCHCopy
                         else HC:=DefaultCursor;
     if HC=DefaultCursor then
     begin
          Result:=DRAGDROP_S_USEDEFAULTCURSORS
     end
         else
     begin
          Result:=S_Ok;
          Windows.SetCursor(HC);
     end;
end;

// TDropTarget interface -------------------------------------------------------

constructor TDropTarget.Create(AOwner: TDragDrop);
begin
     inherited Create;
     FOwner:=AOwner;
     _AddRef;
     HorzStartTimer:=TTimer.Create(FOwner);
     HorzStartTimer.Enabled:=false;
     HorzStartTimer.OnTimer:=OnStartTimer;
     HorzScrollTimer:=TTimer.Create(FOwner);
     HorzScrollTimer.Enabled:=false;
     HorzScrollTimer.OnTimer:=OnScrollTimer;
     VertStartTimer:=TTimer.Create(FOwner);
     VertStartTimer.Enabled:=false;
     VertStartTimer.OnTimer:=OnStartTimer;
     VertScrollTimer:=TTimer.Create(FOwner);
     VertScrollTimer.Enabled:=false;
     VertScrollTimer.OnTimer:=OnScrollTimer;
     FVScrollCode:=0;
     FHScrollCode:=0;
end;

destructor TDropTarget.Destroy;
begin
     HorzStartTimer.Free;
     HorzScrollTimer.Free;
     VertStartTimer.Free;
     VertScrollTimer.Free;
     inherited Destroy;
end;

procedure TDropTarget.InitScroll(VerticalScroll:boolean; ScrollCode:integer);
begin
     TermScroll(VerticalScroll);
     if VerticalScroll then
     begin
          VertStartTimer.Interval:=FOwner.FScrollDetectOptions.FStartDelay;
          VertStartTimer.Enabled:=true;
          FVScrollCode:=ScrollCode;
     end
     else
     begin
          HorzStartTimer.Interval:=FOwner.FScrollDetectOptions.FStartDelay;
          HorzStartTimer.Enabled:=true;
          FHScrollCode:=ScrollCode;
     end;
end;

procedure TDropTarget.TermScroll(VerticalScroll:boolean);
begin
     if VerticalScroll then
     begin
          VertStartTimer.Enabled:=false;
          if VertScrollTimer.Enabled then
             sendmessage(FOwner.DragDropControl.handle,WM_VScroll,SB_ENDSCROLL,0);
          VertScrollTimer.Enabled:=false;
          FVScrollCode:=0;
     end
     else
     begin
          HorzStartTimer.Enabled:=false;
          if HorzScrollTimer.Enabled then
             sendmessage(FOwner.DragDropControl.handle,WM_HScroll,SB_ENDSCROLL,0);
          HorzScrollTimer.Enabled:=false;
          FHScrollCode:=0;
     end;
end;

procedure TDropTarget.DetermineScrollDir(VertScrolling:boolean;
   var ScrollCode:integer);
var p1m,p1r,p2m,p2r:integer;
    ptmc:TPoint;
    SCROLLINFO:TSCROLLINFO;
begin
     GetCursorPos(ptmc);
     ptmc:=FOwner.DragDropControl.ScreenToClient(ptmc);
     if VertScrolling then
     begin
          // Checking vertical scroll areas ...
          // If the vertical scroll areas intersect, we don't allow scrolling
          p1m:=FOwner.FScrollDetectOptions.AreaTop.Margin;
          p1r:=p1m+FOwner.ScrollDetectOptions.AreaTop.Range;
          p2m:=FOwner.DragDropControl.ClientHeight-1-
             FOwner.ScrollDetectOptions.AreaBottom.Margin;
          p2r:=p2m-FOwner.ScrollDetectOptions.AreaBottom.Range;
          if (p1r<p2r) then
          begin
               if (p1m<=ptmc.y) and (p1r>=ptmc.y) then ScrollCode:=1
               else if (p2m>=ptmc.y) and (p2r<=ptmc.y) then ScrollCode:=2
                    else ScrollCode:=0;
               if ScrollCode>0 then
               begin
                    ScrollInfo.cbSize := Sizeof(ScrollInfo);
                    ScrollInfo.FMask:=SIF_PAGE or SIF_POS or SIF_RANGE;
                    if GetScrollInfo(FOwner.DragDropControl.Handle,SB_VERT,
                       ScrollInfo) then
                    begin
                         if ScrollInfo.nPage>0 then dec(ScrollInfo.nPage);
                         if ((ScrollCode=1) and (ScrollInfo.nPos<=ScrollInfo.nMin)) or
                            ((ScrollCode=2) and
                             (ScrollInfo.nPos>=ScrollInfo.nMax-integer(ScrollInfo.nPage))) then
                            ScrollCode:=0;
                    end
                    else ScrollCode:=0;
               end;
          end
          else ScrollCode:=0;
     end
     else
     begin
          // Checking horizontal scroll areas ...
          // If the horizontal scroll areas intersect, we don't allow scrolling
          p1m:=FOwner.FScrollDetectOptions.AreaLeft.Margin;
          p1r:=p1m+FOwner.ScrollDetectOptions.AreaLeft.Range;
          p2m:=FOwner.DragDropControl.ClientWidth-1-
             FOwner.ScrollDetectOptions.AreaRight.Margin;
          p2r:=p2m-FOwner.ScrollDetectOptions.AreaRight.Range;
          if (p1r<p2r) then
          begin
               if (p1m<=ptmc.x) and (p1r>=ptmc.x) then ScrollCode:=1
               else if (p2m>=ptmc.x) and (p2r<=ptmc.x) then ScrollCode:=2
                    else ScrollCode:=0;
               if ScrollCode>0 then
               begin
                    ScrollInfo.cbSize := Sizeof(ScrollInfo);
                    ScrollInfo.FMask:=SIF_PAGE or SIF_POS or SIF_RANGE;
                    if GetScrollInfo(FOwner.DragDropControl.Handle,SB_Horz,
                       ScrollInfo) then
                    begin
                         if ScrollInfo.nPage>0 then dec(ScrollInfo.nPage);
                         if ((ScrollCode=1) and (ScrollInfo.nPos<=ScrollInfo.nMin)) or
                            ((ScrollCode=2) and
                             (ScrollInfo.nPos>=ScrollInfo.nMax-integer(ScrollInfo.nPage))) then
                            ScrollCode:=0;
                    end
                    else ScrollCode:=0;
               end;
          end
          else ScrollCode:=0;
     end;
end;

procedure TDropTarget.OnStartTimer(Sender: TObject);
begin
     if Sender=HorzStartTimer then
     begin
          HorzStartTimer.Enabled:=false;
          HorzScrollTimer.Interval:=FOwner.FScrollDetectOptions.FScrollDelay;
          OnScrollTimer(HorzScrollTimer);
          HorzScrollTimer.Enabled:=true;
     end
     else
     begin
          VertStartTimer.Enabled:=false;
          VertScrollTimer.Interval:=FOwner.FScrollDetectOptions.FScrollDelay;
          OnScrollTimer(VertScrollTimer);
          VertScrollTimer.Enabled:=true;
     end;
end;

procedure TDropTarget.OnScrollTimer(Sender: TObject);
var ScrollPage:boolean;
    pt:TPoint;
    Interval:TScrollInterval;
    ScrollCode,SCWParam:integer;
begin
     Interval:=FOwner.FScrollDetectOptions.FScrollDelay;
     if Sender=VertScrollTimer then
     begin
          if FOwner.FScrollDetectOptions.FVertScrolling then
          begin
               DetermineScrollDir(true,ScrollCode);
               if ScrollCode>0 then
               begin
                    if ((VertStartTimer.Enabled=false) and (VertScrollTimer.Enabled=false)) or
                       (FVScrollCode<>ScrollCode) then InitScroll(true,ScrollCode)
                    else
                    begin
                         ScrollPage:=FOwner.FScrollDetectOptions.FVertPageScroll;
                         if assigned(FOwner.FOnBeforeScrolling) then
                         begin
                              GetCursorPos(pt);
                              pt:=FOwner.DragDropControl.ScreenToClient(pt);
                              if FVScrollCode=1 then FOwner.FOnBeforeScrolling(FOwner, pt,
                                 Interval, sdUp, ScrollPage)
                              else FOwner.FOnBeforeScrolling(FOwner, pt, Interval, sdDown,
                                   ScrollPage);
                         end;
                         if ScrollPage then
                         begin
                              if FVScrollCode=1 then SCWParam:=SB_PAGEUP
                              else SCWParam:=SB_PAGEDOWN;
                         end
                         else
                         begin
                              if FVScrollCode=1 then SCWParam:=SB_LINEUP
                              else SCWParam:=SB_LINEDOWN;
                         end;
                         sendmessage(FOwner.DragDropControl.handle,WM_VScroll,SCWParam,0);
                         if assigned(FOwner.FOnAfterScrolling) then
                            FOwner.FOnAfterScrolling(FOwner);
                         VertScrollTimer.Interval:=Interval;
                    end;
               end
               else if FVScrollCode<>0 then TermScroll(true);
          end
          else if FVScrollCode<>0 then TermScroll(true);
     end
     else
     begin
          if FOwner.FScrollDetectOptions.FHorzScrolling then
          begin
               DetermineScrollDir(false,ScrollCode);
               if ScrollCode>0 then
               begin
                    if ((HorzStartTimer.Enabled=false) and (HorzScrollTimer.Enabled=false)) or
                       (FHScrollCode<>ScrollCode) then InitScroll(false,ScrollCode)
                    else
                    begin
                         ScrollPage:=FOwner.FScrollDetectOptions.FHorzPageScroll;
                         if assigned(FOwner.FOnBeforeScrolling) then
                         begin
                              GetCursorPos(pt);
                              pt:=FOwner.DragDropControl.ScreenToClient(pt);
                              if FHScrollCode=1 then FOwner.FOnBeforeScrolling(FOwner, pt,
                                 Interval, sdLeft, ScrollPage)
                              else FOwner.FOnBeforeScrolling(FOwner, pt, Interval, sdRight,
                                   ScrollPage);
                         end;
                         if ScrollPage then
                         begin
                              if FHScrollCode=1 then SCWParam:=SB_PAGELEFT
                              else SCWParam:=SB_PAGERIGHT;
                         end
                         else
                         begin
                              if FHScrollCode=1 then SCWParam:=SB_LINELEFT
                              else SCWParam:=SB_LINERIGHT;
                         end;
                         sendmessage(FOwner.DragDropControl.handle,WM_HScroll,SCWParam,0);
                         HorzScrollTimer.Interval:=Interval;
                    end;
               end
               else if FHScrollCode<>0 then TermScroll(false);
          end
          else if FHScrollCode<>0 then TermScroll(false);
     end;
end;

procedure TDropTarget.SuggestDropEffect(grfKeyState: Longint; var dwEffect: longint);
begin
     if (FOwner.FAcceptOwnDnD=false) and
        (FOwner.FOwnerIsSource) then dwEffect:=DropEffect_None
     else if (grfKeyState and MK_CONTROL=0) and (grfKeyState and MK_SHIFT<>0) and
             (FOwner.FTargetEffects and DropEffect_Move<>0) then
             dwEffect:=DropEffect_Move
          else if (grfKeyState and MK_CONTROL<>0) and
                  (grfKeyState and MK_SHIFT<>0) and
                  (FOwner.FTargetEffects and DropEffect_Link<>0) then
                  dwEffect:=DropEffect_Link
               else if (deCopy in FOwner.FTargetEffectsSet) and
                       (dwEffect and DropEffect_Copy<>0) then
                       dwEffect:=DropEffect_Copy
                    else if (deMove in FOwner.FTargetEffectsSet) and
                            (dwEffect and DropEffect_Move<>0) then
                            dwEffect:=DropEffect_Move
                         else if (deLink in FOwner.FTargetEffectsSet) and
                                 (dwEffect and DropEffect_Link<>0) then
                                 dwEffect:=DropEffect_Link
                              else dwEffect:=DropEffect_None;
     if FOwner.FTargetScrolling<>0 then dwEffect:=dwEffect or integer(DropEffect_Scroll);
end;

procedure TDropTarget.AcceptDataObject(DataObj: IDataObject; var Accept:boolean);
begin
     Accept:=true;
end;

function TDropTarget.DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult;
// Is called if the d&d-mouse cursor moves ON (one call only) the TargeTWinControl. Here,
// you influence if a drop can be accepted and the drop's effect if accepted.
begin
     TDragDrop(FOwner).FInternalSource:=GInternalSource;
     FOwner.FAvailableDropEffects:=dwEffect;
     FOwner.FContextMenu:=grfKeyState and mk_rbutton<>0;
     if (FOwner.RenderDataOn=rdoEnter) or (FOwner.RenderDataOn=rdoEnterAndDropSync) or
        (FOwner.RenderDataOn=rdoEnterAndDropAsync) then
        RenderDropped(DataObj, grfKeyState, pt, dwEffect);
     SuggestDropEffect(grfKeyState,dwEffect);
     AcceptDataObject(DataObj, FAccept);
     if Assigned(FOwner.OnDragEnter) then
        FOwner.OnDragEnter(DataObj, grfKeyState,
           FOwner.FDragDropControl.ScreenToClient(pt), dwEffect, FAccept);
     if ((FOwner.FAcceptOwnDnD=false) and (FOwner.FOwnerIsSource)) or
        (FAccept=false) then dwEffect:=DropEffect_None;
     Result:= NOERROR;
end;

function TDropTarget.DragOver(grfKeyState: Longint; pt: TPoint;
         var dwEffect: Longint): HResult;
// Is called if the mouse cursor moves OVER (called on every mouse move) the
// TargeTWinControl. Even here may you influence if a drop can be accepted and the
// drop's effect if accepted. Because this function is very often called YOUR
// function should be very efficient programmed.
var ScrollCode:integer;
begin
     if FOwner.FScrollDetectOptions.FVertScrolling then
     begin
          DetermineScrollDir(true,ScrollCode);
          if ScrollCode>0 then
          begin
               if ((VertStartTimer.Enabled=false) and (VertScrollTimer.Enabled=false)) or
                  (FVScrollCode<>ScrollCode) then InitScroll(true,ScrollCode);
          end
          else if FVScrollCode<>0 then TermScroll(true);
     end
     else if FVScrollCode<>0 then TermScroll(true);
     if FOwner.FScrollDetectOptions.FHorzScrolling then
     begin
          DetermineScrollDir(false,ScrollCode);
          if ScrollCode>0 then
          begin
               if ((HorzStartTimer.Enabled=false) and (HorzScrollTimer.Enabled=false)) or
                  (FHScrollCode<>ScrollCode) then InitScroll(false,ScrollCode);
          end
          else if FHScrollCode<>0 then TermScroll(false);
     end
     else if FHScrollCode<>0 then TermScroll(false);
     if FAccept=false then dwEffect:=DropEffect_None;
     SuggestDropEffect(grfKeyState,dwEffect);
     if Assigned(FOwner.OnDragOver) then
        FOwner.OnDragOver(grfKeyState, FOwner.FDragDropControl.ScreenToClient(pt),
           dwEffect);
     if ((FOwner.FAcceptOwnDnD=false) and (FOwner.FOwnerIsSource)) or
        (FAccept=false) then dwEffect:=DropEffect_None;
     Result:=NOERROR;
end;

function TDropTarget.DragLeave: HResult;
// Removes target feedback and releases the data object.
begin
     TDragDrop(FOwner).FInternalSource:=nil;
     if Assigned(FOwner.OnDragLeave) then FOwner.OnDragLeave;
     FOwner.FAvailableDropEffects:=0;
     Result:=NOERROR;
     TermScroll(true);
     TermScroll(false);
end;

function TDropTarget.Drop(const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult;
// Instructs drop target to handle the datas which are dropped on it.
var Menu:HMenu;
     Cmd:Cardinal;
     mcursor:TCursor;
     KeyState:integer;

     function BuildMenuItemInfo(ACaption:string; ShowDefault:boolean;
        ACommand:UInt; ASeparator:boolean):TMenuItemInfo;
     begin
          with Result do
          begin
               // cbSize:=SizeOf(MenuItemInfo);
               cbSize:=44; //Required for Windows95
               fMask:=MIIM_ID or MIIM_STATE or MIIM_TYPE;
               if ASeparator then fType:=MFT_SEPARATOR
               else fType:=MFT_STRING;
               if ShowDefault then fState:=MFS_ENABLED or MFS_Default
               else fState:=MFS_ENABLED;
               wID:=ACommand;
               hSubMenu:=0;
               hbmpChecked:=0;
               hbmpUnchecked:=0;
               dwTypeData:=PChar(ACaption);
          end;
     end;

begin
     Result:=E_Fail;
     if FOwner.FContextMenu then KeyState:=grfKeyState or MK_RButton
     else KeyState:=grfKeyState or MK_LButton;
     if FAccept then SuggestDropEffect(KeyState,dwEffect)
     else dwEffect:=DropEffect_None;
     if assigned(FOwner.OnDragOver) then
        FOwner.OnDragOver(KeyState, FOwner.FDragDropControl.ScreenToClient(pt),
           dwEffect);
     if ((FOwner.FAcceptOwnDnD=false) and (FOwner.FOwnerIsSource)) or
        (FAccept=false) then dwEffect:=DropEffect_None;
     TermScroll(true);
     TermScroll(false);
     if (FOwner.DropHandler(DataObj, KeyState, pt, dwEffect)=false) then
     begin
          // Show popup menu?
          if FOwner.FContextMenu and FOwner.FShowPopupMenu and (dwEffect<>DropEffect_None) then
          begin
               Menu:=CreatePopupMenu;
               if (deMove in FOwner.FTargetEffectsSet) and
                  (FOwner.FAvailableDropEffects and DropEffect_Move<>0) then
                  InsertMenuItem(Menu, DWORD(-1), true,
                     BuildMenuItemInfo(MIMoveStr, dwEffect and DropEffect_Move<>0,
                     CmdMove, false));
               if (deCopy in FOwner.FTargetEffectsSet) and
                  (FOwner.FAvailableDropEffects and DropEffect_Copy<>0) then
                  InsertMenuItem(Menu, DWORD(-1), true,
                     BuildMenuItemInfo(MICopyStr, dwEffect and DropEffect_Copy<>0,
                     CmdCopy, false));
               if (deLink in FOwner.FTargetEffectsSet) and
                  (FOwner.FAvailableDropEffects and DropEffect_Link<>0) then
                  InsertMenuItem(Menu, DWORD(-1), true,
                     BuildMenuItemInfo(MILinkStr, dwEffect and DropEffect_Link<>0,
                     CmdLink, false));
               InsertMenuItem(Menu, DWORD(-1), true,
                  BuildMenuItemInfo('-', false, CmdSeparator, true));
               InsertMenuItem(Menu, DWORD(-1), true,
                  BuildMenuItemInfo(MIAbortStr, false, CmdAbort, false));
               // Add custom-menuitems ...
               FOwner.DoMenuPopup(self, Menu, DataObj, MinCustCmd, KeyState, pt);
               try
                 dwEffect:=DROPEFFECT_None;
                 Cmd:=Cardinal(TrackPopupMenuEx(Menu, TPM_LEFTALIGN or TPM_RIGHTBUTTON or TPM_RETURNCMD,
                    pt.x, pt.y, FOwner.DragDropControl.Handle, nil));
                 case Cmd of
                      CmdMove: dwEffect:=DROPEFFECT_Move;
                      CmdCopy: dwEffect:=DROPEFFECT_Copy;
                      CmdLink: dwEffect:=DROPEFFECT_Link;
                      CmdSeparator, CmdAbort:
                         dwEffect:=DROPEFFECT_None;
                      else // custom-menuitem was selected ...
                      begin
                           dwEffect:=DROPEFFECT_None;
                           if FOwner.DoMenuExecCmd(self, Menu, DataObj, Cmd, dwEffect) and
                              assigned(FOwner.FOnMenuSucceeded) then
                              FOwner.FOnMenuSucceeded(self, KeyState,
                              FOwner.FDragDropControl.ScreenToClient(pt), dwEffect);
                      end;
                 end;
               finally
                 FOwner.DoMenuDestroy(Self, Menu);
                 DestroyMenu(Menu);
               end;
          end;
          if assigned(FOwner.OnDrop) then
             FOwner.OnDrop(DataObj, KeyState,
                FOwner.FDragDropControl.ScreenToClient(pt), dwEffect);
          if dwEffect<>DROPEFFECT_None then
          begin
               if FOwner.FBTF Then
                  SetForegroundWindow((FOwner.Owner As TWinControl).Handle);
               TDragDrop(FOwner).FdwEffect:=dwEffect;
               TDragDrop(FOwner).FgrfKeyState:=KeyState;
               TDragDrop(FOwner).Fpt:=pt;
               if (FOwner.RenderDataOn=rdoDropAsync) or
                  (FOwner.RenderDataOn=rdoEnterAndDropAsync) then
               begin
                    TDragDrop(FOwner).FDataObj:=DataObj;
                    DataObj._AddRef;
               end
               else if (FOwner.RenderDataOn=rdoDropSync) or
                       (FOwner.RenderDataOn=rdoEnterAndDropSync) then
                    begin
                         // Set hourglass-cursor
                         mcursor:=Screen.Cursor;
                         Screen.Cursor:=crHourGlass;
                         try
                            RenderDropped(DataObj, KeyState, pt, dwEffect);
                         finally
                            // Set old cursor
                            Screen.Cursor:=mcursor;
                         end;
                    end;
               PostMessage(FOwner.DragDropControl.Handle,DDM_ProcessDropped,0,0);
               Result:=NOERROR;
          end
          else TDragDrop(FOwner).FInternalSource:=nil;
     end
     else
     begin
          TDragDrop(FOwner).FInternalSource:=nil;
          if assigned(FOwner.FOnDropHandlerSucceeded) then
             FOwner.FOnDropHandlerSucceeded(self, KeyState,
             FOwner.FDragDropControl.ScreenToClient(pt), dwEffect);
     end;
end;

procedure TDropTarget.RenderDropped(DataObj: IDataObject; grfKeyState: Longint;
  pt: TPoint; var dwEffect: longint);
begin
     // override, if you need ...
end;

// TScrollDetectArea methods ---------------------------------------------------

constructor TScrollDetectArea.Create(Control: TPersistent);
begin
     inherited Create;
     FControl:=Control;
end;

procedure TScrollDetectArea.AssignTo(Dest: TPersistent);
begin
     if Dest is TScrollDetectArea then
        with TScrollDetectArea(Dest) do
        begin
             FMargin:=Self.FMargin;
             FRange:=Self.FRange;
             Change;
        end
        else inherited AssignTo(Dest);
end;

procedure TScrollDetectArea.SetValue(Index: Integer;
  Value: word);
begin
     case Index of
          0: if Value<>FMargin then
             begin
                  FMargin:=Value;
                  Change;
             end;
          1: if Value<>FRange then
             begin
                  FRange:=Value;
                  Change;
             end;
     end;
end;

procedure TScrollDetectArea.Change;
begin
     if Assigned(FOnChange) then FOnChange(Self);
end;

// TScrollDetectOptions methods -------------------------------------------------

constructor TScrollDetectOptions.Create(Control: TDragDrop);
begin
     inherited Create;
     FControl:=Control;
     FScrollDelay:=100;
     FStartDelay:=750;
     FLeft:=TScrollDetectArea.Create(self);
     FLeft.Margin:=0;
     FLeft.Range:=10;
     FLeft.OnChange:=FOnChange;
     FTop:=TScrollDetectArea.Create(self);
     FTop.Margin:=0;
     FTop.Range:=10;
     FTop.OnChange:=FOnChange;
     FRight:=TScrollDetectArea.Create(self);
     FRight.Margin:=0;
     FRight.Range:=10;
     FRight.OnChange:=FOnChange;
     FBottom:=TScrollDetectArea.Create(self);
     FBottom.Margin:=0;
     FBottom.Range:=10;
     FBottom.OnChange:=FOnChange;
     FHorzScrolling:=false;
     FVertScrolling:=false;
     FHorzPageScroll:=false;
     FVertPageScroll:=false;
end;

destructor TScrollDetectOptions.Destroy;
begin
     FLeft.Free;
     FTop.Free;
     FRight.Free;
     FBottom.Free;
     inherited Destroy;
end;

procedure TScrollDetectOptions.AssignTo(Dest: TPersistent);
begin
     if Dest is TScrollDetectOptions then
        with TScrollDetectOptions(Dest) do
        begin
             FScrollDelay:=Self.FScrollDelay;
             FStartDelay:=Self.FStartDelay;
             FLeft.AssignTo(Self.FLeft);
             FTop.AssignTo(Self.FTop);
             FRight.AssignTo(Self.FRight);
             FBottom.AssignTo(Self.FBottom);
             Change;
        end
        else inherited AssignTo(Dest);
end;

procedure TScrollDetectOptions.SetValue(index:integer; Value: TScrollInterval);
begin
     if (Index=0) and (Value<>FScrollDelay) then
     begin
          FScrollDelay:=Value;
          Change;
     end;
     if (Index=1) and (Value<>FStartDelay) then
     begin
          FStartDelay:=Value;
          Change;
     end;
end;

procedure TScrollDetectOptions.Change;
begin
     if Assigned(FOnChange) then FOnChange(Self);
end;

// TDragDrop control ------------------------------------------------------

constructor TDragDrop.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     FDropTarget:=TDropTarget.Create(Self);
     FRegistered:=False;
     FDragDropControl:=nil;
     FBTF:=False;
     FAcceptOwnDnD:=false;
     FShowPopupMenu:=true;
     FDragDetectDelta:=10;
     FDragDetectStatus:=ddsNone;
     FRenderDataOn:=rdoDropSync;
     FCHCopy:=DefaultCursor;
     FCHMove:=DefaultCursor;
     FCHLink:=DefaultCursor;
     FCHScrollCopy:=DefaultCursor;
     FCHScrollMove:=DefaultCursor;
     FCHScrollLink:=DefaultCursor;
     FMessageHooked:=false;
     FAvailableDropEffects:=0;
     FTargetScrolling:=0;
     FSrcCompatibilityCheck:=[CheckLindex, CheckdwAspect];
     FScrollDetectOptions:=TScrollDetectOptions.Create(Self);
     FInternalSource:=nil;
end;

destructor TDragDrop.Destroy;
begin
     UnregisterTarget;
     UnhookMessageHandler(true);
     FDropTarget._Release;
     FDropTarget:=nil;
     FDragDropControl:=nil;
     FScrollDetectOptions.Free;
     inherited Destroy;
end;

procedure TDragDrop.WndMethod(var Msg: TMessage); // message-hook to receive DDM_ProcessDropped
var mcursor:TCursor;
begin
     with Msg do
     begin
          Result:=CallWindowProc(OldWndProc, DragDropControl.Handle, Msg, wParam, LParam);
          if (Msg=DDM_ProcessDropped) then
          begin
               if (RenderDataOn=rdoDropAsync) or (RenderDataOn=rdoEnterAndDropAsync) then
               begin
                    // Set hourglass-cursor
                    mcursor:=Screen.Cursor;
                    Screen.Cursor:=crHourGlass;
                    try
                      FDropTarget.RenderDropped(FDataObj, FgrfKeyState, Fpt, FdwEffect);
                      FDataObj._Release;
                   finally
                      // Set old cursor
                      Screen.Cursor:=mcursor;
                   end;
               end;
               if assigned(FOnProcessDropped) then
                  FOnProcessDropped(self, FgrfKeyState,
                     FDragDropControl.ScreenToClient(Fpt), FdwEffect);
               FAvailableDropEffects:=0;
               FInternalSource:=nil;
          end;
          case Msg of
            WM_Destroy:
              begin
                   if FRegistered then
                   begin
                        CoLockObjectExternal(FDropTarget, false, false);
                        if (FDragDropControl.HandleAllocated=false) or
                           (FDragDropControl.HandleAllocated and
                           (RevokeDragDrop(FDragDropControl.Handle)=S_OK)) then
                           FRegistered:=false;
                   end;
                   FMessageHooked:=false;
              end;
            WM_LBUTTONDOWN, WM_RBUTTONDOWN:
              begin
                   if FAutoDetectDnD and (FDragDetectStatus=ddsNone) and
                      (FSourceEffects<>0) then
                   begin
                        if Msg=WM_LBUTTONDOWN then FDragDetectStatus:=ddsLeft
                        else FDragDetectStatus:=ddsRight;
                        GetCursorPos(FDragDetectStart);
                        if assigned(FOnDragDetect) then
                           FOnDragDetect(wparam,
                             FDragDropControl.ScreenToClient(FDragDetectStart),
                             FDragDropControl.ScreenToClient(FDragDetectStart),
                             FDragDetectStatus);
                        if (MouseHookHandle<>0) then
                        begin // MouseHookProc is used by another component ...
                             UnHookWindowsHookEx(MouseHookHandle);
                             MouseHookHandle:=0;
                             if assigned(MouseHookDragDrop) then
                             begin
                                  MouseHookDragDrop.FDragDetectStatus:=ddsNone;
                                  if assigned(MouseHookDragDrop.FOnDragDetect) then
                                     MouseHookDragDrop.FOnDragDetect(wparam,
                                       MouseHookDragDrop.FDragDropControl.ScreenToClient(
                                          MouseHookDragDrop.FDragDetectStart),
                                       MouseHookDragDrop.FDragDropControl.ScreenToClient(
                                          FDragDetectStart),
                                       MouseHookDragDrop.FDragDetectStatus);
                             end;
                       end;
                        MouseHookDragDrop:=self;
                        MouseHookHandle:=SetWindowsHookEx(WH_MOUSE,MouseHookProc,LongWord(HInstance),0);
                   end;
              end;
            WM_HSCROLL:
              if LOWORD(wParam)<>SB_ENDSCROLL then FTargetScrolling:=FTargetScrolling or 1
              else FTargetScrolling:=FTargetScrolling and not 1;
            WM_VSCROLL:
              begin
                if LOWORD(wParam)<>SB_ENDSCROLL then FTargetScrolling:=FTargetScrolling or 2
                else FTargetScrolling:=FTargetScrolling and not 2;
              end;
            WM_MOUSEMOVE:
              if (MouseHookHandle<>0) and (wParam and (MK_LBUTTON or MK_RBUTTON)=0) then
              begin
                   UnHookWindowsHookEx(MouseHookHandle);
                   MouseHookHandle:=0;
                   if assigned(MouseHookDragDrop) then
                   begin
                        MouseHookDragDrop.FDragDetectStatus:=ddsNone;
                        if assigned(MouseHookDragDrop.FOnDragDetect) then
                           MouseHookDragDrop.FOnDragDetect(wparam,
                             MouseHookDragDrop.FDragDropControl.ScreenToClient(
                                MouseHookDragDrop.FDragDetectStart),
                             MouseHookDragDrop.FDragDropControl.ScreenToClient(
                                FDragDetectStart),
                             MouseHookDragDrop.FDragDetectStatus);
                   end;
                   MouseHookDragDrop:=nil;
              end;
          end;
     end;
end;

procedure TDragDrop.StartDnDDetection(Button: TMouseButton);
var grfKeyState: Longint;
begin
     if Button=mbLeft then FDragDetectStatus:=ddsLeft
     else if Button=mbRight then FDragDetectStatus:=ddsRight
          else
          begin
               FDragDetectStatus:=ddsNone;
               exit;
          end;
     GetCursorPos(FDragDetectStart);
     if HiWord(DWord(GetKeyState(VK_SHIFT)))<>0 then grfKeyState:=MK_SHIFT
     else grfKeyState:=0;
     if HiWord(DWord(GetKeyState(VK_CONTROL)))<>0 then
        grfKeyState:=grfKeyState or MK_CONTROL;
     if (MouseHookHandle<>0) then
     begin // MouseHookProc is used by another component ...
           UnHookWindowsHookEx(MouseHookHandle);
           MouseHookHandle:=0;
           if assigned(MouseHookDragDrop) then
           begin
                MouseHookDragDrop.FDragDetectStatus:=ddsNone;
                if assigned(MouseHookDragDrop.FOnDragDetect) then
                   MouseHookDragDrop.FOnDragDetect(grfKeyState,
                     MouseHookDragDrop.FDragDropControl.ScreenToClient(
                       MouseHookDragDrop.FDragDetectStart),
                     MouseHookDragDrop.FDragDropControl.ScreenToClient(FDragDetectStart),
                     MouseHookDragDrop.FDragDetectStatus);
           end;
     end;
     MouseHookDragDrop:=self;
     MouseHookHandle:=SetWindowsHookEx(WH_MOUSE,MouseHookProc,LongWord(HInstance),0);
     if assigned(FOnDragDetect) then
        FOnDragDetect(grfKeyState,
           FDragDropControl.ScreenToClient(FDragDetectStart),
           FDragDropControl.ScreenToClient(FDragDetectStart),
           FDragDetectStatus);
end;

procedure TDragDrop.Loaded;
// Methode which is called if all components are created - now, we can register
// the target control for drag-and-drop operations
begin
     inherited Loaded;
     if (FDragDropControl<>nil) and (csDesigning in ComponentState=false) then RegisterTarget;
end;

procedure TDragDrop.Notification(AComponent: TComponent; Operation: TOperation);
begin
     inherited Notification(AComponent,Operation);
     if (AComponent=FDragDropControl) and (Operation=opRemove) then
     begin
          UnregisterTarget;
          UnhookMessageHandler(true);
          FDragDropControl:=nil;
     end;
end;

function TDragDrop.RegisterTarget: Boolean;
// Methode for registering the DragDropControl for drag-and-drop oprations
begin
     Result:=false;
     try
        HookMessageHandler;
     finally
        // nothing to do
     end;
     if (not FRegistered) and (FTargetEffects <> 0) and (FDragDropControl <> nil) then
     begin
        try
           // CoLockObjectExternal crashes debugging intermittently in C++ Builder 2010
           {$IFNDEF IDE}
           // Ensure that drag-and-drop interface stays in memory
           CoLockObjectExternal(FDropTarget, True, False);
           {$ENDIF}
           if RegisterDragDrop(FDragDropControl.Handle, IDropTarget(FDropTarget))=S_OK then
           begin
                Result:=True;
                FRegistered:=True;
           end;
        except
           Result:=false;
           FRegistered:=false;
        end;
     end;
end;

function TDragDrop.UnRegisterTarget: Boolean;
begin
     Result:=false;
     if (FRegistered=false) or (FDragDropControl=nil) then exit;
     try
        UnHookMessageHandler(false);
        CoLockObjectExternal(FDropTarget, false, False);
        if (FDragDropControl.HandleAllocated=false) or
           (FDragDropControl.HandleAllocated and
           (RevokeDragDrop(FDragDropControl.Handle)=S_OK)) then
        begin
             FRegistered:=false;
             Result:=true;
        end;
     except
     end;
end;

procedure TDragDrop.HookMessageHandler;
begin
     if (FDragDropControl=nil) or (FDragDropControl.Handle=0) then exit;
     if (FMessageHooked=false) and ((FSourceEffects<>0) or (FTargetEffects<>0)) then
     begin
          WndProcPtr:=MakeObjectInstance(WndMethod);
          OldWndProc:=Pointer(SetWindowLong(FDragDropControl.Handle, GWL_WNDPROC,
             longint(WndProcPtr)));
          FMessageHooked:=true;
     end;
end;

procedure TDragDrop.UnhookMessageHandler(ForceUnhook:boolean);
begin
     if FMessageHooked and (ForceUnhook or ((FSourceEffects=0) and (FTargetEffects=0))) then
     begin
          begin
               SetWindowLong(FDragDropControl.Handle, GWL_WNDPROC, longint(OldWndProc));
               FreeObjectInstance(WndProcPtr);
               WndProcPtr:=nil;
               OldWndProc:=nil;
          end;
          FMessageHooked:=false;
     end;
end;

procedure TDragDrop.DoMenuPopup(Sender: TObject; AMenu: HMenu; DataObj: IDataObject; AMinCustCmd:integer;
   grfKeyState: Longint; pt: TPoint);
begin
     if assigned(FOnMenuPopup) then
        FOnMenuPopup(Sender, AMenu, DataObj, AMinCustCmd, grfKeyState,
           FDragDropControl.ScreenToClient(pt));
end;

function TDragDrop.DoMenuExecCmd(Sender: TObject; AMenu: HMenu; DataObj:IDataObject;
  Command:integer; var dwEffect: longint):boolean;
begin
     Result:=false;
     if assigned(FOnMenuExecCmd) then
        FOnMenuExecCmd(Sender, AMenu, DataObj, Command, dwEffect, Result);
end;

procedure TDragDrop.DoMenuDestroy(Sender:TObject; AMenu: HMenu);
begin
     if assigned(FOnMenuDestroy) then FOnMenuDestroy(Sender, AMenu);
end;

procedure TDragDrop.SetDragDropControl(WinControl: TWinControl);
begin
     if WinControl<>FDragDropControl then
     begin
          if FRegistered and (csDesigning in ComponentState=false) then
          begin
               UnhookMessageHandler(true);
               UnregisterTarget;
          end;
          FDragDropControl:=WinControl;
          if (csDesigning in ComponentState=false) then RegisterTarget;
     end;
end;

function TDragDrop.ExecuteOperation(DataObject:TDataObject): TDragResult;
var dwEffect: Longint;
    DropSource: TDropSource;
    pt: tpoint;
    grfKeyState:longint;
begin
     Result:=drInvalid;
     if (DataObject=nil) or (GInternalSource<>nil) then exit;
     GInternalSource:=self;
     if (FSourceEffects<>0) then
     begin
          if MouseHookHandle<>0 then
          begin
               UnHookWindowsHookEx(MouseHookHandle);
               MouseHookHandle:=0;
          end;
          FDragDetectStatus:=ddsDrag;
          DataObject.FCheckLindex:=CheckLindex in FSrcCompatibilityCheck;
          DataObject.FCheckdwAspect:=CheckdwAspect in FSrcCompatibilityCheck;
          try
             FOwnerIsSource:=true;
             try
                DropSource:=TDropSource.Create(self);
                try
                   if (DataObject<>nil) and (DragDropControl<>nil) and
                      (DoDragDrop(IDataObject(DataObject), DropSource,
                       FSourceEffects, dwEffect)=DRAGDROP_S_DROP) then
                   begin
                       case dwEffect and ((DropEffect_Copy or
                         DropEffect_Move or DropEffect_Link)) of
                            DropEffect_Copy: Result:=drCopy;
                            DropEffect_Move: Result:=drMove;
                            DropEffect_Link: Result:=drLink;
                            else
                            begin
                                 {MP dropped on no-drop location or }
                                 {cancelled by ddext after drop with move-effect}
                                 Result:=drInvalid;
                            end;
                       end;
                   end
                   else
                   begin
                     {MP cancelled by user }
                     Result:=drCancelled;
                   end;
                finally
                   DropSource._Release;
                end;
             except
                Result:=drInvalid;
                raise;
             end;
          finally
             FOwnerIsSource:=false;
             DataObject._Release;
          end;
          FDragDetectStatus:=ddsNone;
          if assigned(FOnDragDetect) then
          begin
               GetCursorPos(pt);
               if HiWord(DWord(GetKeyState(VK_SHIFT)))<>0 then grfKeyState:=MK_SHIFT
               else grfKeyState:=0;
               if HiWord(DWord(GetKeyState(VK_CONTROL)))<>0 then
                  grfKeyState:=grfKeyState or MK_CONTROL;
               FOnDragDetect(grfKeyState,
                  FDragDropControl.ScreenToClient(FDragDetectStart),
                  FDragDropControl.ScreenToClient(pt), FDragDetectStatus);
          end;
     end
     else
     begin
          FDragDetectStatus:=ddsNone;
          Result:=drCancelled;
     end;
     GInternalSource:=nil;
end;

function TDragDrop.Execute: TDragResult;
begin
     Result:=ExecuteOperation(CreateDataObject);
end;

procedure TDragDrop.SetSourceEffects(Values:TDropEffectSet);
begin
     FSourceEffectsSet:=Values;
     FSourceEffects:=0;
     if deCopy in Values then inc(FSourceEffects,DROPEFFECT_COPY);
     if deMove in Values then inc(FSourceEffects,DROPEFFECT_MOVE);
     if deLink in Values then inc(FSourceEffects,DROPEFFECT_LINK);
     if (csDesigning in ComponentState=false) and (csLoading in ComponentState=false) then
     begin
          if (csDesigning in ComponentState=false) and (FMessageHooked=false) and
             (FSourceEffects<>0) then HookMessageHandler;
          if (csDesigning in ComponentState=false) and (FMessageHooked=true) and
             (FSourceEffects=0) then UnhookMessageHandler(false);
     end;
end;

procedure TDragDrop.SetTargetEffects(Values:TDropEffectSet);
begin
     FTargetEffectsSet:=Values;
     FTargetEffects:=0;
     if deCopy in Values then inc(FTargetEffects,DROPEFFECT_COPY);
     if deMove in Values then inc(FTargetEffects,DROPEFFECT_MOVE);
     if deLink in Values then inc(FTargetEffects,DROPEFFECT_LINK);
     if (csDesigning in ComponentState=false) and (FRegistered=false) and
        (FTargetEffects<>0) then RegisterTarget;
     if (FRegistered=true) and (FTargetEffects=0) then
        UnRegisterTarget;
end;

procedure SetMenuItemsStrings;
begin
{MP}{     case SysLocale.PriLangID of
          LANG_GERMAN:
             begin
                  MICopyStr:='Hierher &kopieren';
                  MIMoveStr:='Hierher &verschieben';
                  MILinkStr:='Verknüpfung(en) hier &erstellen';
                  MIAbortStr:='&Abbrechen';
             end;
          LANG_FRENCH:
             begin // French
                  MICopyStr:='&Copier ici';
                  MIMoveStr:='&Transférer ici';
                  MILinkStr:='&Créer un ou des raccourci(s) ici';
                  MIAbortStr:='&Arrêt';
             end;
          LANG_ITALIAN:
             begin // Italian
                  MICopyStr:='&Copiare qui';
                  MIMoveStr:='&Muoversi qui';
                  MILinkStr:='&Scorciatoia(e) crea qui';
                  MIAbortStr:='&Terminazione';
             end;
          LANG_POLISH:
             begin // Polish
                  MICopyStr:='&Kopiuj tutaj';
                  MIMoveStr:='&Przenieœ tutaj';
                  MILinkStr:='Utwórz &skrót(y) tutaj';
                  MIAbortStr:='&Anuluj';
             end;
          LANG_PORTUGUESE:
             begin // Portuguese
                  MICopyStr:='&Copíe aqui';
                  MIMoveStr:='&Mova aqui';
                  MILinkStr:='&Atalho(s) cría aqui';
                  MIAbortStr:='&Aborto';
             end;
          LANG_SPANISH:
             begin // Spanish
                  MICopyStr:='&Copie aquí';
                  MIMoveStr:='&Muévase aquí';
                  MILinkStr:='&Atajo(s) crea aquí ';
                  MIAbortStr:='&Aborto';
             end;
          else
             begin // English
                  MICopyStr:=SCopyStr;'&Copy Here';
                  MIMoveStr:=SMoveStr;'&Move Here';
                  MILinkStr:=SLinkStr;'&Shortcut(s) Create Here';
                  MIAbortStr:=SAbortStr;'&Abort';
             end;
     end;}{/MP}
end;

function TDragDrop.CopyToClipboard:boolean;
var DataObject:IDataObject;
begin
     Result:=false;
     DataObject:=CreateDataObject;
     if DataObject=nil then exit;
     try
        Result:=OLESetClipBoard(DataObject)=S_Ok;
     finally
        DataObject._Release;
     end;
end;

function TDragDrop.GetFromClipboard:boolean;
var DataObject:IDataObject;
    pt:TPoint;
    dwEffect:longint;
begin
     Result:=OLEGetClipBoard(DataObject)=S_Ok;
     if Result then
     begin
          pt.x:=-1;
          pt.y:=-1;
          dwEffect:=DropEffect_Copy;
          FDropTarget.RenderDropped(DataObject, 0, pt, dwEffect);
     end;
end;

function TDragDrop.DropHandler(const dataObj: IDataObject; grfKeyState: Longint;
       pt: TPoint; var dwEffect: Longint): boolean;
begin
     Result:=false;
end;

// Register method -------------------------------------------------------------

procedure Register;
begin
  {MP}RegisterComponents({'Shell32'}'DragDrop', [TDragDrop]);
end;

// initialize/de-initialize the ole libary -------------------------------------

initialization
begin
     OleInitialize(nil);
     MouseHookHandle:=0;
     GInternalSource:=nil;
     SetMenuItemsStrings;
     // to avoid mix ups
     DDM_ProcessDropped:=RegisterWindowMessage('DDM_ProcessDropped');
end;

finalization
begin
     if MouseHookHandle<>0 then UnHookWindowsHookEx(MouseHookHandle);
     OleUninitialize;
end;

end.
