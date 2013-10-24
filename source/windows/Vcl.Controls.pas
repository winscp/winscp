// WORKAROUND
// (See InitControls)

{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2011 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit Vcl.Controls;

{$P+,S-,W-,R-,T-,H+,X+}

{$IF NOT DEFINED(CLR)}
{ WARN SYMBOL_PLATFORM OFF}
{$C PRELOAD}
{$IFEND}

interface

{$IF DEFINED(CLR)}
{$R Borland.Vcl.Controls.res}
{$ELSE}
{$R Controls.res}

{ Winapi.CommCtrl.hpp is not required in Controls.hpp }
(*$NOINCLUDE Winapi.CommCtrl *)

(*$HPPEMIT '#if defined(_VCL_ALIAS_RECORDS)' *)
(*$HPPEMIT '// Alias records for C++ code that cannot compile in STRICT mode yet.' *)
(*$HPPEMIT '#if !defined(STRICT)' *)
(*$HPPEMIT '#pragma alias "@Vcl@Controls@TWinControl@CreateParentedControl$qqrpv"="@Vcl@Controls@TWinControl@CreateParentedControl$qqrp6HWND__"' *)
(*$HPPEMIT '#pragma alias "@Vcl@Controls@TCustomHint@NCPaintHint$qqrp26Controls@TCustomHintWindowpv"="@Vcl@Controls@TCustomHint@NCPaintHint$qqrp26Controls@TCustomHintWindowp5HDC__"' *)
(*$HPPEMIT '#endif' *)
(*$HPPEMIT '#endif' *)

{$HPPEMIT '#pragma link "dwmapi.lib"'}
{$HPPEMIT '#pragma link "uxtheme.lib"'}

{$IFEND}

uses
  Winapi.Messages, Winapi.Windows, Winapi.MultiMon, System.Classes, System.SysUtils, Vcl.Graphics, Vcl.Menus,
  Winapi.CommCtrl, Winapi.Imm, Vcl.ImgList, Vcl.ActnList, System.SyncObjs, System.Types, System.UITypes
{$IF DEFINED(LINUX)}
  , WinUtils;
{$ELSE}
  ;
{$IFEND}

{ VCL control message IDs }

const
  CM_BASE                   = $B000;
{$IF DEFINED(CLR)}
  CM_CLROFFSET              = $100;
{$ELSE}
  CM_CLROFFSET              = $0; // Only applicable in CLR
{$IFEND}
  CM_ACTIVATE               = CM_BASE + 0;
  CM_DEACTIVATE             = CM_BASE + 1;
  CM_GOTFOCUS               = CM_BASE + 2;
  CM_LOSTFOCUS              = CM_BASE + 3;
  CM_CANCELMODE             = CM_BASE + CM_CLROFFSET + 4;
  CM_DIALOGKEY              = CM_BASE + 5;
  CM_DIALOGCHAR             = CM_BASE + 6;
{$IF NOT DEFINED(CLR)}
  CM_FOCUSCHANGED           = CM_BASE + 7;
{$IFEND}
  CM_PARENTFONTCHANGED      = CM_BASE + CM_CLROFFSET + 8;
  CM_PARENTCOLORCHANGED     = CM_BASE + 9;
  CM_HITTEST                = CM_BASE + 10;
  CM_VISIBLECHANGED         = CM_BASE + 11;
  CM_ENABLEDCHANGED         = CM_BASE + 12;
  CM_COLORCHANGED           = CM_BASE + 13;
  CM_FONTCHANGED            = CM_BASE + 14;
  CM_CURSORCHANGED          = CM_BASE + 15;
  CM_CTL3DCHANGED           = CM_BASE + 16;
  CM_PARENTCTL3DCHANGED     = CM_BASE + 17;
  CM_TEXTCHANGED            = CM_BASE + 18;
  CM_MOUSEENTER             = CM_BASE + 19;
  CM_MOUSELEAVE             = CM_BASE + 20;
  CM_MENUCHANGED            = CM_BASE + 21;
  CM_APPKEYDOWN             = CM_BASE + 22;
  CM_APPSYSCOMMAND          = CM_BASE + 23;
  CM_BUTTONPRESSED          = CM_BASE + 24;
  CM_SHOWINGCHANGED         = CM_BASE + 25;
  CM_ENTER                  = CM_BASE + 26;
  CM_EXIT                   = CM_BASE + 27;
  CM_DESIGNHITTEST          = CM_BASE + 28;
  CM_ICONCHANGED            = CM_BASE + 29;
  CM_WANTSPECIALKEY         = CM_BASE + 30;
  CM_INVOKEHELP             = CM_BASE + 31;
  CM_WINDOWHOOK             = CM_BASE + 32;
  CM_RELEASE                = CM_BASE + 33;
  CM_SHOWHINTCHANGED        = CM_BASE + 34;
  CM_PARENTSHOWHINTCHANGED  = CM_BASE + 35;
  CM_SYSCOLORCHANGE         = CM_BASE + 36;
  CM_WININICHANGE           = CM_BASE + 37;
  CM_FONTCHANGE             = CM_BASE + 38;
  CM_TIMECHANGE             = CM_BASE + 39;
  CM_TABSTOPCHANGED         = CM_BASE + 40;
  CM_UIACTIVATE             = CM_BASE + 41;
  CM_UIDEACTIVATE           = CM_BASE + 42;
  CM_DOCWINDOWACTIVATE      = CM_BASE + 43;
{$IF NOT DEFINED(CLR)}
  CM_CONTROLLISTCHANGE      = CM_BASE + 44;
  CM_GETDATALINK            = CM_BASE + 45;
{$IFEND}
  CM_CHILDKEY               = CM_BASE + 46;
  CM_DRAG                   = CM_BASE + CM_CLROFFSET + 47;
  CM_HINTSHOW               = CM_BASE + CM_CLROFFSET + 48;
  CM_DIALOGHANDLE           = CM_BASE + 49;
  CM_ISTOOLCONTROL          = CM_BASE + 50;
  CM_RECREATEWND            = CM_BASE + 51;
  CM_INVALIDATE             = CM_BASE + 52;
  CM_SYSFONTCHANGED         = CM_BASE + 53;
{$IF NOT DEFINED(CLR)}
  CM_CONTROLCHANGE          = CM_BASE + 54;
{$IFEND}
  CM_CHANGED                = CM_BASE + 55;
{$IF NOT DEFINED(CLR)}
  CM_DOCKCLIENT             = CM_BASE + 56;
  CM_UNDOCKCLIENT           = CM_BASE + 57;
  CM_FLOAT                  = CM_BASE + 58;
{$IFEND}
  CM_BORDERCHANGED          = CM_BASE + 59;
  CM_BIDIMODECHANGED        = CM_BASE + 60;
  CM_PARENTBIDIMODECHANGED  = CM_BASE + 61;
  CM_ALLCHILDRENFLIPPED     = CM_BASE + 62;
{$IF NOT DEFINED(CLR)}
  CM_ACTIONUPDATE           = CM_BASE + 63;
  CM_ACTIONEXECUTE          = CM_BASE + 64;
{$IFEND}
  CM_HINTSHOWPAUSE          = CM_BASE + 65;
  CM_DOCKNOTIFICATION       = CM_BASE + CM_CLROFFSET + 66;
  CM_MOUSEWHEEL             = CM_BASE + 67;
  CM_ISSHORTCUT             = CM_BASE + 68;
  CM_UPDATEACTIONS          = CM_BASE + 69;
{$IF DEFINED(LINUX)}
  CM_RAWX11EVENT            = CM_BASE + 69;
{$IFEND}
  CM_INVALIDATEDOCKHOST     = CM_BASE + CM_CLROFFSET + 70;
  CM_SETACTIVECONTROL       = CM_BASE + 71;
  CM_POPUPHWNDDESTROY       = CM_BASE + 72;
  CM_CREATEPOPUP            = CM_BASE + 73;
  CM_DESTROYHANDLE          = CM_BASE + 74;
  CM_MOUSEACTIVATE          = CM_BASE + 75;
{$IF DEFINED(MSWINDOWS)}
  CM_CONTROLLISTCHANGING    = CM_BASE + 76;
{$IFEND}
  CM_BUFFEREDPRINTCLIENT    = CM_BASE + 77;
  CM_UNTHEMECONTROL         = CM_BASE + 78;
  CM_DOUBLEBUFFEREDCHANGED  = CM_BASE + 79;
  CM_PARENTDOUBLEBUFFEREDCHANGED = CM_BASE + 80;
  CM_STYLECHANGED           = CM_BASE + 81;
  CM_THEMECHANGED           = CM_STYLECHANGED deprecated 'Use CM_STYLECHANGED';
  CM_GESTURE                = CM_BASE + 82;
  CM_CUSTOMGESTURESCHANGED  = CM_BASE + 83;
  CM_GESTUREMANAGERCHANGED  = CM_BASE + 84;
  CM_STANDARDGESTURESCHANGED = CM_BASE + 85;
  CM_INPUTLANGCHANGE         = CM_BASE + 86;
  CM_TABLETOPTIONSCHANGED    = CM_BASE + 87;
  CM_PARENTTABLETOPTIONSCHANGED = CM_BASE + 88;
  CM_CUSTOMSTYLECHANGED      = CM_BASE + 89;

{ VCL control notification IDs }

const
  CN_BASE              = $BC00;
  CN_CHARTOITEM        = CN_BASE + WM_CHARTOITEM;
  CN_COMMAND           = CN_BASE + WM_COMMAND;
  CN_COMPAREITEM       = CN_BASE + WM_COMPAREITEM;
  CN_CTLCOLORBTN       = CN_BASE + WM_CTLCOLORBTN;
  CN_CTLCOLORDLG       = CN_BASE + WM_CTLCOLORDLG;
  CN_CTLCOLOREDIT      = CN_BASE + WM_CTLCOLOREDIT;
  CN_CTLCOLORLISTBOX   = CN_BASE + WM_CTLCOLORLISTBOX;
  CN_CTLCOLORMSGBOX    = CN_BASE + WM_CTLCOLORMSGBOX;
  CN_CTLCOLORSCROLLBAR = CN_BASE + WM_CTLCOLORSCROLLBAR;
  CN_CTLCOLORSTATIC    = CN_BASE + WM_CTLCOLORSTATIC;
  CN_DELETEITEM        = CN_BASE + WM_DELETEITEM;
  CN_DRAWITEM          = CN_BASE + WM_DRAWITEM;
  CN_HSCROLL           = CN_BASE + WM_HSCROLL;
  CN_MEASUREITEM       = CN_BASE + WM_MEASUREITEM;
  CN_PARENTNOTIFY      = CN_BASE + WM_PARENTNOTIFY;
  CN_VKEYTOITEM        = CN_BASE + WM_VKEYTOITEM;
  CN_VSCROLL           = CN_BASE + WM_VSCROLL;
  CN_KEYDOWN           = CN_BASE + WM_KEYDOWN;
  CN_KEYUP             = CN_BASE + WM_KEYUP;
  CN_CHAR              = CN_BASE + WM_CHAR;
  CN_SYSKEYDOWN        = CN_BASE + WM_SYSKEYDOWN;
  CN_SYSCHAR           = CN_BASE + WM_SYSCHAR;
  CN_NOTIFY            = CN_BASE + WM_NOTIFY;

{ TAnchors elements }

const
  akLeft = System.UITypes.TAnchorKind.akLeft;
  akTop = System.UITypes.TAnchorKind.akTop;
  akRight = System.UITypes.TAnchorKind.akRight;
  akBottom = System.UITypes.TAnchorKind.akBottom;

{ TModalResult values }

const
  mrNone     = System.UITypes.mrNone;
  {$NODEFINE mrNone}
  mrOk       = System.UITypes.mrOk;
  {$NODEFINE mrOK}
  mrCancel   = System.UITypes.mrCancel;
  {$NODEFINE mrCancel}
  mrAbort    = System.UITypes.mrAbort;
  {$NODEFINE mrAbort}
  mrRetry    = System.UITypes.mrRetry;
  {$NODEFINE mrRetry}
  mrIgnore   = System.UITypes.mrIgnore;
  {$NODEFINE mrIgnore}
  mrYes      = System.UITypes.mrYes;
  {$NODEFINE mrYes}
  mrNo       = System.UITypes.mrNo;
  {$NODEFINE mrNo}
  mrAll      = System.UITypes.mrAll;
  {$NODEFINE mrAll}
  mrNoToAll  = System.UITypes.mrNoToAll;
  {$NODEFINE mrNoToAll}
  mrYesToAll = System.UITypes.mrYesToAll;
  {$NODEFINE mrYesToAll}
  mrClose    = System.UITypes.mrClose;
  {$NODEFINE mrClose}

{$HPPEMIT OPENNAMESPACE}
{$HPPEMIT 'using ::System::Uitypes::mrNone;'}
{$HPPEMIT 'using ::System::Uitypes::mrOk;'}
{$HPPEMIT 'using ::System::Uitypes::mrCancel;'}
{$HPPEMIT 'using ::System::Uitypes::mrAbort;'}
{$HPPEMIT 'using ::System::Uitypes::mrRetry;'}
{$HPPEMIT 'using ::System::Uitypes::mrIgnore;'}
{$HPPEMIT 'using ::System::Uitypes::mrYes;'}
{$HPPEMIT 'using ::System::Uitypes::mrNo;'}
{$HPPEMIT 'using ::System::Uitypes::mrAll;'}
{$HPPEMIT 'using ::System::Uitypes::mrNoToAll;'}
{$HPPEMIT 'using ::System::Uitypes::mrYesToAll;'}
{$HPPEMIT 'using ::System::Uitypes::mrClose;'}
{$HPPEMIT CLOSENAMESPACE}

type
  TModalResult = System.UITypes.TModalResult;
  {$NODEFINE TModalResult}
  {$HPPEMIT OPENNAMESPACE}
  {$HPPEMIT 'using ::System::Uitypes::TModalResult;'}
  {$HPPEMIT CLOSENAMESPACE}

function IsPositiveResult(const AModalResult: TModalResult): Boolean; inline;
function IsNegativeResult(const AModalResult: TModalResult): Boolean; inline;
function IsAbortResult(const AModalResult: TModalResult): Boolean; inline;
function IsAnAllResult(const AModalResult: TModalResult): Boolean; inline;
function StripAllFromResult(const AModalResult: TModalResult): TModalResult; inline;

{ Cursor identifiers }

type
  TCursor = System.UITypes.TCursor;
  {$NODEFINE TCursor}
  {$HPPEMIT OPENNAMESPACE}
  {$HPPEMIT 'using ::System::Uitypes::TCursor;'}
  {$HPPEMIT CLOSENAMESPACE}

const
  crDefault     = System.UITypes.crDefault;
  {$NODEFINE crDefault}
  crNone        = System.UITypes.crNone;
  {$NODEFINE crNone}
  crArrow       = System.UITypes.crArrow;
  {$NODEFINE crArrow}
  crCross       = System.UITypes.crCross;
  {$NODEFINE crCross}
  crIBeam       = System.UITypes.crIBeam;
  {$NODEFINE crIBeam}
  crSize        = System.UITypes.crSize;
  {$NODEFINE crSize}
  crSizeNESW    = System.UITypes.crSizeNESW;
  {$NODEFINE crSizeNESW}
  crSizeNS      = System.UITypes.crSizeNS;
  {$NODEFINE crSizeNS}
  crSizeNWSE    = System.UITypes.crSizeNWSE;
  {$NODEFINE crSizeNWSE}
  crSizeWE      = System.UITypes.crSizeWE;
  {$NODEFINE crSizeWE}
  crUpArrow     = System.UITypes.crUpArrow;
  {$NODEFINE crUpArrow}
  crHourGlass   = System.UITypes.crHourGlass;
  {$NODEFINE crHourGlass}
  crDrag        = System.UITypes.crDrag;
  {$NODEFINE crDrag}
  crNoDrop      = System.UITypes.crNoDrop;
  {$NODEFINE crNoDrop}
  crHSplit      = System.UITypes.crHSplit;
  {$NODEFINE crHSplit}
  crVSplit      = System.UITypes.crVSplit;
  {$NODEFINE crVSplit}
  crMultiDrag   = System.UITypes.crMultiDrag;
  {$NODEFINE crMultiDrag}
  crSQLWait     = System.UITypes.crSQLWait;
  {$NODEFINE crSQLWait}
  crNo          = System.UITypes.crNo;
  {$NODEFINE crNo}
  crAppStart    = System.UITypes.crAppStart;
  {$NODEFINE crAppStart}
  crHelp        = System.UITypes.crHelp;
  {$NODEFINE crHelp}
  crHandPoint   = System.UITypes.crHandPoint;
  {$NODEFINE crHandPoint}
  crSizeAll     = System.UITypes.crSizeAll;
  {$NODEFINE crSizeAll}

  {$HPPEMIT OPENNAMESPACE}
  {$HPPEMIT 'using ::System::Uitypes::crDefault;'}
  {$HPPEMIT 'using ::System::Uitypes::crNone;'}
  {$HPPEMIT 'using ::System::Uitypes::crArrow;'}
  {$HPPEMIT 'using ::System::Uitypes::crCross;'}
  {$HPPEMIT 'using ::System::Uitypes::crIBeam;'}
  {$HPPEMIT 'using ::System::Uitypes::crSize;'}
  {$HPPEMIT 'using ::System::Uitypes::crSizeNESW;'}
  {$HPPEMIT 'using ::System::Uitypes::crSizeNS;'}
  {$HPPEMIT 'using ::System::Uitypes::crSizeNWSE;'}
  {$HPPEMIT 'using ::System::Uitypes::crSizeWE;'}
  {$HPPEMIT 'using ::System::Uitypes::crUpArrow;'}
  {$HPPEMIT 'using ::System::Uitypes::crHourGlass;'}
  {$HPPEMIT 'using ::System::Uitypes::crDrag;'}
  {$HPPEMIT 'using ::System::Uitypes::crNoDrop;'}
  {$HPPEMIT 'using ::System::Uitypes::crHSplit;'}
  {$HPPEMIT 'using ::System::Uitypes::crVSplit;'}
  {$HPPEMIT 'using ::System::Uitypes::crMultiDrag;'}
  {$HPPEMIT 'using ::System::Uitypes::crSQLWait;'}
  {$HPPEMIT 'using ::System::Uitypes::crNo;'}
  {$HPPEMIT 'using ::System::Uitypes::crAppStart;'}
  {$HPPEMIT 'using ::System::Uitypes::crHelp;'}
  {$HPPEMIT 'using ::System::Uitypes::crHandPoint;'}
  {$HPPEMIT 'using ::System::Uitypes::crSizeAll;'}
  {$HPPEMIT CLOSENAMESPACE}

const
  // Standard gesture id's
  sgiNoGesture       = 0;
  sgiLeft            = 1;
  sgiRight           = 2;
  sgiUp              = 3;
  sgiDown            = 4;
  sgiUpLeft          = 5;
  sgiUpRight         = 6;
  sgiDownLeft        = 7;
  sgiDownRight       = 8;
  sgiLeftUp          = 9;
  sgiLeftDown        = 10;
  sgiRightUp         = 11;
  sgiRightDown       = 12;
  sgiUpDown          = 13;
  sgiDownUp          = 14;
  sgiLeftRight       = 15;
  sgiRightLeft       = 16;
  sgiUpLeftLong      = 17;
  sgiUpRightLong     = 18;
  sgiDownLeftLong    = 19;
  sgiDownRightLong   = 20;
  sgiScratchout      = 21;
  sgiTriangle        = 22;
  sgiSquare          = 23;
  sgiCheck           = 24;
  sgiCurlicue        = 25;
  sgiDoubleCurlicue  = 26;
  sgiCircle          = 27;
  sgiDoubleCircle    = 28;
  sgiSemiCircleLeft  = 29;
  sgiSemiCircleRight = 30;
  sgiChevronUp       = 31;
  sgiChevronDown     = 32;
  sgiChevronLeft     = 33;
  sgiChevronRight    = 34;
  sgiFirst           = sgiLeft;
  sgiLast            = sgiChevronRight;

  // Recorded custom gestures ID range
  cgiFirst = -512;
  cgiLast  = -1;

  // Registered custom gestures ID range
  rgiFirst = -1024;
  rgiLast  = -513;

  // Interactive gesture id's (maps to Windows 7's WM_GESTURE)
  igiFirst           = 256;
  igiLast            = 511;
  igiBegin           = GID_BEGIN + igiFirst;
  igiEnd             = GID_END + igiFirst;
  igiZoom            = GID_ZOOM + igiFirst;
  igiPan             = GID_PAN + igiFirst;
  igiRotate          = GID_ROTATE + igiFirst;
  igiTwoFingerTap    = GID_TWOFINGERTAP + igiFirst;
  igiPressAndTap     = GID_PRESSANDTAP + igiFirst;

  // Notifications for CM_CUSTOMGESTURESCHANGED and CM_STANDARDGESTURESCHANGED
  gcnRefreshAll = 0;
  gcnAdded      = 1;  // LParam indicates gesture ID
  gcnRemoved    = 2;  // LParam indicates gesture ID
  gcnModified   = 3;  // LParam indicates gesture ID

{ Enumeration element forwarders }
const
  dmAutomatic   = System.UITypes.TDragMode.dmAutomatic;
  dmManual      = System.UITypes.TDragMode.dmManual;
  dsDragEnter   = System.UITypes.TDragState.dsDragEnter;
  dsDragLeave   = System.UITypes.TDragState.dsDragLeave;
  dsDragMove    = System.UITypes.TDragState.dsDragMove;
  dkDrag        = System.UITypes.TDragKind.dkDrag;
  dkDock        = System.UITypes.TDragKind.dkDock;
  maDefault     = System.UITypes.TMouseActivate.maDefault;
  maActivate    = System.UITypes.TMouseActivate.maActivate;
  maActivateAndEat = System.UITypes.TMouseActivate.maActivateAndEat;
  maNoActivate  = System.UITypes.TMouseActivate.maNoActivate;
  maNoActivateAndEat = System.UITypes.TMouseActivate.maNoActivateAndEat;

  mbLeft        = System.UITypes.TMouseButton.mbLeft;
  mbRight       = System.UITypes.TMouseButton.mbRight;
  mbMiddle      = System.UITypes.TMouseButton.mbMiddle;

type
  // Corresponds to GF_* flags
  TInteractiveGestureFlag = (gfBegin, gfInertia, gfEnd);
  TInteractiveGestureFlags = set of TInteractiveGestureFlag;

  TInteractiveGesture = (igZoom, igPan, igRotate, igTwoFingerTap, igPressAndTap);
  TInteractiveGestures = set of TInteractiveGesture;

  TInteractiveGestureOption = (igoPanSingleFingerHorizontal,
    igoPanSingleFingerVertical, igoPanInertia, igoPanGutter,
    igoParentPassthrough);
  TInteractiveGestureOptions = set of TInteractiveGestureOption;

type

{ .NET / Win32 source compatibility declarations }

{$IF DEFINED(CLR)}
  TCustomData = TObject;
  TCustomLongData = TObject deprecated 'Use THelpEventData, TListBoxItemData or NativeInt';
  TWindowProcPtr = IntPtr;
{$ELSE}
  TCustomData = Pointer;
  TCustomLongData = NativeInt deprecated 'Use THelpEventData, TListBoxItemData or NativeInt';
  TWindowProcPtr = Pointer;
{$IFEND}

{ Forward declarations }

  TDragObject = class;
  TControl = class;
  TWinControl = class;
  TDragImageList = class;
  THintWindow = class;
  TCustomHint = class;
  TBalloonHint = class;

  TWinControlClass = class of TWinControl;
  THintWindowClass = class of THintWindow;

{ VCL control message records }

  TCMActivate = TWMNoParams;
  TCMDeactivate = TWMNoParams;
  TCMGotFocus = TWMNoParams;
  TCMLostFocus = TWMNoParams;
  TCMDialogKey = TWMKey;
  TCMDialogChar = TWMKey;
  TCMHitTest = TWMNCHitTest;
  TCMEnter = TWMNoParams;
  TCMExit = TWMNoParams;
  TCMDesignHitTest = TWMMouse;
  TCMWantSpecialKey = TWMKey;

{$IF DEFINED(CLR)}
  TCMObjectMsg = class(TWMNoParams)
  private
    FWHandle: GCHandle;
    FLHandle: GCHandle;
  strict protected
    function GetWObject: TObject;
    procedure SetWObject(const Value: TObject);
    function GetLObject: TObject;
    procedure SetLObject(const Value: TObject);
    procedure FreeHandles; virtual;
    procedure Finalize; override;
  end;

  TCMControlMsg = class(TCMObjectMsg)
  protected
    function GetControl: TControl;
    procedure SetControl(const Value: TControl);
  public
    property Control: TControl read GetControl write SetControl; // lParam
  end;

  TCMWinControlMsg = class(TCMControlMsg)
  protected
    function GetControl: TWinControl;
    procedure SetControl(const Value: TWinControl);
  public
    property Control: TWinControl read GetControl write SetControl; // lParam
  end;

  TCMMouseWheel = class(TWMPosition)
  protected
    function GetShiftState: TShiftState;
    procedure SetShiftState(const Value: TShiftState);
  public
    property ShiftState: TShiftState read GetShiftState write SetShiftState;    // wParam and $FF
    property WheelDelta: SmallInt read GetWParamHiSmall write SetWParamHiSmall;
  end;
{$ELSE}
  TCMMouseWheel = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    ShiftState: TShiftState;
    WheelDelta: SmallInt;
    ShiftStateWheel: TDWordFiller;
    case Integer of
      0: (
        XPos: Smallint;
        YPos: Smallint;
        XYPos: TDWordFiller);
      1: (
        Pos: TSmallPoint;
        PosFiller: TDWordFiller;
        Result: LRESULT);
  end;
{$IFEND}

{$IF DEFINED(CLR)}
  TCMCancelMode = class(TCMControlMsg)
  public
    property Sender: TControl read GetControl write SetControl;
  end;

  TControlListItem = record
    Control: TControl;
    Parent: TWinControl;
  end;
{$ELSE}
  TCMCancelMode = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    Unused: WPARAM;
    Sender: TControl;
    Result: LRESULT;
  end;

  TCMFocusChanged = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    Unused: WPARAM;
    Sender: TWinControl;
    Result: LRESULT;
  end;

  TCMControlListChange = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    Control: TControl;
    Inserting: LongBool;
    InsertingFiller: TDWordFiller;
    Result: LRESULT;
  end;

  PControlListItem = ^TControlListItem;
  TControlListItem = record
    Control: TControl;
    Parent: TWinControl;
  end;

  TCMControlListChanging = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    ControlListItem: PControlListItem;
    Inserting: LongBool;
    InsertingFiller: TDWordFiller;
    Result: LRESULT;
  end;
{$IFEND}

{$IF DEFINED(CLR)}
  TCMChildKey = class(TWMNoParams)
  public
    property CharCode: Word read GetWParamLo write SetWParamLo;
    property Sender: HWND read GetLParamHWND write SetLParamHWND;
  end;
{$ELSE}
  TCMChildKey = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    CharCode: Word;
    CharCodeFiller: TWordFiller;
    Sender: TWinControl;
    Result: LRESULT;
  end;

  TCMControlChange = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    Control: TControl;
    Inserting: LongBool;
    InsertingFiller: TDWordFiller;
    Result: LRESULT;
  end;
{$IFEND}

{$IF DEFINED(CLR)}
  TCMChanged = class(TWMNoParams)
  public
    property Child: LPARAM read GetLParam write SetLParam; // hash of sender
  end;
{$ELSE}
  TCMChanged = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    Unused: Longint;
    UnusedFiller: TDWordFiller;
    Child: TControl;
    Result: LRESULT;
  end;
{$IFEND}


{$IF NOT DEFINED(CLR)}
  PHintInfo = ^THintInfo;
{$IFEND}
  THintInfo = record
    HintControl: TControl;
    HintWindowClass: THintWindowClass;
    HintPos: TPoint;
    HintMaxWidth: Integer;
    HintColor: TColor;
    CursorRect: TRect;
    CursorPos: TPoint;
    ReshowTimeout: Integer;
    HideTimeout: Integer;
    HintStr: string;
    HintData: TCustomData;
  end;

{$IF DEFINED(CLR)}
  TCMHintInfo = class(TCMObjectMsg)
  private
    function GetHintInfo: THintInfo;
    procedure SetHintInfo(Value: THintInfo);  // was HintInfo: PHintInfo
  published
    property HintInfo: THintInfo read GetHintInfo write SetHintInfo;
  end;
{$IFEND}

{$IF DEFINED(CLR)}
  TCMHintShow = TCMHintInfo;
{$ELSE}
  TCMHintShow = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    Reserved: NativeInt;
    HintInfo: PHintInfo;
    Result: LRESULT;
  end;
{$IFEND}

{$IF DEFINED(CLR)}
  TCMParentFontChanged = class(TCMObjectMsg)
  protected
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
  public
    property WParam: WPARAM read GetWParam write SetWParam;
    property Font: TFont read GetFont write SetFont; // lParam
  end;
{$ELSE}
  TCMParentFontChanged = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    WParam: WPARAM;
    case Integer of
      0: (LParam: LPARAM);
      1: (Font: TFont;
          Result: LRESULT);
  end;
{$IFEND}


  TDragMessage = (dmDragEnter, dmDragLeave, dmDragMove, dmDragDrop, dmDragCancel,
    dmFindTarget);

{$IF DEFINED(CLR)}
  TDragRec = class
    Pos: TPoint;
    Source: TDragObject;
    Target: TObject;
    Docking: Boolean;
  end;

  TCMDrag = class(TCMObjectMsg)
  private
    function GetDragMessage: TDragMessage;
    function GetLParamDragRec: TDragRec;
    procedure SetDragMessage(Value: TDragMessage);
    procedure SetLParamDragRec(Value: TDragRec);
  public
    destructor Destroy; override;
    property DragMessage: TDragMessage read GetDragMessage write SetDragMessage;  // wParam and $FF
    property DragRec: TDragRec read GetLParamDragRec write SetLParamDragRec;
  end;
{$ELSE}
  PDragRec = ^TDragRec;
  TDragRec = record
    Pos: TPoint;
    Source: TDragObject;
    Target: Pointer;
    Docking: Boolean;
  end;

  TCMDrag = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    DragMessage: TDragMessage;
    Unused1: Byte;
    Unused2: TWordFiller;
    DragRec: PDragRec;
    Result: LRESULT;
  end;
{$IFEND}


  TDragDockObject = class;

{$IF NOT DEFINED(CLR)}
  PDockNotifyRec = ^TDockNotifyRec;
{$IFEND}
  TDockNotifyRec = record
    ClientMsg: Cardinal;
    ClientMsgFiller: TDWordFiller;
    MsgWParam: WPARAM;
    MsgLParam: LPARAM;
  end;

{$IF DEFINED(CLR)}
  TCMDockNotification = class(TCMControlMsg)
  private
    function GetNotifyRect: TDockNotifyRec;
    procedure SetNotifyRec(Value: TDockNotifyRec);
  public
    class operator Explicit(Message: TMessage): TCMDockNotification;
    property NotifyRec: TDockNotifyRec read GetNotifyRect write SetNotifyRec;
    property Client: TControl read GetControl write SetControl;
  end;
{$IFEND}

{$IF NOT DEFINED(CLR)}
  TCMDockClient = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    DockSource: TDragDockObject;
    MousePos: TSmallPoint;
    MousePosFiller: TDWordFiller;
    Result: LRESULT;
  end;

  TCMUnDockClient = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    NewTarget: TControl;
    Client: TControl;
    Result: LRESULT;
  end;

  TCMFloat = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    Reserved: WPARAM;
    DockSource: TDragDockObject;
    Result: LRESULT;
  end;

  TCMDockNotification = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    Client: TControl;
    NotifyRec: PDockNotifyRec;
    Result: LRESULT;
  end;
{$IFEND}

{$IF NOT DEFINED(CLR)}
  PPopupFormInfo = ^TPopupFormInfo;
{$IFEND}
  [StructLayout(LayoutKind.Sequential)]
  TPopupFormInfo = record
    PopupID: Integer;
    PopupWnd: HWND;
    IsPopup: Boolean;
  end;

{$IF DEFINED(CLR)}
  TCMPopupHWndDestroy = class(TWMNoParams)
  protected
    function GetPopupFormInfo: TPopupFormInfo;
    procedure SetPopupFormInfo(Value: TPopupFormInfo);
  public
    class operator Explicit(Message: TMessage): TCMPopupHWndDestroy;
    property PopupFormInfo: TPopupFormInfo read GetPopupFormInfo write SetPopupFormInfo;
    property PopupControlWnd: HWND read GetLParamHWND write SetLParamHWND;
  end;

  TCMCreatePopup = class(TWMNoParams)
  public
    class operator Explicit(Message: TMessage): TCMCreatePopup;
    property PopupID: WPARAM read GetWParam write SetWParam;
    property OwnerWnd: HWND read GetLParamHWND write SetLParamHWND;
  end;
{$IFEND}

{$IF NOT DEFINED(CLR)}
  TCMPopupHWndDestroy = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    PopupFormInfo: PPopupFormInfo;
    PopupControlWnd: HWND;
    Result: LRESULT;
  end;

  TCMCreatePopup = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    PopupID: Integer;
    PopupUDFiller: TDWordFiller;
    OwnerWnd: HWND;
    Result: LRESULT;
  end;
{$IFEND}

  TAlign = (alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom);

  TAlignSet = set of TAlign;

{ Dragging objects }

{$IF DEFINED(CLR)}
    TDragTarget = TObject;
{$ELSE}
    TDragTarget = Pointer;
{$IFEND}

  TDragObject = class(TObject)
  private
    FAlwaysShowDragImages: Boolean;
    FCancelling: Boolean;
    FDragHandle: HWND;
    FDragPos: TPoint;
    FDragTarget: TDragTarget;
    FDragTargetPos: TPoint;
    FDropped: Boolean;
    FMouseDeltaX: Double;
    FMouseDeltaY: Double;
    FRightClickCancels: Boolean;
    function Capture: HWND;
    procedure ReleaseCapture(Handle: HWND);
  protected
    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); virtual;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; virtual;
    function GetDragImages: TDragImageList; virtual;
    procedure WndProc(var Msg: TMessage); virtual;
    procedure MainWndProc(var Message: TMessage);
  public
    procedure Assign(Source: TDragObject); virtual;
    function GetName: string; virtual;
    procedure HideDragImage; virtual;
    function Instance: THandle; virtual;
    procedure ShowDragImage; virtual;
{$IF NOT DEFINED(CLR)}
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
{$IFEND}
    property AlwaysShowDragImages: Boolean read FAlwaysShowDragImages write FAlwaysShowDragImages;
    property Cancelling: Boolean read FCancelling write FCancelling;
    property DragHandle: HWND read FDragHandle write FDragHandle;
    property DragPos: TPoint read FDragPos write FDragPos;
    property DragTarget: TDragTarget read FDragTarget write FDragTarget;
    property DragTargetPos: TPoint read FDragTargetPos write FDragTargetPos;
    property Dropped: Boolean read FDropped;
    property MouseDeltaX: Double read FMouseDeltaX;
    property MouseDeltaY: Double read FMouseDeltaY;
    property RightClickCancels: Boolean read FRightClickCancels write FRightClickCancels;
  end;

  TDragObjectClass = class of TDragObject;

{$IF NOT DEFINED(CLR)}
  TDragObjectEx = class(TDragObject)
  public
    procedure BeforeDestruction; override;
  end;
{$IFEND}

  TBaseDragControlObject = class(TDragObject)
  private
    FControl: TControl;
  protected
    procedure EndDrag(Target: TObject; X, Y: Integer); virtual;
    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); override;
  public
    constructor Create(AControl: TControl); virtual;
    procedure Assign(Source: TDragObject); override;
    property Control: TControl read FControl write FControl;
  end;

  TDragControlObject = class(TBaseDragControlObject)
  protected
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    function GetDragImages: TDragImageList; override;
  public
    procedure HideDragImage; override;
    procedure ShowDragImage; override;
  end;

{$IF NOT DEFINED(CLR)}
  TDragControlObjectEx = class(TDragControlObject)
  public
    procedure BeforeDestruction; override;
  end;
{$IFEND}

  TDragDockObject = class(TBaseDragControlObject)
  private
    FBrush: TBrush;
    FDockRect: TRect;
    FDropAlign: TAlign;
    FDropOnControl: TControl;
    FEraseDockRect: TRect;
    FFloating: Boolean;
    procedure SetBrush(Value: TBrush);
  protected
    procedure AdjustDockRect(ARect: TRect); virtual;
    procedure DrawDragDockImage; virtual;
    procedure EndDrag(Target: TObject; X, Y: Integer); override;
    procedure EraseDragDockImage; virtual;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    function GetFrameWidth: Integer; virtual;
    function GetEraseWhenMoving: Boolean; virtual;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
    procedure Assign(Source: TDragObject); override;
    property Brush: TBrush read FBrush write SetBrush;
    property DockRect: TRect read FDockRect write FDockRect;
    property DropAlign: TAlign read FDropAlign;
    property DropOnControl: TControl read FDropOnControl;
    property EraseDockRect: TRect read FEraseDockRect write FEraseDockRect;
    property EraseWhenMoving: Boolean read GetEraseWhenMoving;
    property Floating: Boolean read FFloating write FFloating;
    property FrameWidth: Integer read GetFrameWidth;
  end;

{$IF NOT DEFINED(CLR)}
  TDragDockObjectEx = class(TDragDockObject)
  public
    procedure BeforeDestruction; override;
  end;
{$IFEND}

{ Controls }

  { THwndWrapper }

{$IF DEFINED(CLR)}
  TFinalizeHWNDNotify = procedure(Handle: HWND) of object;

  THwndWrapper = class
  private
    FNotifies: TList;
  strict protected
    procedure Finalize; override;
    procedure CallNotifies;
  public
    FObjInstance: TFNWndProc;
    Handle: HWND;
    procedure RegisterFinalizeNotify(Proc: TFinalizeHWNDNotify);
    procedure UnregisterFinalizeNotify(Proc: TFinalizeHWNDNotify);
  end;
{$IFEND}

{$IF DEFINED(CLR)}
  TCanvasDC = class
  private
    FDC: HDC;
    FWindowHandle: THWndWrapper;
    FNotifyDelegate: TFinalizeHWNDNotify;
  strict protected
    procedure FinalizeNotify(Handle: HWND);
    procedure Finalize; override;
  public
    constructor Create;
  end;
{$ELSE}
  TCanvasDC = HDC;
{$IFEND}

  { TControlCanvas }

  TControlCanvas = class(TCanvas)
  private
    FControl: TControl;
    FDeviceContext: TCanvasDC;
{$IF NOT DEFINED(CLR)}
    FWindowHandle: HWnd;
{$IFEND}
  procedure SetControl(AControl: TControl);
  protected
    procedure CreateHandle; override;
  public
    destructor Destroy; override;
    procedure FreeHandle;
    procedure UpdateTextFlags;
    property Control: TControl read FControl write SetControl;
  end;

{ TControlAction }

  TCustomControlAction = class(TCustomAction)
  private
    FDropdownMenu: TPopupMenu;
    FPopupMenu: TPopupMenu;
    FEnableDropdown: Boolean;
    procedure SetDropdownMenu(Value: TPopupMenu);
    procedure SetEnableDropdown(Value: Boolean);
    procedure SetPopupMenu(Value: TPopupMenu);
  public
    property DropdownMenu: TPopupMenu read FDropdownMenu write SetDropdownMenu;
    property EnableDropdown: Boolean read FEnableDropdown write SetEnableDropdown default False;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
  end;

{ TControlAction }

  TControlAction = class(TCustomControlAction)
  published
    property AutoCheck;
    property Caption;
    property Checked;
    property DropdownMenu;
    property Enabled;
    property EnableDropdown;
    property GroupIndex;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property PopupMenu;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property OnExecute;
    property OnHint;
    property OnUpdate;
  end;

{ TControlActionLink }

  TControlActionLink = class(TActionLink)
  protected
    FClient: TControl;
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsDropdownMenuLinked: Boolean; virtual;
    function IsEnabledLinked: Boolean; override;
    function IsEnableDropdownLinked: Boolean; virtual;
    function IsHelpLinked: Boolean;  override;
    function IsHintLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    function IsPopupMenuLinked: Boolean; virtual;
    function DoShowHint(var HintStr: string): Boolean; virtual;
    procedure SetCaption(const Value: string); override;
    procedure SetDropdownMenu(Value: TPopupMenu); virtual;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetEnableDropdown(Value: Boolean); virtual;
    procedure SetHint(const Value: string); override;
    procedure SetHelpContext(Value: THelpContext); override;
    procedure SetHelpKeyword(const Value: string); override;
    procedure SetHelpType(Value: THelpType); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
    procedure SetPopupMenu(Value: TPopupMenu); virtual;
  end;

  TControlActionLinkClass = class of TControlActionLink;

{ TControl }

  TControlState = set of (csLButtonDown, csClicked, csPalette,
    csReadingState, csAlignmentNeeded, csFocusing, csCreating,
    csPaintCopy, csCustomPaint, csDestroyingHandle, csDocking,
    csDesignerHide, csPanning, csRecreating, csAligning, csGlassPaint,
    csPrintClient);


  { New TControlStyles: csNeedsBorderPaint and csParentBackground.

    These two ControlStyles are only applicable when Themes are Enabled
    in applications on Windows XP. csNeedsBorderPaint causes the
    ThemeServices to paint the border of a control with the current theme.
    csParentBackground causes the parent to draw its background into the
    Control's background; this is useful for controls which need to show their
    parent's theme elements, such as a TPanel or TFrame that appear on a
    TPageControl. TWinControl introduces a protected ParentBackground
    property which includes/excludes the csParentBackground control style.
  }
  TControlStyle = set of (csAcceptsControls, csCaptureMouse,
    csDesignInteractive, csClickEvents, csFramed, csSetCaption, csOpaque,
    csDoubleClicks, csFixedWidth, csFixedHeight, csNoDesignVisible,
    csReplicatable, csNoStdEvents, csDisplayDragImage, csReflector,
    csActionClient, csMenuEvents, csNeedsBorderPaint, csParentBackground,
    csPannable, csAlignWithMargins, csGestures, csPaintBlackOpaqueOnGlass,
    csOverrideStylePaint);

  TMouseButton = System.UITypes.TMouseButton;
  {$NODEFINE TMouseButton}

  TMouseActivate = System.UITypes.TMouseActivate;
  {$NODEFINE TMouseActivate}

  TDragMode = System.UITypes.TDragMode;
  {$NODEFINE TDragMode}

  TDragState = System.UITypes.TDragState;
  {$NODEFINE TDragState}

  TDragKind = System.UITypes.TDragKind;
  {$NODEFINE TDragKind}

  TTabOrder = System.UITypes.TTabOrder;
  {$NODEFINE TTabOrder}
  {$HPPEMIT OPENNAMESPACE}
  {$HPPEMIT 'using ::System::Uitypes::TMouseButton;'}
  {$HPPEMIT 'using ::System::Uitypes::TMouseActivate;'}
  {$HPPEMIT 'using ::System::Uitypes::TDragMode;'}
  {$HPPEMIT 'using ::System::Uitypes::TDragState;'}
  {$HPPEMIT 'using ::System::Uitypes::TDragKind;'}
  {$HPPEMIT 'using ::System::Uitypes::TTabOrder;'}
  {$HPPEMIT CLOSENAMESPACE}

  TCaption = type string;

{$IF NOT DEFINED(CLR)}
  PMouseActivateRec = ^TMouseActivateRec;
{$IFEND}
  [StructLayout(LayoutKind.Sequential)]
  TMouseActivateRec = record
    MousePos: TPoint;
    HitTest: Integer;
    Button: TMouseButton;
    ShiftState: TShiftState;
    TopLevel: HWND;
  end;

{$IF DEFINED(CLR)}
  TCMMouseActivate = class(TCMObjectMsg)
  protected
    function GetMouseActivateRec: TMouseActivateRec;
    procedure SetMouseActivateRec(Value: TMouseActivateRec);
  public
    property Reserved: WPARAM read GetWParam write SetWParam;
    property MouseActivateRec: TMouseActivateRec read GetMouseActivateRec write SetMouseActivateRec;
  end;
{$ELSE}
  TCMMouseActivate = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    Reserved: WPARAM;
    MouseActivateRec: PMouseActivateRec;
    Result: LRESULT;
  end;
{$IFEND}

  TScalingFlags = set of (sfLeft, sfTop, sfWidth, sfHeight, sfFont,
    sfDesignSize);

  TAnchorKind = System.UITypes.TAnchorKind;
  {$NODEFINE TAnchorKind}
  TAnchors = System.UITypes.TAnchors;
  {$NODEFINE TAnchors}
  {$HPPEMIT OPENNAMESPACE}
  {$HPPEMIT 'using ::System::Uitypes::TAnchorKind;'}
  {$HPPEMIT 'using ::System::Uitypes::TAnchors;'}
  {$HPPEMIT CLOSENAMESPACE}

  TConstraintSize = 0..MaxInt;

  TSizeConstraints = class(TPersistent)
  private
    FControl: TControl;
    FMaxHeight: TConstraintSize;
    FMaxWidth: TConstraintSize;
    FMinHeight: TConstraintSize;
    FMinWidth: TConstraintSize;
    FOnChange: TNotifyEvent;
    procedure SetConstraints(Index: Integer; Value: TConstraintSize);
  protected
    procedure Change; virtual;
    procedure AssignTo(Dest: TPersistent); override;
    property Control: TControl read FControl;
  public
    constructor Create(Control: TControl); virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property MaxHeight: TConstraintSize index 0 read FMaxHeight write SetConstraints default 0;
    property MaxWidth: TConstraintSize index 1 read FMaxWidth write SetConstraints default 0;
    property MinHeight: TConstraintSize index 2 read FMinHeight write SetConstraints default 0;
    property MinWidth: TConstraintSize index 3 read FMinWidth write SetConstraints default 0;
  end;

  TMarginSize = 0..MaxInt;

  TMargins = class(TPersistent)
  private
    FControl: TControl;
    FLeft, FTop, FRight, FBottom: TMarginSize;
    FOnChange: TNotifyEvent;
    procedure SetMargin(Index: Integer; Value: TMarginSize);
  protected
    procedure Change; virtual;
    procedure AssignTo(Dest: TPersistent); override;
    function GetControlBound(Index: Integer): Integer; virtual;
    class procedure InitDefaults(Margins: TMargins); virtual;
    property Control: TControl read FControl;
  public
    constructor Create(Control: TControl); virtual;
    procedure SetControlBounds(ALeft, ATop, AWidth, AHeight: Integer; Aligning: Boolean = False); overload;
    procedure SetControlBounds(const ARect: TRect; Aligning: Boolean = False); overload;
    procedure SetBounds(ALeft, ATop, ARight, ABottom: Integer);
    property ControlLeft: Integer index 0 read GetControlBound;
    property ControlTop: Integer index 1 read GetControlBound;
    property ControlWidth: Integer index 2 read GetControlBound;
    property ControlHeight: Integer index 3 read GetControlBound;
    property ExplicitLeft: Integer index 4 read GetControlBound;
    property ExplicitTop: Integer index 5 read GetControlBound;
    property ExplicitWidth: Integer index 6 read GetControlBound;
    property ExplicitHeight: Integer index 7 read GetControlBound;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Left: TMarginSize index 0 read FLeft write SetMargin default 3;
    property Top: TMarginSize index 1 read FTop write SetMargin default 3;
    property Right: TMarginSize index 2 read FRight write SetMargin default 3;
    property Bottom: TMarginSize index 3 read FBottom write SetMargin default 3;
  end;

  TPadding = class(TMargins)
  protected
    class procedure InitDefaults(Margins: TMargins); override;
  published
    property Left default 0;
    property Top default 0;
    property Right default 0;
    property Bottom default 0;
  end;

  TGestureID = rgiFirst..igiLast;

{$IF DEFINED(CLR)}
  TGestureEventInfo = record
    GestureID: TGestureID;
    Location: TPoint;
    Flags: TInteractiveGestureFlags;
    Angle: Double;
    InertiaVector: TSmallPoint;
    Distance: Integer;
    TapLocation: TSmallPoint;
  end;
{$ELSE}
  PGestureEventInfo = ^TGestureEventInfo;
  TGestureEventInfo = record
    GestureID: TGestureID;
    Location: TPoint;
    Flags: TInteractiveGestureFlags;
    Angle: Double;
    InertiaVector: TSmallPoint;
    case Integer of
      0: (Distance: Integer);
      1: (TapLocation: TSmallPoint);
  end;
{$IFEND}

{$IF DEFINED(CLR)}
  TCMGesture = TMessage;
{$ELSE}
  TCMGesture = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    Reserved: WPARAM;
    Info: PGestureEventInfo;
    Result: LRESULT;
  end;
{$IFEND}

  TGestureEvent = procedure(Sender: TObject; const EventInfo: TGestureEventInfo;
     var Handled: Boolean) of object;

  TStandardGesture = (
    sgLeft            = sgiLeft,
    sgRight           = sgiRight,
    sgUp              = sgiUp,
    sgDown            = sgiDown,
    sgUpLeft          = sgiUpLeft,
    sgUpRight         = sgiUpRight,
    sgDownLeft        = sgiDownLeft,
    sgDownRight       = sgiDownRight,
    sgLeftUp          = sgiLeftUp,
    sgLeftDown        = sgiLeftDown,
    sgRightUp         = sgiRightUp,
    sgRightDown       = sgiRightDown,
    sgUpDown          = sgiUpDown,
    sgDownUp          = sgiDownUp,
    sgLeftRight       = sgiLeftRight,
    sgRightLeft       = sgiRightLeft,
    sgUpLeftLong      = sgiUpLeftLong,
    sgUpRightLong     = sgiUpRightLong,
    sgDownLeftLong    = sgiDownLeftLong,
    sgDownRightLong   = sgiDownRightLong,
    sgScratchout      = sgiScratchout,
    sgTriangle        = sgiTriangle,
    sgSquare          = sgiSquare,
    sgCheck           = sgiCheck,
    sgCurlicue        = sgiCurlicue,
    sgDoubleCurlicue  = sgiDoubleCurlicue,
    sgCircle          = sgiCircle,
    sgDoubleCircle    = sgiDoubleCircle,
    sgSemiCircleLeft  = sgiSemiCircleLeft,
    sgSemiCircleRight = sgiSemiCircleRight,
    sgChevronUp       = sgiChevronUp,
    sgChevronDown     = sgiChevronDown,
    sgChevronLeft     = sgiChevronLeft,
    sgChevronRight    = sgiChevronRight);

  TStandardGestures = set of TStandardGesture;

  TCustomGestureManager = class;
  TCustomGestureCollection = class;
  TCustomGestureCollectionItem = class;

  TTabletOption = (toPressAndHold, toPenTapFeedback, toPenBarrelFeedback,
    toTouchUIForceOn, toTouchUIForceOff, toTouchSwitch, toFlicks,
    toSmoothScrolling, toFlickFallbackKeys);

  TTabletOptions = set of TTabletOption;

  TGestureType = (gtStandard, gtRecorded, gtRegistered, gtNone);
  TGestureTypes = set of TGestureType;

  TGestureOption = (goUniDirectional, goSkew, goEndpoint, goRotate);
  TGestureOptions = set of TGestureOption;

  TGestureArray = array of TCustomGestureCollectionItem;
  TGesturePointArray = array of TPoint;

  EGestureException = class(Exception);

  TCustomGestureCollectionItem = class(TCollectionItem)
  strict protected
    function GetAction: TBasicAction; virtual; abstract;
    function GetDeviation: Integer; virtual; abstract;
    function GetErrorMargin: Integer; virtual; abstract;
    function GetGestureID: TGestureID; virtual; abstract;
    function GetGestureType: TGestureType; virtual; abstract;
    function GetOptions: TGestureOptions; virtual; abstract;
    function GetName: string; virtual; abstract;
    function GetPoints: TGesturePointArray; virtual; abstract;
    procedure SetAction(const Value: TBasicAction); virtual; abstract;
    procedure SetDeviation(const Value: Integer); virtual; abstract;
    procedure SetErrorMargin(const Value: Integer); virtual; abstract;
    procedure SetGestureID(const Value: TGestureID); virtual; abstract;
    procedure SetName(const Value: string); virtual; abstract;
    procedure SetOptions(const Value: TGestureOptions); virtual; abstract;
    procedure SetPoints(const Value: TGesturePointArray); virtual; abstract;
  public
    property Deviation: Integer read GetDeviation write SetDeviation default 20;
    property ErrorMargin: Integer read GetErrorMargin write SetErrorMargin default 20;
    property GestureID: TGestureID read GetGestureID write SetGestureID;
    property GestureType: TGestureType read GetGestureType;
    property Options: TGestureOptions read GetOptions write SetOptions default [goUniDirectional, goRotate];
    property Name: string read GetName write SetName;
    property Points: TGesturePointArray read GetPoints write SetPoints;
    property Action: TBasicAction read GetAction write SetAction;
  end;

  TCustomGestureCollection = class(TCollection)
  protected
    function GetGestureManager: TCustomGestureManager; virtual; abstract;
    function GetItem(Index: Integer): TCustomGestureCollectionItem;
    procedure SetItem(Index: Integer; const Value: TCustomGestureCollectionItem);
  public
    function AddGesture: TCustomGestureCollectionItem; virtual; abstract;
    function FindGesture(AGestureID: TGestureID): TCustomGestureCollectionItem; overload; virtual; abstract;
    function FindGesture(const AName: string): TCustomGestureCollectionItem; overload; virtual; abstract;
    function GetUniqueGestureID: TGestureID; virtual; abstract;
    procedure RemoveGesture(AGestureID: TGestureID); virtual; abstract;
    property GestureManager: TCustomGestureManager read GetGestureManager;
    property Items[Index: Integer]: TCustomGestureCollectionItem read GetItem write SetItem; default;
  end;

  TCustomGestureManager = class(TComponent)
  protected
    function GetGestureList(AControl: TControl): TGestureArray; virtual; abstract;
    function GetStandardGestures(AControl: TControl): TStandardGestures; virtual; abstract;
    procedure SetStandardGestures(AControl: TControl; AStandardGestures: TStandardGestures); virtual; abstract;
  public
    function AddRecordedGesture(Item: TCustomGestureCollectionItem): TGestureID; overload; virtual; abstract;
    function FindCustomGesture(AGestureID: TGestureID): TCustomGestureCollectionItem; overload; virtual; abstract;
    function FindCustomGesture(const AName: string): TCustomGestureCollectionItem; overload; virtual; abstract;
    function FindGesture(AControl: TControl; AGestureID: TGestureID): TCustomGestureCollectionItem; overload; virtual; abstract;
    function FindGesture(AControl: TControl; const AName: string): TCustomGestureCollectionItem; overload; virtual; abstract;
    procedure RegisterControl(AControl: TControl); virtual; abstract;
    procedure RemoveRecordedGesture(AGestureID: TGestureID); overload; virtual; abstract;
    procedure RemoveRecordedGesture(AGesture: TCustomGestureCollectionItem); overload; virtual; abstract;
    function SelectGesture(AControl: TControl; AGestureID: TGestureID): Boolean; overload; virtual; abstract;
    function SelectGesture(AControl: TControl; const AName: string): Boolean; overload; virtual; abstract;
    procedure UnregisterControl(AControl: TControl); virtual; abstract;
    procedure UnselectGesture(AControl: TControl; AGestureID: TGestureID); virtual; abstract;
    property GestureList[AControl: TControl]: TGestureArray read GetGestureList;
    property StandardGestures[AControl: TControl]: TStandardGestures
      read GetStandardGestures write SetStandardGestures;
  end;

  TCustomGestureEngine = class
  public type
    TGestureEngineFlag = (efMouseEvents, efTouchEvents);
    TGestureEngineFlags = set of TGestureEngineFlag;
  protected
    function GetActive: Boolean; virtual; abstract;
    function GetFlags: TGestureEngineFlags; virtual; abstract;
    procedure SetActive(const Value: Boolean); virtual; abstract;
  public
    constructor Create(AControl: TWinControl); virtual; abstract;
    procedure Notification(const Message: TMessage); virtual; abstract;
    class function Supported: Boolean; virtual;
    property Active: Boolean read GetActive write SetActive;
    property Flags: TGestureEngineFlags read GetFlags;
  end;

  TTouchProperty = (tpInteractiveGestures, tpInteractiveGestureOptions,
    tpParentTabletOptions, tpTabletOptions);

  TCustomTouchManager = class(TPersistent)
  private
    FControl: TControl;
    FGestureEngine: TCustomGestureEngine;
    FGestureManager: TCustomGestureManager;
    FInteractiveGestures: TInteractiveGestures;
    FInteractiveGestureOptions: TInteractiveGestureOptions;
    FNotifyList: TList;
    FParentTabletOptions: Boolean;
    FStandardGestures: TStandardGestures;
    FTabletOptions: TTabletOptions;
    function GetGestureList: TGestureArray;
    function GetStandardGestures: TStandardGestures;
    function IsInteractiveGestureOptionsStored: Boolean;
    function IsInteractiveGesturesStored: Boolean;
    function IsParentTabletOptionsStored: Boolean;
    function IsTabletOptionsStored: Boolean;
    procedure SetGestureEngine(const Value: TCustomGestureEngine);
    procedure SetGestureManager(const Value: TCustomGestureManager);
    procedure SetStandardGestures(const Value: TStandardGestures);
    procedure SetParentTabletOptions(const Value: Boolean);
    procedure SetTabletOptions(const Value: TTabletOptions);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function IsDefault: Boolean;
  public
    constructor Create(AControl: TControl);
    destructor Destroy; override;
    procedure ChangeNotification(AControl: TControl);
    function FindGesture(AGestureID: TGestureID): TCustomGestureCollectionItem; overload;
    function FindGesture(const AName: string): TCustomGestureCollectionItem; overload;
    procedure RemoveChangeNotification(AControl: TControl);
    function SelectGesture(AGestureID: TGestureID): Boolean; overload;
    function SelectGesture(const AName: string): Boolean; overload;
    procedure UnselectGesture(AGestureID: TGestureID); inline;
    property GestureEngine: TCustomGestureEngine read FGestureEngine write SetGestureEngine;
    property GestureList: TGestureArray read GetGestureList;
    property GestureManager: TCustomGestureManager read FGestureManager write SetGestureManager;
    property InteractiveGestures: TInteractiveGestures
      read FInteractiveGestures write FInteractiveGestures stored IsInteractiveGesturesStored;
    property InteractiveGestureOptions: TInteractiveGestureOptions
      read FInteractiveGestureOptions
      write FInteractiveGestureOptions stored IsInteractiveGestureOptionsStored;
    property ParentTabletOptions: Boolean read FParentTabletOptions
      write SetParentTabletOptions stored IsParentTabletOptionsStored;
    property StandardGestures: TStandardGestures read GetStandardGestures write SetStandardGestures;
    property TabletOptions: TTabletOptions read FTabletOptions
      write SetTabletOptions stored IsTabletOptionsStored;
  end;

  TTouchManager = class(TCustomTouchManager)
  published
    property GestureManager;
    property InteractiveGestures;
    property InteractiveGestureOptions;
    property ParentTabletOptions;
    property TabletOptions;
  end;

  TMouseEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer) of object;
  TMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState;
    X, Y: Integer) of object;
  TMouseActivateEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer; HitTest: Integer; var MouseActivate: TMouseActivate) of object;
  TKeyEvent = procedure(Sender: TObject; var Key: Word;
    Shift: TShiftState) of object;
  TKeyPressEvent = procedure(Sender: TObject; var Key: Char) of object;
  TDragOverEvent = procedure(Sender, Source: TObject; X, Y: Integer;
    State: TDragState; var Accept: Boolean) of object;
  TDragDropEvent = procedure(Sender, Source: TObject;
    X, Y: Integer) of object;
  TStartDragEvent = procedure(Sender: TObject;
    var DragObject: TDragObject) of object;
  TEndDragEvent = procedure(Sender, Target: TObject;
    X, Y: Integer) of object;
  TDockDropEvent = procedure(Sender: TObject; Source: TDragDockObject;
    X, Y: Integer) of object;
  TDockOverEvent = procedure(Sender: TObject; Source: TDragDockObject;
    X, Y: Integer; State: TDragState; var Accept: Boolean) of object;
  TUnDockEvent = procedure(Sender: TObject; Client: TControl;
    NewTarget: TWinControl; var Allow: Boolean) of object;
  TStartDockEvent = procedure(Sender: TObject;
    var DragObject: TDragDockObject) of object;
  TGetSiteInfoEvent = procedure(Sender: TObject; DockClient: TControl;
    var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean) of object;
  TCanResizeEvent = procedure(Sender: TObject; var NewWidth, NewHeight: Integer;
    var Resize: Boolean) of object;
  TConstrainedResizeEvent = procedure(Sender: TObject; var MinWidth, MinHeight,
    MaxWidth, MaxHeight: Integer) of object;
  TMouseWheelEvent = procedure(Sender: TObject; Shift: TShiftState;
    WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean) of object;
  TMouseWheelUpDownEvent = procedure(Sender: TObject; Shift: TShiftState;
    MousePos: TPoint; var Handled: Boolean) of object;
  TContextPopupEvent = procedure(Sender: TObject; MousePos: TPoint; var Handled: Boolean) of object;

{$IFDEF LINUX}
  TWndMethod = WinUtils.TWndMethod;
{$ENDIF}
{$IFDEF MSWINDOWS}
  TWndMethod = System.Classes.TWndMethod;
{$ENDIF}

{$IF NOT DEFINED(CLR)}
  {$EXTERNALSYM TWndMethod}
{$IFEND}

{$IF DEFINED(CLR)}
  TReservedControlData = TObject;
{$ELSE}
  TReservedControlData = Pointer;
{$IFEND}

  // TDockOrientation indicates how a zone's child zones are arranged.
  // doNoOrient means a zone contains a TControl and not child zones.
  // doHorizontal means a zone's children are stacked top-to-bottom.
  // doVertical means a zone's children are arranged left-to-right.
  TDockOrientation = (doNoOrient, doHorizontal, doVertical);

  {IControl is an interface for accessing certain protected methods
   of a TControl instance. It is intended primarily for component writers
   who need to operate on a generic TControl instance}
{$IF DEFINED(CLR)}
  IControl = interface
    procedure SetDragMode(Value: TDragMode);
    function GetDragMode: TDragMode;
    property DragMode: TDragMode read GetDragMode write SetDragMode;
    procedure HookDelegate(EventName: string; Handler: MulticastDelegate);
    function GetPopupMenu: TPopupMenu;
    procedure SetPopupMenu(Value: TPopupMenu);
    property PopupMenu: TPopupMenu read GetPopupMenu write SetPopupMenu;
    procedure UnhookDelegate(EventName: string; Handler: MulticastDelegate);
    procedure WndProc(var Msg: TMessage);
    procedure RestoreWndProc;
    function GetFont: TFont;
    [UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
    procedure SetMouseCapture(Value: Boolean);
    [UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
    function GetMouseCapture: Boolean;
    property MouseCapture: Boolean read GetMouseCapture write SetMouseCapture;
  end;
{$IFEND}

  [ToolboxItem(False)]
  [RootDesignerSerializerAttribute('', '', False)]
{$IF DEFINED(CLR)}
  TControl = class(TComponent, IControl)
{$ELSE}
  TControl = class(TComponent)
{$IFEND}
  private
    FParent: TWinControl;
    FWindowProc: TWndMethod;
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FControlStyle: TControlStyle;
    FControlState: TControlState;
    FDesktopFont: Boolean;
    FVisible: Boolean;
    FEnabled: Boolean;
    FParentFont: Boolean;
    FParentColor: Boolean;
    FAlign: TAlign;
    FAutoSize: Boolean;
    FDragMode: TDragMode;
    FIsControl: Boolean;
    FBiDiMode: TBiDiMode;
    FParentBiDiMode: Boolean;
    FAnchors: TAnchors;
    FFont: TFont;
    FActionLink: TControlActionLink;
    FColor: TColor;
    FConstraints: TSizeConstraints;
    FMargins: TMargins;
    FCursor: TCursor;
    FDragCursor: TCursor;
    FPopupMenu: TPopupMenu;
    FHint: string;
    FFontHeight: Integer;
    FScalingFlags: TScalingFlags;
    FShowHint: Boolean;
    FParentShowHint: Boolean;
    FDragKind: TDragKind;
    FDockOrientation: TDockOrientation;
    FHostDockSite: TWinControl;
    FWheelAccumulator: Integer;
    FUndockWidth: Integer;
    FUndockHeight: Integer;
    FLRDockWidth: Integer;
    FTBDockHeight: Integer;
    FFloatingDockSiteClass: TWinControlClass;
    FTouchManager: TTouchManager;
    FOnCanResize: TCanResizeEvent;
    FOnConstrainedResize: TConstrainedResizeEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnDragDrop: TDragDropEvent;
    FOnDragOver: TDragOverEvent;
    FOnResize: TNotifyEvent;
    FOnStartDock: TStartDockEvent;
    FOnEndDock: TEndDragEvent;
    FOnStartDrag: TStartDragEvent;
    FOnEndDrag: TEndDragEvent;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnContextPopup: TContextPopupEvent;
    FOnMouseActivate: TMouseActivateEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnMouseWheelDown: TMouseWheelUpDownEvent;
    FOnMouseWheelUp: TMouseWheelUpDownEvent;
    FOnGesture: TGestureEvent;
    FHelpType: THelpType;
    FHelpKeyword: string;
    FHelpContext: THelpContext;
    FCustomHint: TCustomHint;
    FParentCustomHint: Boolean;
{$IF DEFINED(CLR)}
    FText: string;
{$ELSE}
    FText: PChar;
{$IFEND}
    function GetCustomHint: TCustomHint;
    procedure CalcDockSizes;
    function CheckNewSize(var NewWidth, NewHeight: Integer): Boolean;
    function CreateFloatingDockSite(Bounds: TRect): TWinControl;
    procedure DoActionChange(Sender: TObject);
    function DoCanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
    function DoCanResize(var NewWidth, NewHeight: Integer): Boolean;
    procedure DoConstraintsChange(Sender: TObject);
    procedure DoConstrainedResize(var NewWidth, NewHeight: Integer);
    procedure DoDragMsg(var DragMsg: TCMDrag);
    procedure DoMouseActivate(var Message: TCMMouseActivate);
    procedure DoMouseDown(var Message: TWMMouse; Button: TMouseButton;
      Shift: TShiftState);
    procedure DoMouseUp(var Message: TWMMouse; Button: TMouseButton);
    procedure DoMarginChange(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    function GetAlignWithMargins: Boolean; inline;
    function GetBoundsRect: TRect;
    function GetClientHeight: Integer;
    function GetClientWidth: Integer;
    function GetLRDockWidth: Integer;
    function GetMouseCapture: Boolean;
    function GetTBDockHeight: Integer;
    function GetUndockWidth: Integer;
    function GetUndockHeight: Integer;
    procedure InvalidateControl(IsVisible, IsOpaque: Boolean);
    function IsAnchorsStored: Boolean;
    function IsBiDiModeStored: Boolean;
    function IsCaptionStored: Boolean;
    function IsColorStored: Boolean;
    function IsEnabledStored: Boolean;
    function IsFontStored: Boolean;
    function IsHintStored: Boolean;
    function IsHelpContextStored: Boolean;
    function IsOnClickStored: Boolean;
    function IsShowHintStored: Boolean;
    function IsVisibleStored: Boolean;
    procedure ReadIsControl(Reader: TReader);
    procedure ReadExplicitLeft(Reader: TReader);
    procedure ReadExplicitTop(Reader: TReader);
    procedure ReadExplicitWidth(Reader: TReader);
    procedure ReadExplicitHeight(Reader: TReader);
    procedure SetAlignWithMargins(Value: Boolean);
    procedure SetAnchors(Value: TAnchors);
    procedure SetAction(Value: TBasicAction);
    procedure SetAlign(Value: TAlign);
    procedure SetBoundsRect(const Rect: TRect);
    procedure SetClientHeight(Value: Integer);
    procedure SetClientSize(Value: TPoint);
    procedure SetClientWidth(Value: Integer);
    procedure SetColor(Value: TColor);
    procedure SetCursor(Value: TCursor);
    procedure SetDesktopFont(Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetHeight(Value: Integer);
    procedure SetHelpContext(const Value: THelpContext);
    procedure SetHelpKeyword(const Value: string);
    procedure SetHostDockSite(Value: TWinControl);
    procedure SetLeft(Value: Integer);
    procedure SetMouseCapture(Value: Boolean);
    procedure SetParentColor(Value: Boolean);
    procedure SetParentFont(Value: Boolean);
    procedure SetShowHint(Value: Boolean);
    procedure SetParentShowHint(Value: Boolean);
    procedure SetParentCustomHint(Value: Boolean);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure SetTop(Value: Integer);
    procedure SetVisible(Value: Boolean);
    procedure SetWidth(Value: Integer);
    procedure SetZOrderPosition(Position: Integer);
    procedure UpdateAnchorRules;
    procedure WriteIsControl(Writer: TWriter);
    procedure WriteExplicitLeft(Writer: TWriter);
    procedure WriteExplicitTop(Writer: TWriter);
    procedure WriteExplicitWidth(Writer: TWriter);
    procedure WriteExplicitHeight(Writer: TWriter);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMMButtonDown(var Message: TWMMButtonDown); message WM_MBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMRButtonDblClk(var Message: TWMRButtonDblClk); message WM_RBUTTONDBLCLK;
    procedure WMMButtonDblClk(var Message: TWMMButtonDblClk); message WM_MBUTTONDBLCLK;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;
    procedure WMMButtonUp(var Message: TWMMButtonUp); message WM_MBUTTONUP;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMMouseActivate(var Message: TCMMouseActivate); message CM_MOUSEACTIVATE;
    procedure CMParentFontChanged(var Message: TCMParentFontChanged); message CM_PARENTFONTCHANGED;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMParentShowHintChanged(var Message: TMessage); message CM_PARENTSHOWHINTCHANGED;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMHitTest(var Message: TCMHitTest); message CM_HITTEST;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMParentBiDiModeChanged(var Message: TMessage); message CM_PARENTBIDIMODECHANGED;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure CMGesture(var Message: TCMGesture); message CM_GESTURE;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure CMParentTabletOptionsChanged(var Message: TMessage); message CM_PARENTTABLETOPTIONSCHANGED;
    procedure SetConstraints(const Value: TSizeConstraints);
    procedure SetMargins(const Value: TMargins);
    procedure SetTouchManager(const Value: TTouchManager);
{$IF DEFINED(CLR)}
    function GetFont: TFont;
    procedure HookDelegate(EventName: string; Handler: MulticastDelegate);
    procedure UnhookDelegate(EventName: string; Handler: MulticastDelegate);
    procedure RestoreWndProc;
{$ELSE}
    function GetText: TCaption;
    procedure SetText(const Value: TCaption);
    procedure CMFloat(var Message: TCMFloat); message CM_FLOAT;
{$IFEND}
{$IF DEFINED(CLR)}
  public
    function get_Parent: TWinControl;

    function GetText: TCaption;
    function GetTextPiece(Size: Integer): TCaption; virtual;
    function GetTextLen: Integer; virtual;
    procedure SetText(const Value: TCaption); virtual;
{$IFEND}
  protected
    FAnchorMove: Boolean;
    FAnchorRules: TPoint;
    FAnchorOrigin: TPoint;
    FOriginalParentSize: TPoint;
    FExplicitLeft: Integer;
    FExplicitTop: Integer;
    FExplicitWidth: Integer;
    FExplicitHeight: Integer;
    FReserved: TReservedControlData; // Do not use - for internal use only!
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); dynamic;
    procedure AdjustSize; dynamic;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BeginAutoDrag; dynamic;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; virtual;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; virtual;
    procedure Changed;
    procedure ChangeScale(M, D: Integer); dynamic;
    procedure Click; dynamic;
    procedure ConstrainedResize(var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer); virtual;
    function CalcCursorPos: TPoint;
    procedure CreateTouchManager; virtual;
    procedure DblClick; dynamic;
    procedure DefaultDockImage(DragDockObject: TDragDockObject; Erase: Boolean); dynamic;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DockTrackNoTarget(Source: TDragDockObject; X, Y: Integer); dynamic;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); dynamic;
    procedure DoEndDock(Target: TObject; X, Y: Integer); dynamic;
    procedure DoDock(NewDockSite: TWinControl; var ARect: TRect); dynamic;
    procedure DoStartDock(var DragObject: TDragObject); dynamic;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; dynamic;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; dynamic;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; dynamic;
    procedure DragCanceled; dynamic;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); dynamic;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); dynamic;
    procedure DoStartDrag(var DragObject: TDragObject); dynamic;
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); virtual;
    procedure DoGetGestureOptions(var Gestures: TInteractiveGestures;
      var Options: TInteractiveGestureOptions); virtual;
    procedure DrawDragDockImage(DragDockObject: TDragDockObject); dynamic;
    procedure EraseDragDockImage(DragDockObject: TDragDockObject); dynamic;
    function GetAction: TBasicAction; virtual;
    function GetActionLinkClass: TControlActionLinkClass; dynamic;
    function GetClientOrigin: TPoint; virtual;
    function GetClientRect: TRect; virtual;
    function GetDeviceContext(var WindowHandle: HWND): HDC; overload; virtual;
    function GetDockEdge(MousePos: TPoint): TAlign; dynamic;
    function GetEnabled: Boolean; virtual;
    function GetFloating: Boolean; virtual;
    function GetFloatingDockSiteClass: TWinControlClass; virtual;
    function GetPalette: HPALETTE; dynamic;
    function GetPopupMenu: TPopupMenu; dynamic;
    function GetDragMode: TDragMode;
    function IsTouchPropertyStored(AProperty: TTouchProperty): Boolean; dynamic;
    procedure Loaded; override;
    function MouseActivate(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; HitTest: Integer): TMouseActivate; dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); dynamic;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); dynamic;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure PositionDockRect(DragDockObject: TDragDockObject); dynamic;
    function PaletteChanged(Foreground: Boolean): Boolean; dynamic;
    procedure ReadState(Reader: TReader); override;
    procedure RequestAlign; virtual;
    procedure Resize; dynamic;
    procedure ScaleConstraints(M, D: Integer);
    procedure ScaleMargins(M, D: Integer);
    procedure SendCancelMode(Sender: TControl);
    procedure SendDockNotification(Msg: Cardinal; WParam, LParam: THandle);
    procedure SetAutoSize(Value: Boolean); virtual;
    procedure SetDragMode(Value: TDragMode); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetName(const Value: TComponentName); override;
    procedure SetParent(AParent: TWinControl); virtual;
    procedure SetParentBiDiMode(Value: Boolean); virtual;
    procedure SetBiDiMode(Value: TBiDiMode); virtual;
    procedure SetZOrder(TopMost: Boolean); dynamic;
    procedure SetCustomHint(Value: TCustomHint);
    procedure UpdateExplicitBounds;
    procedure UpdateBoundsRect(const R: TRect);
    procedure VisibleChanging; dynamic;
    procedure WndProc(var Message: TMessage); virtual;
{$IF DEFINED(CLR)}
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure FloatControl(DockSource: TDragDockObject); virtual; // replaces use of CM_FLOAT message
    function GetDeviceContext(var WindowHandle: THWndWrapper): HDC; overload; virtual;
{$ELSE}
    function DesignWndProc(var Message: TMessage): Boolean; dynamic;
    function GetDragImages: TDragImageList; virtual;
{$IFEND}
    property ActionLink: TControlActionLink read FActionLink write FActionLink;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property Caption: TCaption read GetText write SetText stored IsCaptionStored;
    property DesktopFont: Boolean read FDesktopFont write SetDesktopFont default False;
    property DragKind: TDragKind read FDragKind write FDragKind default dkDrag;
    property DragCursor: TCursor read FDragCursor write FDragCursor default crDrag;
    property DragMode: TDragMode read GetDragMode write SetDragMode default dmManual;
    property IsControl: Boolean read FIsControl write FIsControl;
    property MouseCapture: Boolean read GetMouseCapture write SetMouseCapture;
    property ParentBiDiMode: Boolean read FParentBiDiMode write SetParentBiDiMode default True;
    property ParentColor: Boolean read FParentColor write SetParentColor default True;
    property ParentFont: Boolean read FParentFont write SetParentFont default True;
    property ParentShowHint: Boolean read FParentShowHint write SetParentShowHint default True;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property ScalingFlags: TScalingFlags read FScalingFlags write FScalingFlags;
    property Text: TCaption read GetText write SetText;
    property WheelAccumulator: Integer read FWheelAccumulator write FWheelAccumulator;
{$IF DEFINED(CLR)}
    property WindowText: string read FText write FText;
{$ELSE}
    property Color: TColor read FColor write SetColor stored IsColorStored default clWindow;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property WindowText: PChar read FText write FText;
{$IFEND}
    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
    property OnClick: TNotifyEvent read FOnClick write FOnClick stored IsOnClickStored;
    property OnConstrainedResize: TConstrainedResizeEvent read FOnConstrainedResize write FOnConstrainedResize;
    property OnContextPopup: TContextPopupEvent read FOnContextPopup write FOnContextPopup;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDragDrop: TDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnDragOver: TDragOverEvent read FOnDragOver write FOnDragOver;
    property OnEndDock: TEndDragEvent read FOnEndDock write FOnEndDock;
    property OnEndDrag: TEndDragEvent read FOnEndDrag write FOnEndDrag;
    property OnMouseActivate: TMouseActivateEvent read FOnMouseActivate write FOnMouseActivate;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnMouseWheelDown: TMouseWheelUpDownEvent read FOnMouseWheelDown
      write FOnMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read FOnMouseWheelUp write
      FOnMouseWheelUp;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnStartDock: TStartDockEvent read FOnStartDock write FOnStartDock;
    property OnStartDrag: TStartDragEvent read FOnStartDrag write FOnStartDrag;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginDrag(Immediate: Boolean; Threshold: Integer = -1);
    procedure BringToFront;
    function ClientToScreen(const Point: TPoint): TPoint;
    function ClientToParent(const Point: TPoint; AParent: TWinControl = nil): TPoint;
    procedure Dock(NewDockSite: TWinControl; ARect: TRect); dynamic;
    function Dragging: Boolean;
    procedure DragDrop(Source: TObject; X, Y: Integer); dynamic;
    function DrawTextBiDiModeFlags(Flags: Longint): Longint;
    function DrawTextBiDiModeFlagsReadingOnly: Longint;
    property Enabled: Boolean read GetEnabled write SetEnabled stored IsEnabledStored default True;
    procedure EndDrag(Drop: Boolean);
    function GetControlsAlignment: TAlignment; dynamic;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    procedure Hide;
    procedure InitiateAction; virtual;
    procedure Invalidate; virtual;
    procedure MouseWheelHandler(var Message: TMessage); dynamic;
    function IsRightToLeft: Boolean;
    function ManualDock(NewDockSite: TWinControl; DropControl: TControl = nil;
      ControlSide: TAlign = alNone): Boolean;
    function ManualFloat(ScreenPos: TRect): Boolean;
    function Perform(Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): LRESULT; overload;
    procedure Refresh;
    procedure Repaint; virtual;
    function ReplaceDockedControl(Control: TControl; NewDockSite: TWinControl;
      DropControl: TControl; ControlSide: TAlign): Boolean;
    function ScreenToClient(const Point: TPoint): TPoint;
    function ParentToClient(const Point: TPoint; AParent: TWinControl = nil): TPoint;
    procedure SendToBack;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); virtual;
    procedure SetDesignVisible(Value: Boolean); dynamic;
    procedure SetParentComponent(Value: TComponent); override;
    procedure Show;
    procedure Update; virtual;
    function UseRightToLeftAlignment: Boolean; dynamic;
    function UseRightToLeftReading: Boolean;
    function UseRightToLeftScrollBar: Boolean;
{$IF DEFINED(CLR)}
    procedure DefaultHandler(var Message); virtual;
    function DesignWndProc(var Message: TMessage): Boolean; virtual;
    function GetDragImages: TDragImageList; virtual;
    function Perform(Msg: Cardinal; AObjectMsg: TCMObjectMsg): LRESULT; overload;
    function Perform(Msg: Cardinal; WParam: WPARAM; var LParam): LRESULT; overload;
    function Perform(Msg: Cardinal; WParam: WPARAM; LParam: string): LRESULT; overload;
    function Perform(Msg: Cardinal; WParam: WPARAM; var LParam: string;
      BufLen: Integer; ResultIsLen: Boolean = False): LRESULT; overload;
    procedure SetTextBuf(Buffer: string);
{$ELSE}
    procedure DefaultHandler(var Message); override;
    function GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
    function GetTextLen: Integer;
    function Perform(Msg: Cardinal; WParam: WPARAM; LParam: PChar): LRESULT; overload;
    function Perform(Msg: Cardinal; WParam: WPARAM; var LParam: TRect): LRESULT; overload;
    procedure SetTextBuf(Buffer: PChar);
{$IFEND}
    property Action: TBasicAction read GetAction write SetAction;
    property Align: TAlign read FAlign write SetAlign default alNone;
    property Anchors: TAnchors read FAnchors write SetAnchors stored IsAnchorsStored default [akLeft, akTop];
    property BiDiMode: TBiDiMode read FBiDiMode write SetBiDiMode stored IsBiDiModeStored;
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    property ClientHeight: Integer read GetClientHeight write SetClientHeight stored False;
    property ClientOrigin: TPoint read GetClientOrigin;
    property ClientRect: TRect read GetClientRect;
    property ClientWidth: Integer read GetClientWidth write SetClientWidth stored False;
    property Constraints: TSizeConstraints read FConstraints write SetConstraints;
    property ControlState: TControlState read FControlState write FControlState;
    property ControlStyle: TControlStyle read FControlStyle write FControlStyle;
    property DockOrientation: TDockOrientation read FDockOrientation write FDockOrientation;
    property ExplicitLeft: Integer read FExplicitLeft;
    property ExplicitTop: Integer read FExplicitTop;
    property ExplicitWidth: Integer read FExplicitWidth;
    property ExplicitHeight: Integer read FExplicitHeight;
    property Floating: Boolean read GetFloating;
    property FloatingDockSiteClass: TWinControlClass read GetFloatingDockSiteClass write FFloatingDockSiteClass;
    property HostDockSite: TWinControl read FHostDockSite write SetHostDockSite;
    property LRDockWidth: Integer read GetLRDockWidth write FLRDockWidth;
    property ShowHint: Boolean read FShowHint write SetShowHint stored IsShowHintStored;
    property TBDockHeight: Integer read GetTBDockHeight write FTBDockHeight;
    property Touch: TTouchManager read FTouchManager write SetTouchManager;
    property UndockHeight: Integer read GetUndockHeight write FUndockHeight;
    property UndockWidth: Integer read GetUndockWidth write FUndockWidth;
    property Visible: Boolean read FVisible write SetVisible stored IsVisibleStored default True;
    property WindowProc: TWndMethod read FWindowProc write FWindowProc;
{$IF DEFINED(CLR)}
    property Color: TColor read FColor write SetColor stored IsColorStored default clWindow;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
    property Parent: TWinControl read get_Parent write SetParent;
{$ELSE}
    property Parent: TWinControl read FParent write SetParent;
{$IFEND}
    property OnGesture: TGestureEvent read FOnGesture write FOnGesture;
  published
    property AlignWithMargins: Boolean read GetAlignWithMargins write SetAlignWithMargins default False;
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Cursor: TCursor read FCursor write SetCursor default crDefault;
    property Hint: string read FHint write FHint stored IsHintStored;
    property HelpType: THelpType read FHelpType write FHelpType default htContext;
    property HelpKeyword: String read FHelpKeyword write SetHelpKeyword stored IsHelpContextStored;
    property HelpContext: THelpContext read FHelpContext write SetHelpContext stored IsHelpContextStored default 0;
    property Margins: TMargins read FMargins write SetMargins;
    property CustomHint: TCustomHint read GetCustomHint write SetCustomHint;
    property ParentCustomHint: Boolean read FParentCustomHint write SetParentCustomHint default True;
  end;

  TControlClass = class of TControl;

{$IF DEFINED(CLR)}
  TCreateParams = record
    Caption: string;
    Style: DWORD;
    ExStyle: DWORD;
    X, Y: Integer;
    Width, Height: Integer;
    WndParent: HWND;
    Param: IntPtr;
    WindowClass: TWndClassInfo;
    WndProc: TFNWndProc;
    WinClassName: string;
  end;
{$ELSE}
  TCreateParams = record
    Caption: PChar;
    Style: DWORD;
    ExStyle: DWORD;
    X, Y: Integer;
    Width, Height: Integer;
    WndParent: HWnd;
    Param: Pointer;
    WindowClass: TWndClass;
    WinClassName: array[0..63] of Char;
  end;
{$IFEND}

{ TWinControlActionLink }

  TWinControlActionLink = class(TControlActionLink)
  protected
    FClient: TWinControl;
    procedure AssignClient(AClient: TObject); override;
    function IsHelpContextLinked: Boolean; override;
    procedure SetHelpContext(Value: THelpContext); override;
  end;

  TWinControlActionLinkClass = class of TWinControlActionLink;

{$IF DEFINED(CLR)}
  {IWinControl is an interface for accessing certain protected methods
   of a TWinControl instance. It is intended primarily for component writers
   who need to operate on a generic TWinControl instance}
  IWinControl = interface
    procedure SelectNext(CurControl: TWinControl; GoForward, CheckTabStop: Boolean);
    procedure CreateParams(var Params: TCreateParams);
    [UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
    procedure FocusChanged(NewFocusControl: TWinControl);
  end;
{$IFEND}

{ TWinControl }

  TImeMode = (imDisable, imClose, imOpen, imDontCare,
              imSAlpha, imAlpha, imHira, imSKata, imKata,
              imChinese, imSHanguel, imHanguel);
  TImeName = type string;

  TAlignInfo = record
    AlignList: TList;
    ControlIndex: Integer;
    Align: TAlign;
    Scratch: Integer;
  end;

  TBorderWidth = 0..MaxInt;

  TBevelCut = (bvNone, bvLowered, bvRaised, bvSpace);
  TBevelEdge = (beLeft, beTop, beRight, beBottom);
  TBevelEdges = set of TBevelEdge;
  TBevelKind = (bkNone, bkTile, bkSoft, bkFlat);
  TBevelWidth = 1..MaxInt;

  // IDockManager defines an interface for managing a dock site's docked
  // controls. The default VCL implementation of IDockManager is TDockTree.
  IDockManager = interface
    ['{8619FD79-C281-11D1-AA60-00C04FA370E8}']
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure GetControlBounds(Control: TControl; out CtlBounds: TRect);
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl);
    procedure LoadFromStream(Stream: TStream);
    procedure PaintSite(DC: HDC);
    procedure PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign;
      var DockRect: TRect);
    procedure RemoveControl(Control: TControl);
    procedure ResetBounds(Force: Boolean);
    procedure SaveToStream(Stream: TStream);
    procedure SetReplacingControl(Control: TControl);
  end;

  TAlignInsertBeforeEvent = function(Sender: TWinControl; C1, C2: TControl): Boolean of object;
  TAlignPositionEvent = procedure(Sender: TWinControl; Control: TControl;
    var NewLeft, NewTop, NewWidth, NewHeight: Integer;
    var AlignRect: TRect; AlignInfo: TAlignInfo) of object;

{$IF DEFINED(CLR)}
  TWinControl = class(TControl, IWinControl)
  strict private
    class var
      FPendingParentWindow: HWND;
{$ELSE}
  TWinControl = class(TControl)
{$IFEND}
  private
    FAlignControlList: TList;
    FAlignLevel: Word;
    FBevelEdges: TBevelEdges;
    FBevelInner: TBevelCut;
    FBevelOuter: TBevelCut;
    FBevelKind: TBevelKind;
    FBevelWidth: TBevelWidth;
    FBorderWidth: TBorderWidth;
    FPadding: TPadding;
    FBrush: TBrush;
    FDockClients: TList;
    FDockManager: IDockManager;
    FImeMode: TImeMode;
    FImeName: TImeName;
    FParentWindow: HWND;
    FTabList: TList;
    FControls: TList;
    FWinControls: TList;
    FTabOrder: Integer;
    FTabStop: Boolean;
    FCtl3D: Boolean;
    FShowing: Boolean;
    FUseDockManager: Boolean;
    FDockSite: Boolean;
    FParentCtl3D: Boolean;
    FParentDoubleBuffered: Boolean;
    FPerformingShowingChanged: Boolean;
    FOnDockDrop: TDockDropEvent;
    FOnDockOver: TDockOverEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnGetSiteInfo: TGetSiteInfoEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyUp: TKeyEvent;
    FOnUnDock: TUnDockEvent;
    FOnAlignInsertBefore: TAlignInsertBeforeEvent;
    FOnAlignPosition: TAlignPositionEvent;
    FMouseInClient: Boolean;
    FMouseControl: TControl;
    FTouchControl: TControl;
{$IF DEFINED(CLR)}
    FDefWndProc: TFNWndProc;
{$ELSE}
    FDefWndProc: Pointer;
    FHandle: HWnd;
    FObjectInstance: Pointer;
{$IFEND}
    procedure AlignControl(AControl: TControl);
    procedure CalcConstraints(var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer);
    procedure DoPaddingChange(Sender: TObject);
    function GetAlignDisabled: Boolean;
    function GetControl(Index: Integer): TControl;
    function GetControlCount: Integer;
    function GetDockClientCount: Integer;
    function GetDockClients(Index: Integer): TControl;
    function GetHandle: HWND;
    function GetParentBackground: Boolean; inline;
    function GetTabOrder: TTabOrder;
    function GetVisibleDockClientCount: Integer;
    procedure Insert(AControl: TControl);
    procedure InvalidateFrame;
    procedure InvokeHelp;
    function IsCtl3DStored: Boolean;
    function IsDoubleBufferedStored: Boolean;
    function PrecedingWindow(Control: TWinControl): HWND;
    procedure ReadDesignSize(Reader: TReader);
    procedure Remove(AControl: TControl);
    procedure RemoveFocus(Removing: Boolean);
    procedure SetBevelCut(Index: Integer; const Value: TBevelCut);
    procedure SetBevelEdges(const Value: TBevelEdges);
    procedure SetBevelKind(const Value: TBevelKind);
    procedure SetBevelWidth(const Value: TBevelWidth);
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetCtl3D(Value: Boolean);
    procedure SetDockSite(Value: Boolean);
    procedure SetDoubleBuffered(Value: Boolean);
    procedure SetPadding(const Value: TPadding);
    procedure SetParentCtl3D(Value: Boolean);
    procedure SetParentWindow(Value: HWND);
    procedure SetTabOrder(Value: TTabOrder);
    procedure SetTabStop(Value: Boolean);
    procedure SetUseDockManager(Value: Boolean);
    procedure SetZOrderPosition(Position: Integer);
    procedure UpdateTabOrder(Value: TTabOrder);
    procedure UpdateShowing;
    procedure WriteDesignSize(Writer: TWriter);
    function IsMenuKey(var Message: TWMKey): Boolean;
    procedure WMInputLangChange(var Message: TMessage); message WM_INPUTLANGCHANGE;
    procedure CMInputLangChange(var Message: TMessage); message CM_INPUTLANGCHANGE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure WMSysColorChange(var Message: TWMSysColorChange); message WM_SYSCOLORCHANGE;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMCompareItem(var Message: TWMCompareItem); message WM_COMPAREITEM;
    procedure WMDeleteItem(var Message: TWMDeleteItem); message WM_DELETEITEM;
    procedure WMDrawItem(var Message: TWMDrawItem); message WM_DRAWITEM;
    procedure WMMeasureItem(var Message: TWMMeasureItem); message WM_MEASUREITEM;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMSysKeyDown(var Message: TWMSysKeyDown); message WM_SYSKEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    procedure WMSysKeyUp(var Message: TWMSysKeyUp); message WM_SYSKEYUP;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMCharToItem(var Message: TWMCharToItem); message WM_CHARTOITEM;
    procedure WMParentNotify(var Message: TWMParentNotify); message WM_PARENTNOTIFY;
    procedure WMVKeyToItem(var Message: TWMVKeyToItem); message WM_VKEYTOITEM;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMQueryNewPalette(var Message: TMessage); message WM_QUERYNEWPALETTE;
    procedure WMPaletteChanged(var Message: TMessage); message WM_PALETTECHANGED;
    procedure WMWinIniChange(var Message: TMessage); message WM_WININICHANGE;
    procedure WMFontChange(var Message: TMessage); message WM_FONTCHANGE;
    procedure WMTimeChange(var Message: TMessage); message WM_TIMECHANGE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMIMEStartComp(var Message: TMessage); message WM_IME_STARTCOMPOSITION;
    procedure WMIMEEndComp(var Message: TMessage); message WM_IME_ENDCOMPOSITION;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMGesture(var Message: TMessage); message WM_GESTURE;
    procedure WMGestureNotify(var Message: TWMGestureNotify); message WM_GESTURENOTIFY;
    procedure WMTabletQuerySystemGestureStatus(var Message: TMessage); message WM_TABLET_QUERYSYSTEMGESTURESTATUS;
    procedure CMChanged(var Message: TCMChanged); message CM_CHANGED;
    procedure CMChildKey(var Message: TCMChildKey); message CM_CHILDKEY;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMBorderChanged(var Message: TMessage); message CM_BORDERCHANGED;
    procedure CMCursorChanged(var Message: TMessage); message CM_CURSORCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentCtl3DChanged(var Message: TMessage); message CM_PARENTCTL3DCHANGED;
    procedure CMParentDoubleBufferedChanged(var Message: TMessage); message CM_PARENTDOUBLEBUFFEREDCHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMShowHintChanged(var Message: TMessage); message CM_SHOWHINTCHANGED;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure CMWinIniChange(var Message: TWMWinIniChange); message CM_WININICHANGE;
    procedure CMFontChange(var Message: TMessage); message CM_FONTCHANGE;
    procedure CMTimeChange(var Message: TMessage); message CM_TIMECHANGE;
    procedure CMDrag(var Message: TCMDrag); message CM_DRAG;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure CNKeyUp(var Message: TWMKeyUp); message CN_KEYUP;
    procedure CNChar(var Message: TWMChar); message CN_CHAR;
    procedure CNSysKeyDown(var Message: TWMKeyDown); message CN_SYSKEYDOWN;
    procedure CNSysChar(var Message: TWMChar); message CN_SYSCHAR;
    procedure CMRecreateWnd(var Message: TMessage); message CM_RECREATEWND;
    procedure CMInvalidate(var Message: TMessage); message CM_INVALIDATE;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMDoubleBufferedChanged(var Message: TMessage); message CM_DOUBLEBUFFEREDCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMPrintClient(var Message: TWMPrintClient); message WM_PRINTCLIENT;
    procedure CMTabletOptionsChanged(var Message: TMessage); message CM_TABLETOPTIONSCHANGED;
{$IF NOT DEFINED(CLR)}
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMControlListChange(var Message: TMessage); message CM_CONTROLLISTCHANGE;
    procedure CMControlListChanging(var Message: TMessage); message CM_CONTROLLISTCHANGING;
    procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
    procedure CMFloat(var Message: TCMFloat); message CM_FLOAT;
{$ELSE}
    function GetWindowHandle: HWND;
    procedure SetWindowHandle(Value: HWND);
    procedure WMIMEChar(var Message: TWMChar); message WM_IME_CHAR;
  public
    function GetTextPiece(Size: Integer): TCaption; override;
    function GetTextLen: Integer; override;
    procedure SetText(const Value: TCaption); override;
{$IFEND}
  protected
    FDoubleBuffered: Boolean;
    FInImeComposition: Boolean;
    FDesignSize: TPoint;
{$IF DEFINED(CLR)}
    FHandle: THWndWrapper;
{$IFEND}
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure AddBiDiModeExStyle(var ExStyle: DWORD);
    procedure AssignTo(Dest: TPersistent); override;
    procedure AdjustClientRect(var Rect: TRect); virtual;
    procedure AdjustSize; override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); virtual;
    procedure ArrangeControl(AControl: TControl; const ParentSize: TPoint; AAlign: TAlign;
      AAlignInfo: TAlignInfo; var Rect: TRect; UpdateAnchorOrigin: Boolean = False);
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure ChangeScale(M, D: Integer); override;
    procedure ConstrainedResize(var MinWidth, MinHeight, MaxWidth,
      MaxHeight: Integer); override;
    procedure ControlsAligned; dynamic;
    function CreateDockManager: IDockManager; dynamic;
    procedure CreateHandle; virtual;
    procedure CreateParams(var Params: TCreateParams); virtual;
    procedure CreateWindowHandle(const Params: TCreateParams); virtual;
    procedure CreateWnd; virtual;
    function CustomAlignInsertBefore(C1, C2: TControl): Boolean; virtual;
    procedure CustomAlignPosition(Control: TControl; var NewLeft, NewTop, NewWidth,
      NewHeight: Integer; var AlignRect: TRect; AlignInfo: TAlignInfo); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DestroyHandle; virtual;
    procedure DestroyWindowHandle; virtual;
    procedure DestroyWnd; virtual;
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); dynamic;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); dynamic;
    function DockReplaceDockClient(Client: TControl;
      NewDockSite: TWinControl; DropControl: TControl;
      ControlSide: TAlign; ReplacementClient: TControl): Boolean; virtual;
    procedure DoDockOver(Source: TDragDockObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); dynamic;
    procedure DoEnter; dynamic;
    procedure DoExit; dynamic;
    procedure DoFlipChildren; dynamic;
    function DoKeyDown(var Message: TWMKey): Boolean;
    function DoKeyPress(var Message: TWMKey): Boolean;
    function DoKeyUp(var Message: TWMKey): Boolean;
    function DoHandleStyleMessage(var Message: TMessage): Boolean; dynamic;
    procedure DoRemoveDockClient(Client: TControl); dynamic;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; dynamic;
    function FindNextControl(CurControl: TWinControl;
      GoForward, CheckTabStop, CheckParent: Boolean): TWinControl;
    procedure FixupTabList;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetClientOrigin: TPoint; override;
    function GetClientRect: TRect; override;
    function GetControlExtents: TRect; virtual;
    function GetDeviceContext(var WindowHandle: HWND): HDC; overload; override;
    function GetParentHandle: HWND;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); dynamic;
    function GetTopParentHandle: HWnd;
    procedure InvalidateDockHostSite(FocusLost: Boolean);
    function IsControlMouseMsg(var Message: TWMMouse): Boolean;
    function IsControlActivateMsg(var Message: TWMMouseActivate; Control: TControl = nil): Boolean;
    function IsQualifyingSite(const Client: TControl): Boolean; dynamic;
    procedure KeyDown(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyUp(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyPress(var Key: Char); dynamic;
    procedure MainWndProc(var Message: TMessage);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NotifyControls(Msg: Word);
    procedure PaintControls(DC: HDC; First: TControl);
    procedure PaintHandler(var Message: TWMPaint);
    procedure PaintWindow(DC: HDC); virtual;
    function PaletteChanged(Foreground: Boolean): Boolean; override;
    procedure ReadState(Reader: TReader); override;
    procedure RecreateWnd;
    procedure ReloadDockedControl(const AControlName: string;
      var AControl: TControl); dynamic;
    procedure ResetIme;
    function ResetImeComposition(Action: DWORD): Boolean;
    procedure RequestAlign; override;
    procedure ScaleControls(M, D: Integer);
    procedure ScalePadding(M, D: Integer);
    procedure SelectFirst;
    procedure SelectNext(CurControl: TWinControl;
      GoForward, CheckTabStop: Boolean);
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetIme;
    function SetImeCompositionWindow(Font: TFont; XPos, YPos: Integer): Boolean;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetParentBackground(Value: Boolean); virtual;
    procedure SetParentDoubleBuffered(Value: Boolean); virtual;
    procedure SetZOrder(TopMost: Boolean); override;
    procedure ShowControl(AControl: TControl); virtual;
    procedure UpdateBounds;
    procedure UpdateControlOriginalParentSize(AControl: TControl; var AOriginalParentSize: TPoint); virtual;
    procedure UpdateRecreatingFlag(Recreating: Boolean);
    procedure UpdateUIState(CharCode: Word);
    procedure WndProc(var Message: TMessage); override;
{$IF DEFINED(CLR)}
    procedure FocusChanged(NewFocusControl: TWinControl); virtual;
    procedure FloatControl(DockSource: TDragDockObject); override;
    function DockClient(DockSource: TDragDockObject; MousePos: TPoint): Integer; virtual; // replaces use of CM_DockClient
    function UndockClient(NewTarget, Client: TControl): Boolean; virtual; // replaces use of CM_UndockClient
    procedure ControlChange(Inserting: Boolean; Child: TControl); virtual; // replaces use of CM_CONTROLCHANGE
    procedure ControlListChange(Inserting: Boolean; Child: TControl); virtual; // replaces use of CM_CONTROLLISTCHANGE
    procedure ControlListChanging(Inserting: Boolean; Child: TControl; AParent: TWinControl); overload; virtual; deprecated; // replaces use of CM_CONTROLLISTCHANGING
    procedure ControlListChanging(Inserting: Boolean; var Item: TControlListItem); overload; virtual; // replaces use of CM_CONTROLLISTCHANGING
    procedure CreateSubClass(var Params: TCreateParams; ControlClassName: string);
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    function GetDeviceContext(var WindowHandle: THWndWrapper): HDC; overload; override;
{$ELSE}
    procedure CreateSubClass(var Params: TCreateParams; ControlClassName: PChar);
    procedure RemoveWindowProps;
{$IFEND}
    property AlignControlList: TList read FAlignControlList;
    property BevelEdges: TBevelEdges read FBevelEdges write SetBevelEdges default [beLeft, beTop, beRight, beBottom];
    property BevelInner: TBevelCut index 0 read FBevelInner write SetBevelCut default bvRaised;
    property BevelOuter: TBevelCut index 1 read FBevelOuter write SetBevelCut default bvLowered;
    property BevelKind: TBevelKind read FBevelKind write SetBevelKind default bkNone;
    property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
    property Ctl3D: Boolean read FCtl3D write SetCtl3D stored IsCtl3DStored;
    property ImeMode: TImeMode read FImeMode write FImeMode default imDontCare;
    property ImeName: TImeName read FImeName write FImeName;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
    property ParentCtl3D: Boolean read FParentCtl3D write SetParentCtl3D default True;
{$IF DEFINED(CLR)}
    property DefWndProc: TFNWndProc read FDefWndProc write FDefWndProc;
    property WindowHandle: HWND read GetWindowHandle write SetWindowHandle;
{$ELSE}
    property DefWndProc: Pointer read FDefWndProc write FDefWndProc;
    property WindowHandle: HWnd read FHandle write FHandle;
{$IFEND}
    property OnAlignInsertBefore: TAlignInsertBeforeEvent read FOnAlignInsertBefore
      write FOnAlignInsertBefore;
    property OnAlignPosition: TAlignPositionEvent read FOnAlignPosition write FOnAlignPosition;
    property OnDockDrop: TDockDropEvent read FOnDockDrop write FOnDockDrop;
    property OnDockOver: TDockOverEvent read FOnDockOver write FOnDockOver;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnGetSiteInfo: TGetSiteInfoEvent read FOnGetSiteInfo write FOnGetSiteInfo;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnUnDock: TUnDockEvent read FOnUnDock write FOnUnDock;
  public
    constructor Create(AOwner: TComponent); override;
{$IF DEFINED(CLR)}
    class function CreateParented(AParentWindow: HWND): TWinControl;
{$ELSE}
    constructor CreateParented(ParentWindow: HWnd);
{$IFEND}
    class function CreateParentedControl(ParentWindow: HWND): TWinControl;
    destructor Destroy; override;
    procedure Broadcast(var Message);
    function CanFocus: Boolean; dynamic;
    function ContainsControl(Control: TControl): Boolean;
    function ControlAtPos(const Pos: TPoint; AllowDisabled: Boolean;
      AllowWinControls: Boolean = False; AllLevels: Boolean = False): TControl;
    procedure DefaultHandler(var Message); override;
    procedure DisableAlign; inline;
    property DockClientCount: Integer read GetDockClientCount;
    property DockClients[Index: Integer]: TControl read GetDockClients;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); dynamic;
    property DockSite: Boolean read FDockSite write SetDockSite default False;
    property DockManager: IDockManager read FDockManager write FDockManager;
    property DoubleBuffered: Boolean read FDoubleBuffered write SetDoubleBuffered stored IsDoubleBufferedStored;
    procedure EnableAlign;
    function FindChildControl(const ControlName: string): TControl;
    procedure FlipChildren(AllLevels: Boolean); dynamic;
    function Focused: Boolean; dynamic;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure GetTabControlList(List: TList); dynamic;
    procedure GetTabOrderList(List: TList); dynamic;
    function HandleAllocated: Boolean;
    procedure HandleNeeded;
    procedure InsertControl(AControl: TControl);
    procedure Invalidate; override;
    procedure PaintTo(DC: HDC; X, Y: Integer); overload;
    procedure PaintTo(Canvas: TCanvas; X, Y: Integer); overload;
    function PreProcessMessage(var Msg: TMsg): Boolean; dynamic;
    procedure RemoveControl(AControl: TControl);
    procedure Realign; inline;
    procedure Repaint; override;
    procedure ScaleBy(M, D: Integer);
    procedure ScrollBy(DeltaX, DeltaY: Integer);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetDesignVisible(Value: Boolean); override;
    procedure SetFocus; virtual;
    procedure Update; override;
    procedure UpdateControlState;
    property AlignDisabled: Boolean read GetAlignDisabled;
    property MouseInClient: Boolean read FMouseInClient;
    property VisibleDockClientCount: Integer read GetVisibleDockClientCount;
    property Brush: TBrush read FBrush;
    property Controls[Index: Integer]: TControl read GetControl;
    property ControlCount: Integer read GetControlCount;
    property Handle: HWND read GetHandle;
    property Padding: TPadding read FPadding write SetPadding;
    property ParentDoubleBuffered: Boolean read FParentDoubleBuffered write SetParentDoubleBuffered default True;
    property ParentWindow: HWND read FParentWindow write SetParentWindow;
    property Showing: Boolean read FShowing;
    property TabOrder: TTabOrder read GetTabOrder write SetTabOrder default -1;
    property TabStop: Boolean read FTabStop write SetTabStop default False;
    property UseDockManager: Boolean read FUseDockManager write SetUseDockManager default False;
  end;

  TGraphicControl = class(TControl)
  private
    FCanvas: TCanvas;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure Paint; virtual;
{$IF DEFINED(CLR)}


    function get_Canvas: TCanvas;
    property Canvas: TCanvas read get_Canvas;
{$ELSE}
    property Canvas: TCanvas read FCanvas;
{$IFEND}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCustomControl = class(TWinControl)
  private
    FCanvas: TCanvas;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure Paint; virtual;
    procedure PaintWindow(DC: HDC); override;
{$IF NOT DEFINED(CLR)}
    property Canvas: TCanvas read FCanvas;
{$IFEND}
  public
{$IF DEFINED(CLR)}

    function get_Canvas: TCanvas;
    property Canvas: TCanvas read get_Canvas;
    property Color;
{$IFEND}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCustomTransparentControl = class(TCustomControl)
  private
    FInterceptMouse: Boolean;
  protected
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure InvalidateControlsUnderneath;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Invalidate; override;
    property InterceptMouse: Boolean read FInterceptMouse write FInterceptMouse default False;
  end;

  THintWindow = class(TCustomControl)
  private
    FActivating: Boolean;
    FLastActive: Cardinal;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure NCPaint(DC: HDC); virtual;
    procedure Paint; override;
    procedure WMPrint(var Message: TMessage); message WM_PRINT;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ActivateHint(Rect: TRect; const AHint: string); virtual;
    function IsHintMsg(var Msg: TMsg): Boolean; virtual;
    function ShouldHideHint: Boolean; virtual;
    procedure ReleaseHandle;
    procedure ActivateHintData(Rect: TRect; const AHint: string; AData: TCustomData); virtual;
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: TCustomData): TRect; virtual;
    property BiDiMode;
    property Caption;
    property Color;
    property Canvas;
    property Font;
  end;

{ TDragImageList }

  TDragImageList = class(TCustomImageList)
  private
    FDragCursor: TCursor;
    FDragging: Boolean;
    FDragHandle: HWND;
    FDragHotspot: TPoint;
    FDragIndex: Integer;
    FOldCursor: TCursor;
    procedure SetDragCursor(Value: TCursor);
  protected
    procedure Initialize; override;
  public
    function BeginDrag(Window: HWND; X, Y: Integer): Boolean;
    function DragLock(Window: HWND; XPos, YPos: Integer): Boolean;
    function DragMove(X, Y: Integer): Boolean;
    procedure DragUnlock;
    function EndDrag: Boolean;
    function GetHotSpot: TPoint; override;
    procedure HideDragImage;
    function SetDragImage(Index, HotSpotX, HotSpotY: Integer): Boolean;
    procedure ShowDragImage;
    property DragCursor: TCursor read FDragCursor write SetDragCursor;
    property DragHotspot: TPoint read FDragHotspot write FDragHotspot;
    property Dragging: Boolean read FDragging;
  end;

{ TImageList }

  TImageList = class(TDragImageList)
  published
    property BlendColor;
    property BkColor;
    property AllocBy;
    property ColorDepth;
    property DrawingStyle;
    property Height;
    property ImageType;
    property Masked;
    property OnChange;
    property ShareImages;
    property Width;
  end;

{ TDockZone }

  TDockTree = class;

  /// TDockZone encapsulates a region into which other zones are contained.
  /// A TDockZone can be a parent to other zones (when FChildZones <> nil) or
  /// can contain only a control (when FChildControl <> nil).  A TDockZone also
  /// stores pointers to previous and next siblings and its parent.  Parents
  /// store a pointer to only the first child in a doubly-linked list of child
  /// zones, though each child maintains a pointer to its parent.  Thus, the
  /// data structure of relating TDockZones works out to a kind of a
  /// doubly-linked list tree.  The FZoneLimit field of TDockZone represents
  /// the coordinate of either the left or bottom of the zone, depending on
  /// whether its parent zone's orientation is doVertical or doHorizontal.

  TDockZone = class
  private
    FChildControl: TControl;
    FChildZones: TDockZone;
    FNextSibling: TDockZone;
    FOrientation: TDockOrientation;
    FParentZone: TDockZone;
    FPrevSibling: TDockZone;
    FTree: TDockTree;
    FZoneLimit: Integer;
    FOldSize: Integer;
    function GetChildCount: Integer;
    function GetControlName: string;
    function GetLimitBegin: Integer;
    function GetLimitSize: Integer;
    function GetTopLeft(Orient: Integer{TDockOrientation}): Integer;
    function GetHeightWidth(Orient: Integer{TDockOrientation}): Integer;
    function GetVisible: Boolean;
    function GetVisibleChildCount: Integer;
    function GetZoneLimit: Integer;
    function SetControlName(const Value: string): Boolean;
    procedure SetZoneLimit(const Value: Integer); inline;
  public
    constructor Create(Tree: TDockTree);
    procedure ExpandZoneLimit(NewLimit: Integer);
    function FirstVisibleChild: TDockZone;
    function NextVisible: TDockZone;
    function PrevVisible: TDockZone;
    procedure ResetChildren;
    procedure ResetZoneLimits;
    procedure Update;
    property ChildCount: Integer read GetChildCount;
    property ChildControl: TControl read FChildControl;
    property Height: Integer index Ord(doHorizontal) read GetHeightWidth;
    property Left: Integer index Ord(doVertical) read GetTopLeft;
    property LimitBegin: Integer read GetLimitBegin;
    property LimitSize: Integer read GetLimitSize;
    property Top: Integer index Ord(doHorizontal) read GetTopLeft;
    property Visible: Boolean read GetVisible;
    property VisibleChildCount: Integer read GetVisibleChildCount;
    property Width: Integer index Ord(doVertical) read GetHeightWidth;
    property ZoneLimit: Integer read GetZoneLimit write SetZoneLimit;
  end;

{ TDockTree }

  TForEachZoneProc = procedure(Zone: TDockZone) of object;

  TDockTreeClass = class of TDockTree;

  /// TDockTree serves as a manager for a tree of TDockZones.  It is responsible
  /// for inserting and removing controls (and thus zones) from the tree and
  /// associated housekeeping, such as orientation, zone limits, parent zone
  /// creation, and painting of controls into zone bounds.
  TDockTree = class(TInterfacedObject, IDockManager)
  private
    FBorderWidth: Integer;
    FBrush: TBrush;
    FDockSite: TWinControl;
    FGrabberSize: Integer;
    FGrabbersOnTop: Boolean;
    FOldRect: TRect;
    FOldWndProc: TWndMethod;
    FReplacementZone: TDockZone;
    FScaleBy: Double;
    FShiftScaleOrient: TDockOrientation;
    FShiftBy: Integer;
    FSizePos: TPoint;
    FSizingDC: HDC;
    FSizingWnd: HWND;
    FSizingZone: TDockZone;
    FTopZone: TDockZone;
    FTopXYLimit: Integer;
    FUpdateCount: Integer;
    FVersion: Integer;
    FRelativeSizes: Boolean;
    procedure ControlVisibilityChanged(Control: TControl; Visible: Boolean);
    function ActualSize(const RelativeSize, Reference: Integer): Integer;
    function RelativeSize(const ActualSize, Reference: Integer): Integer;
    procedure DrawSizeSplitter;
    function FindControlZone(Control: TControl): TDockZone;
    procedure ForEachAt(Zone: TDockZone; Proc: TForEachZoneProc);
    function GetNextLimit(AZone: TDockZone): Integer;
    procedure InsertNewParent(NewZone, SiblingZone: TDockZone;
      ParentOrientation: TDockOrientation; InsertLast: Boolean);
    procedure InsertSibling(NewZone, SiblingZone: TDockZone; InsertLast: Boolean);
    function InternalHitTest(const MousePos: TPoint; out HTFlag: Integer): TDockZone;
    procedure PruneZone(Zone: TDockZone);
    procedure RemoveZone(Zone: TDockZone);
    procedure ScaleZone(Zone: TDockZone);
    procedure SetNewBounds(Zone: TDockZone);
    procedure ShiftZone(Zone: TDockZone);
    procedure SplitterMouseDown(OnZone: TDockZone; MousePos: TPoint);
    procedure SplitterMouseUp;
    procedure UpdateZone(Zone: TDockZone);
    procedure WindowProc(var Message: TMessage);
  protected
    procedure AdjustDockRect(Control: TControl; var ARect: TRect); virtual;
    procedure AdjustFrameRect(Control: TControl; var ARect: TRect); virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    function FindControlAtPos(const Pos: TPoint): TControl;
    procedure GetControlBounds(Control: TControl; out CtlBounds: TRect);
    function HitTest(const MousePos: TPoint; out HTFlag: Integer): TControl; virtual;
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer;
      var Handled: Boolean); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); virtual;
    procedure PaintDockFrame(Canvas: TCanvas; Control: TControl;
      const ARect: TRect); virtual;
    procedure PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign;
      var DockRect: TRect); virtual;
    function ReferenceFromOrient(const Orient: TDockOrientation): Integer; virtual;
    procedure RemoveControl(Control: TControl); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetReplacingControl(Control: TControl);
    procedure ShowHint(CursorPos: TPoint; var CursorRect: TRect;
      var HintStr: string); virtual;
    procedure ResetBounds(Force: Boolean); virtual;
    procedure UpdateAll;
    procedure WndProc(var Message: TMessage); virtual;
    function ZoneCaptionHitTest(const Zone: TDockZone; const MousePos: TPoint;
      var HTFlag: Integer): Boolean; virtual;
    property DockSite: TWinControl read FDockSite write FDockSite;
    property RelativeSizes: Boolean read FRelativeSizes write FRelativeSizes;
    property TopZone: TDockZone read FTopZone;
  public
    constructor Create(DockSite: TWinControl); virtual;
    destructor Destroy; override;
    procedure PaintSite(DC: HDC); virtual;
  end;

{ Mouse support }

  TPanningWindowClass = class of TCustomPanningWindow;

  { TCustomPanningWindow }
  TCustomPanningWindow = class(TCustomControl)
    function GetIsPanning: Boolean; virtual; abstract;
    function StartPanning(AHandle: THandle; AControl: TControl): Boolean; virtual; abstract;
    procedure StopPanning; virtual; abstract;
  end;

  TMouse = class
  private
    FDragImmediate: Boolean;
    FDragThreshold: Integer;
    FMousePresent: Boolean;
    FNativeWheelSupport: Boolean;
    FScrollLines: Integer;
    FScrollLinesMessage: UINT;
    FWheelHwnd: HWND;
    FWheelMessage: UINT;
    FWheelPresent: Boolean;
    FWheelSupportMessage: UINT;
    FPanningWindow: TCustomPanningWindow;
    FPanningWindowClass: TPanningWindowClass;
    procedure GetMouseData;
    procedure GetNativeData;
    procedure GetRegisteredData;
    function GetCursorPos: TPoint;
    procedure SetCursorPos(const Value: TPoint);
    function GetCapture: HWND;
    procedure SetCapture(const Value: HWND);
    function GetIsDragging: Boolean;
    procedure SetPanningWindow(const Value: TCustomPanningWindow);
    function GetIsPanning: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SettingChanged(Setting: Integer);
    function CreatePanningWindow: TCustomPanningWindow;
    property Capture: HWND read GetCapture write SetCapture;
    property CursorPos: TPoint read GetCursorPos write SetCursorPos;
    property DragImmediate: Boolean read FDragImmediate write FDragImmediate default True;
    property DragThreshold: Integer read FDragThreshold write FDragThreshold default 5;
    property MousePresent: Boolean read FMousePresent;
    property IsDragging: Boolean read GetIsDragging;
    property IsPanning: Boolean read GetIsPanning;
    property PanningWindow: TCustomPanningWindow read FPanningWindow write SetPanningWindow;
    property PanningWindowClass: TPanningWindowClass read FPanningWindowClass write
      FPanningWindowClass;
    property RegWheelMessage: UINT read FWheelMessage;
    property WheelPresent: Boolean read FWheelPresent;
    property WheelScrollLines: Integer read FScrollLines;
  end;

  TCustomListControl = class(TWinControl)
  protected
    function GetItemIndex: Integer; virtual; abstract;
    function IsTouchPropertyStored(AProperty: TTouchProperty): Boolean; override;
    procedure SetItemIndex(const Value: Integer); overload; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddItem(Item: String; AObject: TObject); virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure ClearSelection; virtual; abstract;
    procedure CopySelection(Destination: TCustomListControl); virtual; abstract;
    procedure DeleteSelected; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    procedure MoveSelection(Destination: TCustomListControl); virtual;
    procedure SelectAll; virtual; abstract;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
  end;

  TCustomMultiSelectListControl = class(TCustomListControl)
  protected
    FMultiSelect: Boolean;
    function GetSelCount: Integer; virtual; abstract;
    procedure SetMultiSelect(Value: Boolean); virtual; abstract;
  public
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property SelCount: Integer read GetSelCount;
  end;

  [SuppressUnmanagedCodeSecurity]
  TAnimateWindowProc = function(hWnd: HWND; dwTime: DWORD; dwFlags: DWORD): BOOL; {$IFNDEF CLR}stdcall;{$ENDIF}

  TBalloonHintIcon = (bhInfo, bhWarning, bhError);

  TBalloonHintIconSize = (bhsNormal, bhsLarge, bhsSmall);

  TBalloonHintStyle = (bhsStandard, bhsBalloon);

  TCustomHintShowHideThread = class;

  TCustomHintWindow = class(TCustomControl)
  private
    FHintParent: TCustomHint;
    FPopAbove: Boolean;
    FTitle: string;
    FDescription: string;
    FImageIndex: TImageIndex;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure NCPaint(DC: HDC); virtual;
    procedure Paint; override;
    procedure WMPrint(var Message: TMessage); message WM_PRINT;
    function IsThemed: Boolean;
    function NewStylePainting: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AutoSize;
    procedure PositionAt(Point: TPoint); overload;
    procedure PositionAt(Rect: TRect); overload;
    procedure PositionAtCursor;
    property HintParent: TCustomHint read FHintParent write FHintParent;
    property PopAbove: Boolean read FPopAbove;
    property Title: string read FTitle;
    property Description: string read FDescription;
    property ImageIndex: TImageIndex read FImageIndex;
  end;

  TCustomHintShowHideThread = class(TThread)
  private
    FHintWindowQueue: TThreadList;
    FHintObject: TCustomHint;
    FHideHint: Boolean;
    FActive: Boolean;
    FDisplayTime: Cardinal;
    FWaitEvent: TEvent;
    procedure QueHintWindow(Value: TCustomHintWindow);
  public
    constructor Create(Hint: TCustomHintWindow; HintObject: TCustomHint); overload;
    destructor Destroy; override;
    procedure ResumeWork;
    procedure Execute; override;
    procedure HideHint;
  end;

  TCustomHint = class(TComponent)
  private
    FTitle: string;
    FDescription: string;
    FImages: TImageList;
    FImageIndex: TImageIndex;
    FStyle: TBalloonHintStyle;
    FAnimateThread: TCustomHintShowHideThread;
    FShowDelay: Cardinal;
    FShow: Boolean;
    FHideAfter: Integer;
    FLatestHintControl: TControl;
    FWorkComplete: Boolean;
  protected
    property WorkComplete: Boolean read FWorkComplete;
    procedure ShowAnotherHint;
    procedure SetImages(Value: TImageList);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowHint; overload;
    procedure ShowHint(Point: TPoint); overload;
    procedure ShowHint(Rect: TRect); overload;
    procedure ShowHint(Control: TControl); overload;
    procedure HideHint; overload;
    procedure HideHint(HidingControl: TControl); overload;
    procedure PaintHint(HintWindow: TCustomHintWindow); virtual;
    procedure NCPaintHint(HintWindow: TCustomHintWindow; DC: HDC); virtual;
    procedure SetHintSize(HintWindow: TCustomHintWindow); virtual;
    property ShowingHint: Boolean read FShow;
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property ImageIndex: TImageIndex read FImageIndex write FImageIndex;
  published
    property Images: TImageList read FImages write SetImages;
    property Style: TBalloonHintStyle read FStyle write FStyle default bhsBalloon;
    property Delay: Cardinal read FShowDelay write FShowDelay default 500;
    property HideAfter: Integer read FHideAfter write FHideAfter default -1;
  end;

  TBalloonHint = class(TCustomHint)
  public
    procedure PaintHint(HintWindow: TCustomHintWindow); override;
    procedure SetHintSize(HintWindow: TCustomHintWindow); override;
  end;

const
  cImageMargin = 4;
  cTextVerticalMargin = 4;
  cTextHorizontalMargin = 4;
  cBalloonStemHeight = 10;
  cEdgeRadius = 4;

var
  Mouse: TMouse;
  AnimateWindowProc: TAnimateWindowProc = nil;
{$IF DEFINED(CLR)}
  SupportsAnimateWindow: Boolean = False deprecated 'Use "if Assigned(AnimateWindowProc) then"';
{$IFEND}

{ Drag stuff }

function IsDragObject(Sender: TObject): Boolean;
function IsVCLControl(Handle: HWND): Boolean;
function FindControl(Handle: HWND): TWinControl;
function FindVCLWindow(const Pos: TPoint): TWinControl;
function FindDragTarget(const Pos: TPoint; AllowDisabled: Boolean): TControl;
function GetCaptureControl: TControl;
procedure SetCaptureControl(Control: TControl);
procedure CancelDrag;

{ Misc }

function CursorToString(Cursor: TCursor): string; //inline;
function StringToCursor(const S: string): TCursor; //inline;
procedure GetCursorValues(Proc: TGetStrProc);
function CursorToIdent(Cursor: Longint; var Ident: string): Boolean; //inline;
function IdentToCursor(const Ident: string; var Cursor: Longint): Boolean; //inline;

function GetShortHint(const Hint: string): string;
function GetLongHint(const Hint: string): string;

procedure PerformEraseBackground(Control: TControl; DC: HDC);
procedure PerformBufferedPrintClient(Handle: HWND; const Rect: TRect);

function MouseOriginToShiftState: TShiftState;

function InitWndProc(HWindow: HWND; Msg: UINT; WParam: WPARAM;
  LParam: LPARAM): LRESULT; {$IF NOT DEFINED(CLR)}stdcall;{$IFEND}

var
  DefaultDockTreeClass: TDockTreeClass = TDockTree;
{$IF NOT DEFINED(CLR)}
  CreationControl: TWinControl = nil;
  IsVCLControlHook: function (Handle: HWnd): Boolean of object;
{$ELSE}
  CreationControl: TWinControl;

const
  InitProcDelegate: TFNWndProc = @InitWndProc; // reference to keep the delegate alive
  WindowProcDelegate: TFNWndProc = @DefWindowProc; // keep delegate alive
{$IFEND}

const
  CTL3D_ALL = $FFFF;
{$IF NOT DEFINED(CLR)}
  NullDockSite = TWinControl($FFFFFFFF);
{$ELSE}
var
{$IFEND}
  AnchorAlign: array[TAlign] of TAnchors = (
    { alNone }
    [akLeft, akTop],
    { alTop }
    [akLeft, akTop, akRight],
    { alBottom }
    [akLeft, akRight, akBottom],
    { alLeft }
    [akLeft, akTop, akBottom],
    { alRight }
    [akRight, akTop, akBottom],
    { alClient }
    [akLeft, akTop, akRight, akBottom],
    { alCustom }
    [akLeft, akTop]
    );

var
{$IF DEFINED(CLR)}
  NullDockSite: TWinControl;
{$IFEND}
  NewStyleControls: Boolean;

procedure ChangeBiDiModeAlignment(var Alignment: TAlignment);

function SendAppMessage(Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): LRESULT;
procedure MoveWindowOrg(DC: HDC; DX, DY: Integer);

procedure SetImeMode(hWnd: HWND; Mode: TImeMode);
procedure SetImeName(Name: TImeName);
function Win32NLSEnableIME(hWnd: HWND; Enable: Boolean): Boolean;
function Imm32GetContext(hWnd: HWND): HIMC;
function Imm32ReleaseContext(hWnd: HWND; hImc: HIMC): Boolean;
function Imm32GetConversionStatus(hImc: HIMC; var Conversion, Sentence: DWORD): Boolean;
function Imm32SetConversionStatus(hImc: HIMC; Conversion, Sentence: DWORD): Boolean;
function Imm32SetOpenStatus(hImc: HIMC; fOpen: Boolean): Boolean;
function Imm32SetCompositionWindow(hImc: HIMC; lpCompForm: TCOMPOSITIONFORM): Boolean; overload;
function Imm32SetCompositionFont(hImc: HIMC; var lpLogfont: LOGFONT): Boolean; overload;
{$IF DEFINED(CLR)}
function Imm32GetCompositionString(hImc: HIMC; dWord1: DWORD; lpBuf: IntPtr; dwBufLen: DWORD): Longint;
{$ELSE}
function Imm32SetCompositionWindow(hImc: HIMC; lpCompForm: PCOMPOSITIONFORM): Boolean; overload; deprecated;
function Imm32SetCompositionFont(hImc: HIMC; lpLogfont: PLOGFONT): Boolean; overload; deprecated;
function Imm32GetCompositionString(hImc: HIMC; dWord1: DWORD; lpBuf: pointer; dwBufLen: DWORD): Longint;
{$IFEND}
function Imm32IsIME(hKl: HKL): Boolean;
function Imm32NotifyIME(hImc: HIMC; dwAction, dwIndex, dwValue: DWORD): Boolean;
procedure DragDone(Drop: Boolean);

{ Control types for CM_MOUSEENTER & CM_MOUSELEAVE mouse events }

{$IF DEFINED(CLR)}
const
  meTControl    = 1;
  meTWinControl = 2;
{$IFEND}

implementation

uses
{$IF DEFINED(CLR)}
  System.Collections, System.Reflection, System.Threading, Types,
{$ELSE}
  Winapi.TpcShrd,
{$IFEND}
  System.UIConsts, Vcl.Consts, Vcl.Forms, Winapi.ActiveX, System.Math, Vcl.Themes, Winapi.UxTheme, Winapi.DwmApi,
  System.TypInfo, Vcl.GraphUtil;

{$IF DEFINED(CLR)}
var
  ControlHash: Hashtable;
{$IFEND}

// This function can only be called when handling a mouse message. It uses
// GetMessageExtraInfo to determine the origin (mouse, touch or pen) of the
// message. Touch or pen origins are encoded into the returned TShiftState.
function MouseOriginToShiftState: TShiftState;
const
  MouseOriginMask  = $FFFFFF80;
  MouseOriginPen   = $FF515700;
  MouseOriginTouch = $FF515780;
var
  ExtraInfo: Cardinal;
begin
  ExtraInfo := GetMessageExtraInfo and MouseOriginMask;
  if ExtraInfo = MouseOriginPen then
    Result := [ssPen]
  else if ExtraInfo = MouseOriginTouch then
    Result := [ssTouch]
  else
    Result := [];
end;

{ TCMMouseWheel }

{$IF DEFINED(CLR)}
function TCMMouseWheel.GetShiftState: TShiftState;
begin
  Result := TShiftState(Word(GetWParamLo));
end;

procedure TCMMouseWheel.SetShiftState(const Value: TShiftState);
begin
  SetWParamLo(Word(Value));
end;
{$IFEND}

{ TCMObjectMsg }

{$IF DEFINED(CLR)}
function TCMObjectMsg.GetWObject: TObject;
begin
  if GetWParam <> 0 then // make sure it was assigned
    Result := GCHandle(IntPtr(GetWParam)).Target
  else
    Result := nil;
end;

procedure TCMObjectMsg.SetWObject(const Value: TObject);
begin
  if (GetWParam <> 0) and (IntPtr(FWHandle).ToInt64 <> 0) then
    FWHandle.Free;
  FWHandle := GCHandle.Alloc(Value, GCHandleType.Normal);
  SetWParam(Winapi.Windows.WPARAM(IntPtr(FWHandle)));
end;

function TCMObjectMsg.GetLObject: TObject;
begin
  if GetLParam <> 0 then
    Result := GCHandle(IntPtr(GetLParam)).Target
  else
    Result := nil;
end;

procedure TCMObjectMsg.SetLObject(const Value: TObject);
begin
  if (GetLParam <> 0) and (IntPtr(FLHandle).ToInt64 <> 0) then
    FLHandle.Free;
  FLHandle := GCHandle.Alloc(Value, GCHandleType.Normal);
  SetLParam(Winapi.Windows.LPARAM(IntPtr(FLHandle)));
end;

procedure TCMObjectMsg.FreeHandles;
begin
  if (GetWParam <> 0) and (FWHandle.IsAllocated) then
    FWHandle.Free;
  if (GetLParam <> 0) and FLHandle.IsAllocated then
    FLHandle.Free;
end;

procedure TCMObjectMsg.Finalize;
begin
  FreeHandles;
  inherited;
end;
{$IFEND}

{ TCMHintInfo }

{$IF DEFINED(CLR)}
function TCMHintInfo.GetHintInfo: THintInfo;
begin
  Result := THintInfo(GetLObject);
end;

procedure TCMHintInfo.SetHintInfo(Value: THintInfo);
begin
  SetLObject(TObject(Value));
end;
{$IFEND}

{ TCMParentFontChanged }

{$IF DEFINED(CLR)}
function TCMParentFontChanged.GetFont: TFont;
begin
  Result := TFont(GetLObject);
end;

procedure TCMParentFontChanged.SetFont(const Value: TFont);
begin
  SetLObject(Value);
end;
{$IFEND}

{ TCMControlMsg }

{$IF DEFINED(CLR)}
function TCMControlMsg.GetControl: TControl;
begin
  Result := TControl(GetLObject);
end;

procedure TCMControlMsg.SetControl(const Value: TControl);
begin
  SetLObject(Value);
end;
{$IFEND}

{ TCMWinControlMsg }

{$IF DEFINED(CLR)}
function TCMWinControlMsg.GetControl: TWinControl;
begin
  Result := TWinControl(GetLObject);
end;

procedure TCMWinControlMsg.SetControl(const Value: TWinControl);
begin
  SetLObject(Value);
end;
{$IFEND}

{ TCMDrag }

{$IF DEFINED(CLR)}
destructor TCMDrag.Destroy;
begin
  FreeHandles;
  GC.SuppressFinalize(Self);
  inherited;
end;

function TCMDrag.GetDragMessage: TDragMessage;
begin
  Result := TDragMessage(GetWParamLo);
end;

function TCMDrag.GetLParamDragRec: TDragRec;
begin
  Result := TDragRec(GetLObject);
end;

procedure TCMDrag.SetDragMessage(Value: TDragMessage);
begin
  SetWParamLo(Word(Value));
end;

procedure TCMDrag.SetLParamDragRec(Value: TDragRec);
begin
  SetLObject(Value);
end;
{$IFEND}

{ TCMDockNotification }

{$IF DEFINED(CLR)}
function TCMDockNotification.GetNotifyRect: TDockNotifyRec;
begin
  Result := TDockNotifyRec(System.Object(GetWObject));
end;

procedure TCMDockNotification.SetNotifyRec(Value: TDockNotifyRec);
begin
  SetWObject(TObject(Value));
end;

class operator TCMDockNotification.Explicit(Message: TMessage): TCMDockNotification;
begin
  Result := TCMDockNotification.Create(Message);
end;
{$IFEND}

{ TCMPopupHWndDestroy }

{$IF DEFINED(CLR)}
function TCMPopupHWndDestroy.GetPopupFormInfo: TPopupFormInfo;
begin
  Result := TPopupFormInfo(Marshal.PtrToStructure(IntPtr(WParam), TypeOf(TPopupFormInfo)));
end;

procedure TCMPopupHWndDestroy.SetPopupFormInfo(Value: TPopupFormInfo);
begin
  Marshal.StructureToPtr(TObject(Value), IntPtr(OriginalMessage.WParam), False);
end;

class operator TCMPopupHWndDestroy.Explicit(Message: TMessage): TCMPopupHWndDestroy;
begin
  Result := TCMPopupHWndDestroy.Create(Message);
end;
{$IFEND}

{ TCMCreatePopup }

{$IF DEFINED(CLR)}
class operator TCMCreatePopup.Explicit(Message: TMessage): TCMCreatePopup;
begin
  Result := TCMCreatePopup.Create(Message);
end;
{$IFEND}

{ TCMMouseActivate }

{$IF DEFINED(CLR)}
function TCMMouseActivate.GetMouseActivateRec: TMouseActivateRec;
begin
  Result := TMouseActivateRec(GetLObject);
end;

procedure TCMMouseActivate.SetMouseActivateRec(Value: TMouseActivateRec);
begin
  SetLObject(TObject(Value));
end;
{$IFEND}

{$IF NOT DEFINED(CLR)}
var
  WindowAtom: TAtom;
  ControlAtom: TAtom;
  WindowAtomString: string;
  ControlAtomString: string;
  RM_GetObjectInstance: DWORD;  // registered window message
{$IFEND}

{ BiDiMode support routines }

procedure ChangeBiDiModeAlignment(var Alignment: TAlignment);
begin
  case Alignment of
    taLeftJustify:  Alignment := taRightJustify;
    taRightJustify: Alignment := taLeftJustify;
  end;
end;

{ Initialization window procedure }

function InitWndProc(HWindow: HWND; Msg: UINT; WParam: WPARAM; LParam: LPARAM): LRESULT;
{$IF DEFINED(CLR)}
begin
  CreationControl.WindowHandle := HWindow;
  SetWindowLong(HWindow, GWL_WNDPROC, CreationControl.FHandle.FObjInstance);
  if (GetWindowLong(HWindow, GWL_STYLE) and WS_CHILD <> 0) and
    (GetWindowLong(HWindow, GWL_ID) = 0) then
    SetWindowLong(HWindow, GWL_ID, HWindow);
  if ControlHash = nil then
    ControlHash := Hashtable.Create;
  ControlHash[TObject(HWindow)] := CreationControl;
  Result := CreationControl.FHandle.FObjInstance(HWindow, Msg, WParam, LParam);
end;
{$ELSE}
{$IFDEF LINUX}
type
  TThunkProc = function (HWindow: HWnd; Message, WParam, LParam: Longint): Longint stdcall;
var
  WinControl: TWinControl;
{$ENDIF}
{$IFDEF WIN64}
type
  TThunkProc = function (HWindow: HWnd; Message: Longint; WParam: Winapi.Windows.WPARAM; LParam: Winapi.Windows.LPARAM): LRESULT; stdcall;
var
  WinControl: TWinControl;
{$ENDIF}
begin
  CreationControl.FHandle := HWindow;
  if IsWindowUnicode(HWindow) then
  begin
    SetWindowLongW(HWindow, GWL_WNDPROC, IntPtr(CreationControl.FObjectInstance));
    if (GetWindowLongW(HWindow, GWL_STYLE) and WS_CHILD <> 0) and
      (GetWindowLongW(HWindow, GWL_ID) = 0) then
      SetWindowLongW(HWindow, GWL_ID, HWindow);
  end else
  begin
    SetWindowLongA(HWindow, GWL_WNDPROC, IntPtr(CreationControl.FObjectInstance));
    if (GetWindowLongA(HWindow, GWL_STYLE) and WS_CHILD <> 0) and
      (GetWindowLongA(HWindow, GWL_ID) = 0) then
      SetWindowLongA(HWindow, GWL_ID, HWindow);
  end;
  SetProp(HWindow, MakeIntAtom(ControlAtom), THandle(CreationControl));
  SetProp(HWindow, MakeIntAtom(WindowAtom), THandle(CreationControl));
{$IFDEF LINUX}
  WinControl := CreationControl;
  CreationControl := nil;
  Result := TThunkProc(WinControl.FObjectInstance)(HWindow, Message, WParam, LParam);
{$ENDIF}
{$IFDEF WIN32}
  asm
        PUSH    LParam
        PUSH    WParam
        PUSH    Msg
        PUSH    HWindow
        MOV     EAX,CreationControl
        MOV     CreationControl,0
        CALL    [EAX].TWinControl.FObjectInstance
        MOV     Result,EAX
  end;
{$ENDIF}
{$IFDEF WIN64}

  WinControl := CreationControl;
  CreationControl := nil;
  Result := TThunkProc(WinControl.FObjectInstance)(HWindow, Msg, WParam, LParam);
{$ENDIF}
end;
{$IFEND}

{$IF DEFINED(CLR)}
function ControlFromPointer(ControlPtr: THandle): TWinControl;
begin
  Result := GCHandle(IntPtr(ControlPtr)).Target as TWinControl;
end;

function FindControl(Handle: HWND): TWinControl;
begin
  if ControlHash <> nil then
    Result := TWinControl(ControlHash[TObject(Handle)])
  else
    Result := nil;
end;
{$IFEND}

{$IF NOT DEFINED(CLR)}
function ObjectFromHWnd(Handle: HWnd): TWinControl;
var
  OwningProcess: DWORD;
begin
  if (GetWindowThreadProcessID(Handle, OwningProcess) <> 0) and
     (OwningProcess = GetCurrentProcessID) then
    Result := Pointer(SendMessage(Handle, RM_GetObjectInstance, 0, 0))
  else
    Result := nil;
end;

{ Find a TWinControl given a window handle }
{ The global atom table is trashed when the user logs off.  The extra test
  below protects UI interactive services after the user logs off.
  Added additional tests to enure that Handle is at least within the same
  process since otherwise a bogus result can occur due to problems with
  GlobalFindAtom in Winapi.Windows.  }
function FindControl(Handle: HWnd): TWinControl;
var
  OwningProcess: DWORD;
begin
  Result := nil;
  if (Handle <> 0) and (GetWindowThreadProcessID(Handle, OwningProcess) <> 0) and
     (OwningProcess = GetCurrentProcessId) then
  begin
    if GlobalFindAtom(PChar(ControlAtomString)) = ControlAtom then
      Result := Pointer(GetProp(Handle, MakeIntAtom(ControlAtom)))
    else
      Result := ObjectFromHWnd(Handle);
  end;
end;
{$IFEND}

function IsVCLControl(Handle: HWND): Boolean;
begin
  Result := FindControl(Handle) <> nil;
{$IF NOT DEFINED(CLR)}
  if not Result and Assigned(IsVCLControlHook) then
    Result := IsVCLControlHook(Handle);
{$IFEND}
end;

{ Send message to application object }

[SecurityPermission(SecurityAction.LinkDemand, UnmanagedCode=True)]
function SendAppMessage(Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): LRESULT;
begin
  if Application.Handle <> 0 then
    Result := SendMessage(Application.Handle, Msg, WParam, LParam) else
    Result := 0;
end;

{ Cursor translation function }

function CursorToString(Cursor: TCursor): string;
begin
  Result := System.UIConsts.CursorToString(Cursor);
end;

function StringToCursor(const S: string): TCursor;
begin
  Result := System.UIConsts.StringToCursor(S);
end;

procedure GetCursorValues(Proc: TGetStrProc);
begin
  System.UIConsts.GetCursorValues(Proc);
end;

function CursorToIdent(Cursor: Longint; var Ident: string): Boolean;
begin
  Result := System.UIConsts.CursorToIdent(Cursor, Ident);
end;

function IdentToCursor(const Ident: string; var Cursor: Longint): Boolean;
begin
  Result := System.UIConsts.IdentToCursor(Ident, Cursor);
end;

function GetShortHint(const Hint: string): string;
var
  I: Integer;
begin
  I := AnsiPos('|', Hint);
  if I = 0 then
    Result := Hint else
    Result := Copy(Hint, 1, I - 1);
end;

function GetLongHint(const Hint: string): string;
var
  I: Integer;
begin
  I := AnsiPos('|', Hint);
  if I = 0 then
    Result := Hint else
    Result := Copy(Hint, I + 1, Maxint);
end;

procedure PerformEraseBackground(Control: TControl; DC: HDC);
var
  LastOrigin: TPoint;
begin
  GetWindowOrgEx(DC, LastOrigin);
  SetWindowOrgEx(DC, LastOrigin.X + Control.Left, LastOrigin.Y + Control.Top, nil);
  Control.Parent.Perform(WM_ERASEBKGND, DC, DC);
  SetWindowOrgEx(DC, LastOrigin.X, LastOrigin.Y, nil);
end;

procedure PerformBufferedPrintClient(Handle: HWND; const Rect: TRect);
var
  DC, MemDC: HDC;
  PaintBuffer: HPAINTBUFFER;
begin
  DC := GetDC(Handle);
  try
    PaintBuffer := BeginBufferedPaint(DC, Rect, BPBF_TOPDOWNDIB, nil, MemDC);
    try
      SendMessage(Handle, WM_PRINTCLIENT, MemDC, PRF_CLIENT);
      BufferedPaintMakeOpaque(PaintBuffer, Rect);
    finally
      EndBufferedPaint(PaintBuffer, True);
    end;
  finally
    ReleaseDC(Handle, DC);
  end;
end;

{ Mouse capture management }

var
  CaptureControl: TControl = nil;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
function GetCaptureControl: TControl;
begin
  Result := FindControl(GetCapture);
  if (Result <> nil) and (CaptureControl <> nil) and
    (CaptureControl.Parent = Result) then Result := CaptureControl;
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure SetCaptureControl(Control: TControl);
begin
  ReleaseCapture;
  CaptureControl := nil;
  if Control <> nil then
  begin
    if not (Control is TWinControl) then
    begin
      if Control.Parent = nil then Exit;
      CaptureControl := Control;
      Control := Control.Parent;
    end;
    SetCapture(TWinControl(Control).Handle);
  end;
end;

{ Drag-and-drop management }

type
  TDragOperation = (dopNone, dopDrag, dopDock);

{$IF NOT DEFINED(CLR)}
  PSiteInfoRec = ^TSiteInfoRec;
{$IFEND}
  TSiteInfoRec = record
    Site: TWinControl;
    TopParent: HWND;
  end;

{ TSiteList }

  // TSiteList deals with the relative z-order positions of dock sites
  TSiteList = class(TList)
  public
    procedure AddSite(ASite: TWinControl);
    function Find(ParentWnd: Hwnd; var Index: Integer): Boolean;
    function GetTopSite: TWinControl;
{$IF NOT DEFINED(CLR)}
    procedure Clear; override;
{$IFEND}
  end;

function TSiteList.Find(ParentWnd: Hwnd; var Index: Integer): Boolean;
{$IF DEFINED(CLR)}
var
  SiteInfo: TSiteInfoRec;
{$IFEND}
begin
  Index := 0;
  Result := False;
  while Index < Count do
  begin
{$IF DEFINED(CLR)}
    SiteInfo := TSiteInfoRec(Items[Index]);
    Result := SiteInfo.TopParent = ParentWnd;
{$ELSE}
    Result := (PSiteInfoRec(Items[Index]).TopParent = ParentWnd);
{$IFEND}
    if Result then Exit;
    Inc(Index);
  end;
end;

procedure TSiteList.AddSite(ASite: TWinControl);

  function GetTopParent: HWND;
  var
    NextParent: HWND;
  begin
    NextParent := ASite.Handle;
    Result := NextParent;
    while NextParent <> 0 do
    begin
      Result := NextParent;
      NextParent := GetParent(NextParent);
    end;
  end;

var
{$IF DEFINED(CLR)}
  SI, CurrentInfo: TSiteInfoRec;
{$ELSE}
  SI, CurrentInfo: PSiteInfoRec;
{$IFEND}
  Index: Integer;
begin
{$IF NOT DEFINED(CLR)}
  New(SI);
{$IFEND}
  SI.Site := ASite;
  SI.TopParent := GetTopParent;
  if Find(SI.TopParent, Index) then
  begin
    { Since they share the same parent, correctly set the z order based
      on whoever is higher in the list }
{$IF DEFINED(CLR)}
    CurrentInfo := TSiteInfoRec(Items[Index]);
{$ELSE}
    CurrentInfo := Items[Index];
{$IFEND}
    if IsChild(SI.Site.Handle, CurrentInfo.Site.Handle) then
    begin
      { A parent is being inserted in the list higher than the child.
        We want the child to take precedence, so insert the parent after the
        child }
      Inc(Index);
    end;
{$IF DEFINED(CLR)}
    Insert(Index, TObject(SI));
  end
  else
    Add(TObject(SI));
{$ELSE}
    Insert(Index, SI);
  end
  else
    Add(SI);
{$IFEND}
end;

{$IF NOT DEFINED(CLR)}
procedure TSiteList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Dispose(PSiteInfoRec(Items[I]));
  inherited Clear;
end;
{$IFEND}

function TSiteList.GetTopSite: TWinControl;
var
  Index: Integer;
  DesktopWnd, CurrentWnd: HWND;
begin
  Result := nil;
  if Count = 0 then Exit
{$IF DEFINED(CLR)}
  else if Count = 1 then Result := TSiteInfoRec(Items[0]).Site
{$ELSE}
  else if Count = 1 then Result := PSiteInfoRec(Items[0]).Site
{$IFEND}
  else begin
    DesktopWnd := GetDesktopWindow;
    CurrentWnd := GetTopWindow(DesktopWnd);
    while (Result = nil) and (CurrentWnd <> 0) do
    begin
      if Find(CurrentWnd, Index) then
{$IF DEFINED(CLR)}
        Result := TSiteInfoRec(List[Index]).Site
{$ELSE}
        Result := PSiteInfoRec(List[Index]).Site
{$IFEND}
      else
        CurrentWnd := GetNextWindow(CurrentWnd, GW_HWNDNEXT);
    end;
  end;
end;

var
  DragControl: TControl;
  DragObject: TDragObject;
  DragInternalObject: Boolean;
  DragCapture: HWND;
  DragStartPos: TPoint;
  DragSaveCursor: HCURSOR;
  DragThreshold: Integer;
  ActiveDrag: TDragOperation;
  DragImageList: TDragImageList;
  DockSiteList: TList;
  QualifyingSites: TSiteList;

procedure DragTo(const Pos: TPoint); forward;

function IsDragObject(Sender: TObject): Boolean;
var
  SenderClass: TClass;
begin
  SenderClass := Sender.ClassType;
  Result := True;
  while SenderClass <> nil do
    if SenderClass.ClassName = TDragObject.ClassName then Exit
    else SenderClass := SenderClass.ClassParent;
  Result := False;
end;

{ TDragObject }

procedure TDragObject.Assign(Source: TDragObject);
begin
  FDragTarget := Source.FDragTarget;
  FDragHandle := Source.FDragHandle;
  FDragPos := Source.FDragPos;
  FDragTargetPos := Source.FDragTargetPos;
  FMouseDeltaX := Source.FMouseDeltaX;
  FMouseDeltaY := Source.FMouseDeltaY;
end;

function TDragObject.Capture: HWND;
begin
  Result := AllocateHWND(MainWndProc);
  SetCapture(Result);
end;

procedure TDragObject.Finished(Target: TObject; X, Y: Integer; Accepted: Boolean);
begin
end;

function TDragObject.GetName: string;
begin
  Result := ClassName;
end;

procedure TDragObject.ReleaseCapture(Handle: HWND);
begin
  Winapi.Windows.ReleaseCapture;
  DeallocateHWND(Handle);
end;

[SecurityPermission(SecurityAction.InheritanceDemand, UnmanagedCode=True)]
procedure TDragObject.WndProc(var Msg: TMessage);
var
  P: TPoint;
begin
  try
    case Msg.Msg of
      WM_MOUSEMOVE:
        begin
          P := SmallPointToPoint(TWMMouse(Msg).Pos);
          ClientToScreen(DragCapture, P);
          DragTo(P);
        end;
      WM_CAPTURECHANGED:
        DragDone(False);
      WM_LBUTTONUP:
        DragDone(True);
      WM_RBUTTONUP:
        if FRightClickCancels then
          DragDone(False)
        else
          DragDone(True);
      { Forms.IsKeyMsg sends WM_KEYxxx messages here (+CN_BASE) when a
        TPUtilWindow has the mouse capture. }
      CN_KEYUP:
        if Msg.WParam = VK_CONTROL then DragTo(DragObject.DragPos);
      CN_KEYDOWN:
        begin
{$IF DEFINED(CLR)}
          case Msg.WParam.ToInt64 of
{$ELSE}
          case Msg.WParam of
{$IFEND}
            VK_CONTROL:
              DragTo(DragObject.DragPos);
            VK_ESCAPE:
              begin
                { Consume keystroke and cancel drag operation }
                Msg.Result := 1;
                DragDone(False);
              end;
          end;
        end;
    end;
  except
    if DragControl <> nil then DragDone(False);
    Application.HandleException(Self);
  end;
end;

function TDragObject.GetDragImages: TDragImageList;
begin
  Result := nil;
end;

function TDragObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  if Accepted then Result := crDrag
  else Result := crNoDrop;
end;

procedure TDragObject.HideDragImage;
begin
  // do nothing
end;

function TDragObject.Instance: THandle;
begin
{$IF DEFINED(CLR)}
  Result := THandle(HInstance)
{$ELSE}
  Result := SysInit.HInstance;
{$IFEND}
end;

procedure TDragObject.ShowDragImage;
begin
  // do nothing
end;

procedure TDragObject.MainWndProc(var Message: TMessage);
begin
  try
    WndProc(Message);
  except
    Application.HandleException(Self);
  end;
end;

var
  DragSave: TDragObject;

{$IF NOT DEFINED(CLR)}
procedure TDragObject.BeforeDestruction;
begin
  inherited BeforeDestruction;
  DragSave := nil;
end;

procedure TDragObject.AfterConstruction;
begin
  inherited AfterConstruction;
  DragSave := nil;
end;

{ TDragObjectEx }

procedure TDragObjectEx.BeforeDestruction;
begin
  // Do not call inherited here otherwise DragSave will be cleared and thus
  // we will be unable to automatically free the dragobject.
end;
{$IFEND}

{ TBaseDragControlObject }

constructor TBaseDragControlObject.Create(AControl: TControl);
begin
  inherited Create;
  FControl := AControl;
end;

procedure TBaseDragControlObject.Assign(Source: TDragObject);
begin
  inherited Assign(Source);
  if Source is TBaseDragControlObject then
    FControl := TBaseDragControlObject(Source).FControl;
end;

procedure TBaseDragControlObject.EndDrag(Target: TObject; X, Y: Integer);
begin
  if FControl <> nil then
    FControl.DoEndDrag(Target, X, Y);
end;

procedure TBaseDragControlObject.Finished(Target: TObject; X, Y: Integer; Accepted: Boolean);
begin
  if not Accepted then
  begin
    FControl.DragCanceled;
    Target := nil;
  end;
  EndDrag(Target, X, Y);
end;

{ TDragControlObject }

function TDragControlObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  if Accepted then Result := Control.DragCursor
  else Result := crNoDrop;
end;

function TDragControlObject.GetDragImages: TDragImageList;
begin
  Result := Control.GetDragImages;
end;

procedure TDragControlObject.HideDragImage;
begin
  if Control.GetDragImages <> nil then
    Control.GetDragImages.HideDragImage;
end;

procedure TDragControlObject.ShowDragImage;
begin
  if Control.GetDragImages <> nil then
    Control.GetDragImages.ShowDragImage;
end;

{ TDragControlObjectEx }

{$IF NOT DEFINED(CLR)}
procedure TDragControlObjectEx.BeforeDestruction;
begin
  // Do not call inherited here otherwise DragSave will be cleared and thus
  // we will be unable to automatically free the dragobject.
end;
{$IFEND}

{ TDragDockObject }

constructor TDragDockObject.Create(AControl: TControl);
begin
  inherited Create(AControl);
  RightClickCancels := True;
  FBrush := TBrush.Create;
{$IF DEFINED(CLR) OR DEFINED(MSWINDOWS)}
  FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
{$IFEND}
{$IFDEF LINUX}
  //Pattern Bitmaps do not get xor'd correctly under Wine.
  //So we create a solid colored brush for dock object's drag rect
  FBrush.Color := clGray;
{$ENDIF}
end;

destructor TDragDockObject.Destroy;
begin
  FBrush.Free;
  inherited Destroy;
end;

procedure TDragDockObject.Assign(Source: TDragObject);
begin
  inherited Assign(Source);
  if Source is TDragDockObject then
  begin
    FDropAlign := TDragDockObject(Source).FDropAlign;
    FDropOnControl := TDragDockObject(Source).FDropOnControl;
    FFloating := TDragDockObject(Source).FFloating;
    FDockRect := TDragDockObject(Source).FDockRect;
    FEraseDockRect := TDragDockObject(Source).FEraseDockRect;
    FBrush.Assign(TDragDockObject(Source).FBrush);
  end;
end;

procedure TDragDockObject.SetBrush(Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TDragDockObject.EndDrag(Target: TObject; X, Y: Integer);
begin
  FControl.DoEndDock(Target, X, Y);
end;

procedure TDragDockObject.AdjustDockRect(ARect: TRect);
var
  DeltaX, DeltaY: Integer;

  function AbsMin(Value1, Value2: Integer): Integer;
  begin
    if Abs(Value1) < Abs(Value2) then Result := Value1
    else Result := Value2;
  end;

begin
  { Make sure dock rect is touching mouse point }
  if (ARect.Left > FDragPos.x) or (ARect.Right < FDragPos.x) then
    DeltaX := AbsMin(ARect.Left - FDragPos.x, ARect.Right - FDragPos.x)
  else DeltaX := 0;
  if (ARect.Top > FDragPos.y) or (ARect.Bottom < FDragPos.y) then
    DeltaY := AbsMin(ARect.Top - FDragPos.y, ARect.Bottom - FDragPos.y)
  else DeltaY := 0;
  if (DeltaX <> 0) or (DeltaY <> 0) then
    OffsetRect(FDockRect, -DeltaX, -DeltaY);
end;

procedure TDragDockObject.DrawDragDockImage;
begin
  FControl.DrawDragDockImage(Self);
end;

procedure TDragDockObject.EraseDragDockImage;
begin
  FControl.EraseDragDockImage(Self);
end;

function TDragDockObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  Result := crDefault;
end;

function TDragDockObject.GetEraseWhenMoving: Boolean;
begin
  Result := True;
end;

function TDragDockObject.GetFrameWidth: Integer;
begin
  Result := 4;
end;

{ TDragDockObjectEx }

{$IF NOT DEFINED(CLR)}
procedure TDragDockObjectEx.BeforeDestruction;
begin
  // Do not call inherited here otherwise DragSave will be cleared and thus
  // we will be unable to automatically free the dragobject.
end;
{$IFEND}

{ Drag dock functions }

type
{$IF DEFINED(CLR)}
  TCheckTargetInfo = class
  public
    ClientWnd, TargetWnd: HWND;
    CurrentWnd: HWND;
    MousePos: TPoint;
    Found: Boolean;
    function IsBeforeTargetWindow(Window: HWND; Data: LPARAM): Bool;
  end;
{$ELSE}
  PCheckTargetInfo = ^TCheckTargetInfo;
  TCheckTargetInfo = record
    ClientWnd, TargetWnd: HWnd;
    CurrentWnd: HWnd;
    MousePos: TPoint;
    Found: Boolean;
  end;
{$IFEND}

{$IF DEFINED(CLR)}
function TCheckTargetInfo.IsBeforeTargetWindow(Window: HWND; Data: LPARAM): Bool;
{$ELSE}
function IsBeforeTargetWindow(Window: HWnd; Data: Longint): Bool; stdcall;
{$IFEND}
var
  R: TRect;
begin
{$IF NOT DEFINED(CLR)}
  with PCheckTargetInfo(Data)^ do
{$IFEND}
  begin
    if Window = TargetWnd then
      Result := False
    else
    begin
      if CurrentWnd = 0 then
      begin
        GetWindowRect(Window, R);
        if PtInRect(R, MousePos) then
          CurrentWnd := Window;
      end;
      if Window = CurrentWnd then
      begin
        Result := False;
        Found := True;
      end
      else if Window = ClientWnd then
      begin
        Result := True;
        CurrentWnd := 0; // Look for next window
      end
      else
        Result := True;
    end;
  end;
end;

function DragFindWindow(const Pos: TPoint): HWND; forward;

function GetDockSiteAtPos(MousePos: TPoint; Client: TControl): TWinControl;
var
  I: Integer;
  R: TRect;
  Site: TWinControl;
  CanDock: Boolean;

  function ValidDockTarget(Target: TWinControl): Boolean;
  var
    Info: TCheckTargetInfo;
    Control: TWinControl;
    R1, R2: TRect;
  begin
    Result := True;
{$IF DEFINED(CLR)}
    Info := TCheckTargetInfo.Create;
{$IFEND}

    { Find handle for topmost container of current }
    Info.CurrentWnd := DragFindWindow(MousePos);
    if (GetWindow(Info.CurrentWnd, GW_OWNER) <> Application.Handle) then
    begin
      Control := FindControl(Info.CurrentWnd);
      if Control = nil then Exit;
      while Control.Parent <> nil do Control := Control.Parent;
      Info.CurrentWnd := Control.Handle;
    end;
    if Info.CurrentWnd = 0 then Exit;

    { Find handle for topmost container of target }
    Control := Target;
    while Control.Parent <> nil do Control := Control.Parent;
    Info.TargetWnd := Control.Handle;
    if Info.CurrentWnd = Info.TargetWnd then Exit;

    { Find handle for topmost container of client }
    if Client.Parent <> nil then
    begin
      Control := Client.Parent;
      while Control.Parent <> nil do Control := Control.Parent;
      Info.ClientWnd := Control.Handle;
    end
    else if Client is TWinControl then
      Info.ClientWnd := TWinControl(Client).Handle
    else
      Info.ClientWnd := 0;

    Info.Found := False;
    Info.MousePos := MousePos;
{$IF DEFINED(CLR)}

    EnumThreadWindows(GetCurrentThreadID, Info.IsBeforeTargetWindow, 0);
{$ELSE}
    EnumThreadWindows(GetCurrentThreadID, @IsBeforeTargetWindow, Winapi.Windows.LPARAM(@Info));
{$IFEND}
    { CurrentWnd is in front of TargetWnd, so check whether they're overlapped. }
    if Info.Found then
    begin
      GetWindowRect(Info.CurrentWnd, R1);
      Target.GetSiteInfo(Client, R2, MousePos, CanDock);
      { Docking control's host shouldn't count as an overlapped window }
      if DragObject is TDragDockObject
      and (TDragDockObject(DragObject).Control.HostDockSite <> nil)
      and (TDragDockObject(DragObject).Control.HostDockSite.Handle = Info.CurrentWnd) then
        Exit;
      if IntersectRect(R1, R1, R2) then
        Result := False;
    end;
  end;

  function IsSiteChildOfClient: Boolean;
  begin
    if Client is TWinControl then
      Result := IsChild(TWinControl(Client).Handle, Site.Handle)
    else
      Result := False;
  end;

begin
  Result := nil;
  if (DockSiteList = nil) or
     not (Application.AutoDragDocking xor ((GetKeyState(VK_CONTROL) and not $7FFF) <> 0)) then
    Exit;
  QualifyingSites.Clear;
  for I := 0 to DockSiteList.Count - 1 do
  begin
    Site := TWinControl(DockSiteList[I]);
    if (Site <> Client) and Site.Showing and Site.Enabled and
      IsWindowVisible(Site.Handle) and (not IsSiteChildOfClient) and
      Site.IsQualifyingSite(Client) then
    begin
      CanDock := True;
      Site.GetSiteInfo(Client, R, MousePos, CanDock);
      if CanDock and PtInRect(R, MousePos) then
        QualifyingSites.AddSite(Site);
    end;
  end;
  if QualifyingSites.Count > 0 then
    Result := QualifyingSites.GetTopSite;
  if (Result <> nil) and not ValidDockTarget(Result) then
    Result := nil;
end;

procedure RegisterDockSite(Site: TWinControl; DoRegister: Boolean);
var
  Index: Integer;
begin
  if (Site <> nil) then
  begin
    if DockSiteList = nil then DockSiteList := TList.Create;
{$IF DEFINED(CLR)}
    Index := DockSiteList.IndexOf(Site);
{$ELSE}
    Index := DockSiteList.IndexOf(Pointer(Site));
{$IFEND}
    if DoRegister then
    begin
      if Index = -1 then DockSiteList.Add(Site);
    end
    else
    begin
      if Index <> -1 then DockSiteList.Delete(Index);
    end;
  end;
end;

{ Drag drop functions }

{$IF DEFINED(CLR)}
var
  DragMsgRef: TCMDrag; // Reference to most recent CMDrag to prevent GC

function DragMessage(Handle: HWND; Msg: TDragMessage;
  Source: TDragObject; Target: TObject; const Pos: TPoint): Longint;
var
  DragRec: TDragRec;
  DragMsg: TCMDrag;
begin
  Result := 0;
  if Handle <> 0 then
  begin
    DragRec := TDragRec.Create;
    DragRec.Pos := Pos;
    DragRec.Target := Target;
    DragRec.Source := Source;
    DragRec.Docking := ActiveDrag = dopDock;
    // Create a CM_Drag cracker to handle all the marshalling requirements
    DragMsg := TCMDrag.Create;
    DragMsg.DragMessage := Msg;
    DragMsg.DragRec := DragRec;
    Result := SendMessage(Handle, CM_DRAG, DragMsg.OriginalMessage.WParam, DragMsg.OriginalMessage.LParam);
    // Save reference to CMDrag object to prevent premature garbage collection
    if Assigned(DragMsgRef) then
      DragMsgRef.Free;
    DragMsgRef := DragMsg;
  end;
end;
{$ELSE}
function DragMessage(Handle: HWND; Msg: TDragMessage;
  Source: TDragObject; Target: Pointer; const Pos: TPoint): Longint;
var
  DragRec: TDragRec;
begin
  Result := 0;
  if Handle <> 0 then
  begin
    DragRec.Pos := Pos;
    DragRec.Target := Target;
    DragRec.Source := Source;
    DragRec.Docking := ActiveDrag = dopDock;
    Result := SendMessage(Handle, CM_DRAG, Winapi.Windows.WPARAM(Msg), Winapi.Windows.LPARAM(@DragRec));
  end;
end;
{$IFEND}

{$IF DEFINED(CLR)}
function DragTargetMessage(Handle: HWND; Msg: TDragMessage;
  Source: TDragObject; const Pos: TPoint): TObject;
var
  DragRec: TDragRec;
  DragMsg: TCMDrag;
begin
  Result := nil;
  if Handle <> 0 then
  begin
    DragRec := TDragRec.Create;
    DragRec.Pos := Pos;
    DragRec.Target := nil;
    DragRec.Source := Source;
    DragRec.Docking := ActiveDrag = dopDock;
    // Create a CM_Drag cracker to handle all the marshalling requirements
    DragMsg := TCMDrag.Create;
    DragMsg.DragMessage := Msg;
    DragMsg.DragRec := DragRec;
    SendMessage(Handle, CM_DRAG, DragMsg.OriginalMessage.WParam, DragMsg.OriginalMessage.LParam);
    Result := DragRec.Target;
    // Save reference to CMDrag object to prevent premature garbage collection
    if Assigned(DragMsgRef) then
      DragMsgRef.Free;
    DragMsgRef := DragMsg;
  end;
end;
{$IFEND}

// See comments for FindControl about global atom stability in service apps.

function IsDelphiHandle(Handle: HWND): Boolean;
var
  OwningProcess: DWORD;
begin
  Result := False;
  if (Handle <> 0) and (GetWindowThreadProcessID(Handle, OwningProcess) <> 0) and
     (OwningProcess = GetCurrentProcessId) then
  begin
{$IF DEFINED(CLR)}
    Result := FindControl(Handle) <> nil;
{$ELSE}
    if GlobalFindAtom(PChar(WindowAtomString)) = WindowAtom then
      Result := GetProp(Handle, MakeIntAtom(WindowAtom)) <> 0
    else
      Result := ObjectFromHWnd(Handle) <> nil;
{$IFEND}
  end;
end;

function DragFindWindow(const Pos: TPoint): HWND;
begin
  Result := WindowFromPoint(Pos);
  while Result <> 0 do
    if not IsDelphiHandle(Result) then Result := GetParent(Result)
    else Exit;
end;

{$IF DEFINED(CLR)}
function DragFindTarget(const Pos: TPoint; var Handle: HWND;
  DragKind: TDragKind; Client: TControl): TObject;
{$ELSE}
function DragFindTarget(const Pos: TPoint; var Handle: HWND;
  DragKind: TDragKind; Client: TControl): Pointer;
{$IFEND}
begin
  if DragKind = dkDrag then
  begin
    Handle := DragFindWindow(Pos);
{$IF DEFINED(CLR)}
    Result := DragTargetMessage(Handle, dmFindTarget, DragObject, Pos);
{$ELSE}
    Result := Pointer(DragMessage(Handle, dmFindTarget, DragObject, nil, Pos));
{$IFEND}
  end
  else begin
    Result := GetDockSiteAtPos(Pos, Client);
    if Result <> nil then
      Handle := TWinControl(Result).Handle;
  end;
end;

function DoDragOver(DragMsg: TDragMessage): Boolean;
begin
  Result := False;
  if DragObject.DragTarget <> nil then
    Result := LongBool(DragMessage(DragObject.DragHandle, DragMsg, DragObject,
      DragObject.DragTarget, DragObject.DragPos));
end;

procedure DragTo(const Pos: TPoint);

  function GetDropCtl: TControl;
  var
    NextCtl: TControl;
    TargetCtl: TWinControl;
    CtlIdx: Integer;
  begin
    Result := nil;
    TargetCtl := TWinControl(TDragObject(DragObject).DragTarget);
    if (TargetCtl = nil) or not TargetCtl.UseDockManager or
      (TargetCtl.FDockClients = nil) or (TargetCtl.DockClientCount = 0) or
      ((TargetCtl.DockClientCount = 1) and
        (TargetCtl.FDockClients[0] = TDragDockObject(DragObject).Control)) then
      Exit;
    NextCtl := FindDragTarget(DragObject.DragPos, False);
    while (NextCtl <> nil) and (NextCtl <> TargetCtl) do
    begin
      CtlIdx := TargetCtl.FDockClients.IndexOf(NextCtl);
      if CtlIdx <> -1 then
      begin
        Result := TargetCtl.DockClients[CtlIdx];
        Exit;
      end
      else
        NextCtl := NextCtl.Parent;
    end;
  end;

var
  DragCursor: TCursor;
  Target: TControl;
  TargetHandle: HWND;
  DoErase: Boolean;
begin
  if (ActiveDrag <> dopNone) or (Abs(DragStartPos.X - Pos.X) >= DragThreshold) or
    (Abs(DragStartPos.Y - Pos.Y) >= DragThreshold) then
  begin
    Target := TControl(DragFindTarget(Pos, TargetHandle, DragControl.DragKind, DragControl));
    if (ActiveDrag = dopNone) and (DragImageList <> nil) then
      with DragStartPos do DragImageList.BeginDrag(GetDeskTopWindow, X, Y);
    if DragControl.DragKind = dkDrag then
    begin
      ActiveDrag := dopDrag;
      DoErase := False;
    end
    else
    begin
      DoErase := ActiveDrag <> dopNone;
      ActiveDrag := dopDock;
    end;
    if Target <> DragObject.DragTarget then
    begin
      DoDragOver(dmDragLeave);
      if DragObject = nil then Exit;
      DragObject.DragTarget := Target;
      DragObject.DragHandle := TargetHandle;
      DragObject.DragPos := Pos;
      DoDragOver(dmDragEnter);
      if DragObject = nil then Exit;
    end;
    DragObject.DragPos := Pos;
    if DragObject.DragTarget <> nil then
      DragObject.DragTargetPos := TControl(DragObject.DragTarget).ScreenToClient(Pos);
    DragCursor := TDragObject(DragObject).GetDragCursor(DoDragOver(dmDragMove),
      Pos.X, Pos.Y);
    if DragImageList <> nil then
    begin
      if (Target = nil) or (csDisplayDragImage in Target.ControlStyle) or
          DragObject.AlwaysShowDragImages then
      begin
        DragImageList.DragCursor := DragCursor;
        if not DragImageList.Dragging then
          DragImageList.BeginDrag(GetDeskTopWindow, Pos.X, Pos.Y)
        else DragImageList.DragMove(Pos.X, Pos.Y);
      end
      else
      begin
        DragImageList.EndDrag;
        Winapi.Windows.SetCursor(Screen.Cursors[DragCursor]);
      end;
    end;
    Winapi.Windows.SetCursor(Screen.Cursors[DragCursor]);
    if ActiveDrag = dopDock then
    begin
      with TDragDockObject(DragObject) do
      begin
        if Target = nil then
          Control.DockTrackNoTarget(TDragDockObject(DragObject), Pos.X, Pos.Y)
        else
        begin
          FDropOnControl := GetDropCtl;
          if FDropOnControl = nil then
            with DragObject do
              FDropAlign := TWinControl(DragTarget).GetDockEdge(DragTargetPos)
          else
            FDropAlign := FDropOnControl.GetDockEdge(FDropOnControl.ScreenToClient(Pos));
        end;
      end;
      if DragObject <> nil then
        with TDragDockObject(DragObject) do
{$IF DEFINED(CLR)}
          if not TObject(FDockRect).Equals(TObject(FEraseDockRect)) then
{$ELSE}
          if not CompareMem(@FDockRect, @FEraseDockRect, SizeOf(TRect)) then
{$IFEND}
          begin
            if DoErase and EraseWhenMoving then EraseDragDockImage;
            DrawDragDockImage;
            FEraseDockRect := FDockRect;
          end;
    end;
  end;
end;

procedure DragInit(ADragObject: TDragObject; Immediate: Boolean; Threshold: Integer);
begin
  DragObject := ADragObject;
  DragObject.DragTarget := nil;
  GetCursorPos(DragStartPos);
  DragObject.DragPos := DragStartPos;
  DragSaveCursor := Winapi.Windows.GetCursor;
  DragCapture := DragObject.Capture;
  DragThreshold := Threshold;
  if ADragObject is TDragDockObject then
  begin
    with TDragDockObject(ADragObject), FDockRect do
    begin
      if Right - Left > 0 then
        FMouseDeltaX :=  (DragPos.x - Left) / (Right - Left) else
        FMouseDeltaX := 0;
      if Bottom - Top > 0 then
        FMouseDeltaY :=  (DragPos.y - Top) / (Bottom - Top) else
        FMouseDeltaY := 0;
      if Immediate then
      begin
        ActiveDrag := dopDock;
        DrawDragDockImage;
      end
      else ActiveDrag := dopNone;
    end;
  end
  else begin
    if Immediate then ActiveDrag := dopDrag
    else ActiveDrag := dopNone;
  end;
  DragImageList := DragObject.GetDragImages;
  if DragImageList <> nil then
    with DragStartPos do DragImageList.BeginDrag(GetDeskTopWindow, X, Y);
  QualifyingSites := TSiteList.Create;
  if ActiveDrag <> dopNone then DragTo(DragStartPos);
end;

procedure DragInitControl(Control: TControl; Immediate: Boolean; Threshold: Integer);
var
  DragObject: TDragObject;
  StartPos: TPoint;
begin
  DragControl := Control;
  try
    DragObject := nil;
    DragInternalObject := False;
    if Control.FDragKind = dkDrag then
    begin
      Control.DoStartDrag(DragObject);
      if DragControl = nil then Exit;
      if DragObject = nil then
      begin
{$IF DEFINED(CLR)}
        DragObject := TDragControlObject.Create(Control);
{$ELSE}
        DragObject := TDragControlObjectEx.Create(Control);
{$IFEND}
        DragInternalObject := True;
      end
    end
    else
    begin
      Control.DoStartDock(DragObject);
      if DragControl = nil then Exit;
      if DragObject = nil then
      begin
{$IF DEFINED(CLR)}
        DragObject := TDragDockObject.Create(Control);
{$ELSE}
        DragObject := TDragDockObjectEx.Create(Control);
{$IFEND}
        DragInternalObject := True;
      end;
      with TDragDockObject(DragObject) do
      begin
        if Control is TWinControl then
          GetWindowRect(TWinControl(Control).Handle, FDockRect)
        else
        begin
          if (Control.Parent = nil) and not (Control is TWinControl) then
          begin
            GetCursorPos(StartPos);
            FDockRect.TopLeft := StartPos;
          end
          else
            FDockRect.TopLeft := Control.ClientToScreen(Point(0, 0));
          FDockRect.BottomRight := Point(FDockRect.Left + Control.Width,
            FDockRect.Top + Control.Height);
        end;
        FEraseDockRect := FDockRect;
      end;
    end;
    DragInit(DragObject, Immediate, Threshold);
  except
    DragControl := nil;
    raise;
  end;
end;

procedure DragDone(Drop: Boolean);

  function CheckUndock: Boolean;
  begin
    Result := DragObject.DragTarget <> nil;
    with DragControl do
      if Drop and (ActiveDrag = dopDock) then
        if Floating or (FHostDockSite = nil) then
          Result := True
        else if FHostDockSite <> nil then
          Result := FHostDockSite.DoUnDock(TWinControl(DragObject.DragTarget), DragControl);
  end;

var
  SaveDrag: TDragObject;
  DockObject: TDragDockObject;
  Accepted: Boolean;
  DragMsg: TDragMessage;
  TargetPos: TPoint;
  ParentForm: TCustomForm;
begin
  DockObject := nil;
  Accepted := False;
  if (DragObject = nil) or DragObject.Cancelling then Exit;  // recursion control
  try
    DragSave := DragObject;
    try
      DragObject.Cancelling := True;
      DragObject.FDropped := Drop;
      DragObject.ReleaseCapture(DragCapture);
      if ActiveDrag = dopDock then
      begin
        DockObject := DragObject as TDragDockObject;
        DockObject.EraseDragDockImage;
        DockObject.Floating := DockObject.DragTarget = nil;
      end;
      if (DragObject.DragTarget <> nil) and
        (TObject(DragObject.DragTarget) is TControl) then
        TargetPos := DragObject.DragTargetPos
      else
        TargetPos := DragObject.DragPos;
      Accepted := CheckUndock and
        (((ActiveDrag = dopDock) and DockObject.Floating) or
        ((ActiveDrag <> dopNone) and DoDragOver(dmDragLeave))) and
        Drop;
      if ActiveDrag = dopDock then
      begin
        if Accepted and DockObject.Floating then
        begin
          ParentForm := GetParentForm(DockObject.Control);
          if (ParentForm <> nil) and
            (ParentForm.ActiveControl = DockObject.Control) then
            ParentForm.ActiveControl := nil;
{$IF DEFINED(CLR)}
          DragControl.FloatControl(DragObject as TDragDockObject);
{$ELSE}
          DragControl.Perform(CM_FLOAT, 0, Winapi.Windows.LPARAM(DragObject));
{$IFEND}
        end;
      end
      else
      begin
        if DragImageList <> nil then
          DragImageList.EndDrag
        else
          Winapi.Windows.SetCursor(DragSaveCursor);
      end;
      DragControl := nil;
{$IF DEFINED(CLR)}
      // Release referenced CMDrag object
      if Assigned(DragMsgRef) then
      begin
        DragMsgRef.Free;
        DragMsgRef := nil;
      end;
{$IFEND}
      DragObject := nil;
      if Assigned(DragSave) and (DragSave.DragTarget <> nil) then
      begin
        DragMsg := dmDragDrop;
        if not Accepted then
        begin
          DragMsg := dmDragCancel;
          DragSave.FDragPos.X := 0;
          DragSave.FDragPos.Y := 0;
          TargetPos.X := 0;
          TargetPos.Y := 0;
        end;
        SaveDrag := DragSave;
        try
          DragMessage(DragSave.DragHandle, DragMsg, DragSave,
            DragSave.DragTarget, DragSave.DragPos);
        finally
          if DragSave = nil then
            DragSave := SaveDrag;
        end;
      end;
    finally
      QualifyingSites.Free;
      QualifyingSites := nil;
      if Assigned(DragSave) then
      begin
        DragSave.Cancelling := False;
        DragSave.Finished(DragSave.DragTarget, TargetPos.X, TargetPos.Y, Accepted);
      end;
{$IF DEFINED(CLR)}
      // Release referenced CMDrag object
      if Assigned(DragMsgRef) then
      begin
        DragMsgRef.Free;
        DragMsgRef := nil;
      end;
{$IFEND}
      DragObject := nil;
    end;
  finally
    DragControl := nil;
{$IF DEFINED(CLR)}
    DragSave := nil;
{$ELSE}
    if Assigned(DragSave) and ((DragSave is TDragControlObjectEx) or (DragSave is TDragObjectEx) or
       (DragSave is TDragDockObjectEx)) then
      DragSave.Free;
{$IFEND}
    ActiveDrag := dopNone;
  end;
end;

procedure CancelDrag;
begin
  if DragObject <> nil then DragDone(False);
  DragControl := nil;
end;

function FindVCLWindow(const Pos: TPoint): TWinControl;
var
  Handle: HWND;
begin
  Handle := WindowFromPoint(Pos);
  Result := nil;
  while Handle <> 0 do
  begin
    Result := FindControl(Handle);
    if Result <> nil then Exit;
    Handle := GetParent(Handle);
  end;
end;

function FindDragTarget(const Pos: TPoint; AllowDisabled: Boolean): TControl;
var
  Window: TWinControl;
  Control: TControl;
begin
  Result := nil;
  Window := FindVCLWindow(Pos);
  if Window <> nil then
  begin
    Result := Window;
    Control := Window.ControlAtPos(Window.ScreenToClient(Pos), AllowDisabled);
    if Control <> nil then Result := Control;
  end;
end;

{ List helpers }

{$IF DEFINED(CLR)}
procedure ListAdd(var List: TList; Item: TObject);
{$ELSE}
procedure ListAdd(var List: TList; Item: Pointer);
{$IFEND}
begin
  if List = nil then List := TList.Create;
  List.Add(Item);
end;

{$IF DEFINED(CLR)}
procedure ListRemove(var List: TList; Item: TObject);
{$ELSE}
procedure ListRemove(var List: TList; Item: Pointer);
{$IFEND}
var
  Count: Integer;
begin
  Count := List.Count;
  if Count > 0 then
  begin
    { On destruction usually the last item is deleted first }
    if List[Count - 1] = Item then
      List.Delete(Count - 1)
    else
      List.Remove(Item);
  end;
  if List.Count = 0 then
  begin
    List.Free;
    List := nil;
  end;
end;

{ Miscellaneous routines }

procedure MoveWindowOrg(DC: HDC; DX, DY: Integer);
var
  P: TPoint;
begin
  GetWindowOrgEx(DC, P);
  SetWindowOrgEx(DC, P.X - DX, P.Y - DY, nil);
end;

{ Object implementations }

{ TControlCanvas }

const
  CanvasListCacheSize = 4;

var
  CanvasList: TThreadList;

// Free the first available device context
procedure FreeDeviceContext;
var
  I: Integer;
begin
  with CanvasList.LockList do
  try
    for I := 0 to Count-1 do
      with TControlCanvas(Items[I]) do
        if TryLock then
        try
          FreeHandle;
          Exit;
        finally
          Unlock;
        end;
  finally
    CanvasList.UnlockList;
  end;
end;

procedure FreeDeviceContexts;
var
  I: Integer;
begin
  with CanvasList.LockList do
  try
    for I := Count-1 downto 0 do
      with TControlCanvas(Items[I]) do
        if TryLock then
        try
          FreeHandle;
        finally
          Unlock;
        end;
  finally
    CanvasList.UnlockList;
  end;
end;

{$IF DEFINED(CLR)}
constructor TCanvasDC.Create;
begin
  inherited;
  FNotifyDelegate := @FinalizeNotify;
end;

procedure TCanvasDC.Finalize;
begin
  if FDC <> 0 then
  begin
    SelectObject(FDC, GetStockObject(BLACK_PEN));
    SelectObject(FDC, GetStockObject(HOLLOW_BRUSH));
    SelectObject(FDC, GetStockObject(SYSTEM_FONT));
  end;
  if Assigned(FWindowHandle) and (FWindowHandle.Handle <> 0) then
  begin
    FWindowHandle.UnregisterFinalizeNotify(FNotifyDelegate);
    if FDC <> 0 then
      ReleaseDC(FWindowHandle.Handle, FDC);
    FWindowHandle := nil;
  end else if FDC <> 0 then
    ReleaseDC(0, FDC);
  FDC := 0;
end;

procedure TCanvasDC.FinalizeNotify(Handle: HWND);
begin
  if Assigned(FWindowHandle) and (FDC <> 0) then
  begin
    ReleaseDC(FWindowHandle.Handle, FDC);
    FDC := 0;
  end;
  FWindowHandle := nil;
end;
{$IFEND}

destructor TControlCanvas.Destroy;
begin
  FreeHandle;
  inherited Destroy;
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TControlCanvas.CreateHandle;
begin
{$IF DEFINED(CLR)}
  if FControl = nil then
    inherited CreateHandle
  else
  begin
    if FDeviceContext = nil then
      FDeviceContext := TCanvasDC.Create;
    if FDeviceContext.FDC = 0 then
    begin
      with CanvasList.LockList do
      try
        if Count >= CanvasListCacheSize then FreeDeviceContext;
        FDeviceContext.FDC := FControl.GetDeviceContext(FDeviceContext.FWindowHandle);
        if Assigned(FDeviceContext.FWindowHandle) then
          FDeviceContext.FWindowHandle.RegisterFinalizeNotify(FDeviceContext.FNotifyDelegate);
        Add(Self);
      finally
        CanvasList.UnlockList;
      end;
    end;
    Handle := FDeviceContext.FDC;
    UpdateTextFlags;
  end;
{$ELSE}
  if FControl = nil then
    inherited CreateHandle
  else
  begin
    // Creation of a window could trigger messages that require
    // the canvas to have a valid handle. Prevents creating two DCs.
    if (FDeviceContext = 0) and (FControl is TWinControl) then
      TWinControl(FControl).HandleNeeded;
    if FDeviceContext = 0 then
    begin
      with CanvasList.LockList do
      try
        if Count >= CanvasListCacheSize then FreeDeviceContext;
        FDeviceContext := FControl.GetDeviceContext(FWindowHandle);
        Add(Self);
      finally
        CanvasList.UnlockList;
      end;
    end;
    Handle := FDeviceContext;
    UpdateTextFlags;
  end;
{$IFEND}
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TControlCanvas.FreeHandle;
begin
{$IF DEFINED(CLR)}
  if (FDeviceContext <> nil) and (FDeviceContext.FDC <> 0) then
  begin
    Handle := 0;
    CanvasList.RemoveItem(Self, TList.TDirection.FromEnd);
    if Assigned(FDeviceContext.FWindowHandle) then
      ReleaseDC(FDeviceContext.FWindowHandle.Handle, FDeviceContext.FDC)
    else
      ReleaseDC(0, FDeviceContext.FDC);
    FDeviceContext.FDC := 0;
  end;
{$ELSE}
  if FDeviceContext <> 0 then
  begin
    Handle := 0;
    CanvasList.RemoveItem(Self, TList.TDirection.FromEnd);
    ReleaseDC(FWindowHandle, FDeviceContext);
    FDeviceContext := 0;
  end;
{$IFEND}
end;

procedure TControlCanvas.SetControl(AControl: TControl);
begin
  if FControl <> AControl then
  begin
    FreeHandle;
    FControl := AControl;
  end;
end;

procedure TControlCanvas.UpdateTextFlags;
begin
  if Control = nil then Exit;
  if Control.UseRightToLeftReading then
    TextFlags := TextFlags or ETO_RTLREADING
  else
    TextFlags := TextFlags and not ETO_RTLREADING;
end;

{ TSizeConstraints }

constructor TSizeConstraints.Create(Control: TControl);
begin
  inherited Create;
  FControl := Control;
end;

procedure TSizeConstraints.AssignTo(Dest: TPersistent);
begin
  if Dest is TSizeConstraints then
    with TSizeConstraints(Dest) do
    begin
      FMinHeight := Self.FMinHeight;
      FMaxHeight := Self.FMaxHeight;
      FMinWidth := Self.FMinWidth;
      FMaxWidth := Self.FMaxWidth;
      Change;
    end
  else inherited AssignTo(Dest);
end;

procedure TSizeConstraints.SetConstraints(Index: Integer;
  Value: TConstraintSize);
begin
  case Index of
    0:
      if Value <> FMaxHeight then
      begin
        FMaxHeight := Value;
        if (Value > 0) and (Value < FMinHeight) then
          FMinHeight := Value;
        Change;
      end;
    1:
      if Value <> FMaxWidth then
      begin
        FMaxWidth := Value;
        if (Value > 0) and (Value < FMinWidth) then
          FMinWidth := Value;
        Change;
      end;
    2:
      if Value <> FMinHeight then
      begin
        FMinHeight := Value;
        if (FMaxHeight > 0) and (Value > FMaxHeight) then
          FMaxHeight := Value;
        Change;
      end;
    3:
      if Value <> FMinWidth then
      begin
        FMinWidth := Value;
        if (FMaxWidth > 0) and (Value > FMaxWidth) then
          FMaxWidth := Value;
        Change;
      end;
  end;
end;

procedure TSizeConstraints.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ TControlActionLink }

procedure TControlActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TControl;
end;

function TControlActionLink.DoShowHint(var HintStr: string): Boolean;
begin
  Result := True;
  if Action is TCustomAction then
  begin
    if TCustomAction(Action).DoHint(HintStr) and Application.HintShortCuts and
      (TCustomAction(Action).ShortCut <> scNone) then
    begin
      if HintStr <> '' then
        HintStr := Format('%s (%s)', [HintStr, ShortCutToText(TCustomAction(Action).ShortCut)]);
    end;
  end;
end;

function TControlActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
    (FClient.Caption = TCustomAction(Action).Caption);
end;

function TControlActionLink.IsDropdownMenuLinked: Boolean;
begin
  Result := Action is TCustomControlAction;
end;

function TControlActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and
    (FClient.Enabled = TCustomAction(Action).Enabled);
end;

function TControlActionLink.IsEnableDropdownLinked: Boolean;
begin
  Result := Action is TCustomControlAction;
end;

function TControlActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked and
    (FClient.Hint = TCustomAction(Action).Hint);
end;

function TControlActionLink.IsPopupMenuLinked: Boolean;
begin
  Result := (Action is TCustomControlAction) and
    (FClient.PopupMenu = TCustomControlAction(Action).PopupMenu);
end;

function TControlActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked and
    (FClient.Visible = TCustomAction(Action).Visible);
end;

function TControlActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked and ((
{$IF DEFINED(CLR)}

    (not Assigned(FClient.OnClick)) and (not Assigned(Action.OnExecute))) or
     (Assigned(FClient.OnClick) and
{$IFEND}
      DelegatesEqual(@FClient.OnClick, @Action.OnExecute)));
end;

procedure TControlActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then FClient.Caption := Value;
end;

procedure TControlActionLink.SetDropdownMenu(Value: TPopupMenu);
begin
end;

procedure TControlActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then FClient.Enabled := Value;
end;

procedure TControlActionLink.SetEnableDropdown(Value: Boolean);
begin
end;

procedure TControlActionLink.SetHint(const Value: string);
begin
  if IsHintLinked then FClient.Hint := Value;
end;

procedure TControlActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then FClient.Visible := Value;
end;

procedure TControlActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then FClient.OnClick := Value;
end;

function TControlActionLink.IsHelpLinked: Boolean;
begin
  Result := inherited IsHelpLinked and
    (FClient.HelpContext = TCustomAction(Action).HelpContext) and
    (FClient.HelpKeyword = TCustomAction(Action).HelpKeyword) and
    (FClient.HelpType = TCustomAction(Action).HelpType);
end;

procedure TControlActionLink.SetHelpKeyword(const Value: String);
begin
  if IsHelpLinked then FClient.HelpKeyword := Value;
end;

procedure TControlActionLink.SetHelpContext(Value: THelpContext);
begin
  if IsHelpLinked then FClient.HelpContext := Value;
end;

procedure TControlActionLink.SetHelpType(Value: THelpType);
begin
  if IsHelpLinked then FClient.HelpType := Value;
end;

procedure TControlActionLink.SetPopupMenu(Value: TPopupMenu);
begin
  if IsPopupMenuLinked then FClient.PopupMenu := Value;
end;

{ TControl }

constructor TControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWindowProc := WndProc;
  FControlStyle := [csCaptureMouse, csClickEvents, csSetCaption, csDoubleClicks, csGestures];
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FAnchors := [akLeft, akTop];
  FConstraints := TSizeConstraints.Create(Self);
  FConstraints.OnChange := DoConstraintsChange;
  FMargins := TMargins.Create(Self);
  FMargins.OnChange := DoMarginChange;
  FColor := clWindow;
  FVisible := True;
  FEnabled := True;
  FParentFont := True;
  FParentColor := True;
  FParentShowHint := True;
  FParentBiDiMode := True;
  FIsControl := False;
  FDragCursor := crDrag;
  FFloatingDockSiteClass := TCustomDockForm;
  FHelpType := htContext;
{$IFNDEF CLR}
  FText := nil;
{$ENDIF}
  FCustomHint := nil;
  FParentCustomHint := True;
  CreateTouchManager;
  FTouchManager.FInteractiveGestures := [igPressAndTap];
  FTouchManager.FInteractiveGestureOptions := [igoParentPassthrough];
  FTouchManager.FParentTabletOptions := True;
  FTouchManager.FTabletOptions := [toPressAndHold];
end;

destructor TControl.Destroy;
begin
  Application.ControlDestroyed(Self);
  if (FHostDockSite <> nil) then
  begin
    FHostDockSite.RemoveFreeNotification(Self);
{$IF DEFINED(CLR)}
    FHostDockSite.UndockClient(nil, Self);
{$ELSE}
    FHostDockSite.Perform(CM_UNDOCKCLIENT, 0, Winapi.Windows.LPARAM(Self));
{$IFEND}
    SetParent(nil);
    Dock(NullDockSite, BoundsRect);
    FHostDockSite := nil;
  end else
    SetParent(nil);
  FreeAndNil(FActionLink);
  FreeAndNil(FConstraints);
  FreeAndNil(FFont);
{$IF NOT DEFINED(CLR)}
  StrDispose(FText);
{$IFEND}
  FreeAndNil(FMargins);
  FCustomHint := nil;
  FreeAndNil(FTouchManager);
  inherited Destroy;
end;

function TControl.GetDragImages: TDragImageList;
begin
  Result := nil;
end;

function TControl.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TControl.GetPalette: HPALETTE;
begin
  Result := 0;
end;

function TControl.HasParent: Boolean;
begin
  Result := FParent <> nil;
end;

[UIPermission(SecurityAction.InheritanceDemand, Window=UIPermissionWindow.AllWindows),
UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
function TControl.GetParentComponent: TComponent;
begin
  Result := Parent;
end;

[UIPermission(SecurityAction.InheritanceDemand, Window=UIPermissionWindow.AllWindows),
UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TControl.SetParentComponent(Value: TComponent);
begin
  if (Parent <> Value) and (Value is TWinControl) then
    SetParent(TWinControl(Value));
end;

function TControl.PaletteChanged(Foreground: Boolean): Boolean;
var
  OldPalette, Palette: HPALETTE;
  WindowHandle: HWND;
  DC: HDC;
begin
  Result := False;
  if not Visible then Exit;
  Palette := GetPalette;
  if Palette <> 0 then
  begin
    DC := GetDeviceContext(WindowHandle);
    OldPalette := SelectPalette(DC, Palette, not Foreground);
    if RealizePalette(DC) <> 0 then Invalidate;
    SelectPalette(DC, OldPalette, True);
    ReleaseDC(WindowHandle, DC);
    Result := True;
  end;
end;

function TControl.GetAction: TBasicAction;
begin
  if ActionLink <> nil then
    Result := ActionLink.Action else
    Result := nil;
end;


procedure TControl.SetAnchors(Value: TAnchors);
var
  OldAnchors: TAnchors;
begin
  if FAnchors <> Value then
  begin
    OldAnchors := FAnchors;
    FAnchors := Value;
    if not (csLoading in ComponentState) then
      if (OldAnchors <> [akLeft, akTop]) and (FAnchors = [akLeft, akTop]) and
        ((FExplicitLeft <> Left) or (FExplicitTop <> Top) or
         (FExplicitWidth <> Width) or(FExplicitHeight <> Height)) then
        SetBounds(FExplicitLeft, FExplicitTop, FExplicitWidth, FExplicitHeight)
      else
        UpdateAnchorRules;
  end;
end;

procedure TControl.SetAction(Value: TBasicAction);
begin
  if Value = nil then
  begin
    ActionLink.Free;
    ActionLink := nil;
    Exclude(FControlStyle, csActionClient);
  end
  else
  begin
    Include(FControlStyle, csActionClient);
    if ActionLink = nil then
      ActionLink := GetActionLinkClass.Create(Self);
    ActionLink.Action := Value;
    ActionLink.OnChange := DoActionChange;
    ActionChange(Value, csLoading in Value.ComponentState);
    Value.FreeNotification(Self);
  end;
end;

function TControl.IsAnchorsStored: Boolean;
begin
  Result := Anchors <> AnchorAlign[Align];
end;

function TControl.IsTouchPropertyStored(AProperty: TTouchProperty): Boolean;
begin
  Result := True;
  case AProperty of
    tpInteractiveGestures: Result := FTouchManager.InteractiveGestures <> [igPressAndTap];
    tpInteractiveGestureOptions: Result := FTouchManager.InteractiveGestureOptions <> [igoParentPassthrough];
    tpParentTabletOptions: Result := not FTouchManager.ParentTabletOptions;
    tpTabletOptions: Result := not FTouchManager.ParentTabletOptions;
  end;
end;

procedure TControl.SetDesignVisible(Value: Boolean);
begin
  if (csDesigning in ComponentState) and (Value <> not (csDesignerHide in ControlState)) then
  begin
    if not Value then
      Include(FControlState, csDesignerHide)
    else
      Exclude(FControlState, csDesignerHide);
    InvalidateControl(True, False);
  end;
end;

procedure TControl.SetDragMode(Value: TDragMode);
begin
  FDragMode := Value;
end;

procedure TControl.RequestAlign;
begin
  if Parent <> nil then
    Parent.AlignControl(Self);
end;

function TControl.GetDragMode: TDragMode;
begin
  Result := FDragMode;
end;

{$IF DEFINED(CLR)}
procedure TControl.HookDelegate(EventName: string; Handler: MulticastDelegate);
var
  EI: EventInfo;
  Params: array of TObject;
begin
  EI := TypeOf(self).GetEvent(EventName);
  if EI <> nil then
  begin
    SetLength(Params, 1);
    Params[0] := Handler;
    EI.GetAddMethod(True).Invoke(self, Params);
  end;
end;

procedure TControl.UnhookDelegate(EventName: string; Handler: MulticastDelegate);
var
  EI: EventInfo;
  Params: array of TObject;
begin
  EI := TypeOf(self).GetEvent(EventName);
  if EI <> nil then
  begin
    SetLength(Params, 1);
    Params[0] := Handler;
    EI.GetRemoveMethod(True).Invoke(self, Params);
  end;
end;
{$IFEND}

procedure TControl.Resize;
begin
  if Assigned(FOnResize) then FOnResize(Self);
end;

procedure TControl.ReadState(Reader: TReader);
begin
  Include(FControlState, csReadingState);
  if Reader.Parent is TWinControl then Parent := TWinControl(Reader.Parent);
  inherited ReadState(Reader);
  Exclude(FControlState, csReadingState);
  if Parent <> nil then
  begin
    Perform(CM_PARENTCOLORCHANGED, 0, 0);
    Perform(CM_PARENTFONTCHANGED, 0, 0);
    Perform(CM_PARENTSHOWHINTCHANGED, 0, 0);
    Perform(CM_SYSFONTCHANGED, 0, 0);
    Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
    Perform(CM_PARENTDOUBLEBUFFEREDCHANGED, 0, 0);
    Perform(CM_PARENTTABLETOPTIONSCHANGED, 0, 0);
  end;
end;

procedure TControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = PopupMenu then
      PopupMenu := nil
    else if AComponent = Action then
      Action := nil
    else if AComponent = FHostDockSite then
      FHostDockSite := nil
    else if AComponent = FCustomHint then
      FCustomHint := nil
    else if (FTouchManager <> nil) and (AComponent = FTouchManager.GestureManager) then
      FTouchManager.FGestureManager := nil;
  end;
end;

procedure TControl.SetAlign(Value: TAlign);
var
  OldAlign: TAlign;
begin
  if FAlign <> Value then
  begin
    OldAlign := FAlign;
    FAlign := Value;
    Anchors := AnchorAlign[Value];
    if not (csLoading in ComponentState) and (not (csDesigning in ComponentState) or
      (Parent <> nil)) and (Value <> alCustom) and (OldAlign <> alCustom) then
      if ((OldAlign in [alTop, alBottom]) = (Value in [alRight, alLeft])) and
        not (OldAlign in [alNone, alClient]) and not (Value in [alNone, alClient]) then
        SetBounds(Left, Top, Height, Width)
      else if (OldAlign <> alNone) and (Value = alNone) then
        SetBounds(FExplicitLeft, FExplicitTop, FExplicitWidth, FExplicitHeight)
      else
        AdjustSize;
  end;
  RequestAlign;
end;

procedure TControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if CheckNewSize(AWidth, AHeight) and
    ((ALeft <> FLeft) or (ATop <> FTop) or
    (AWidth <> FWidth) or (AHeight <> FHeight)) then
  begin
    InvalidateControl(Visible, False);
    FLeft := ALeft;
    FTop := ATop;
    FWidth := AWidth;
    FHeight := AHeight;
    UpdateAnchorRules;
    UpdateExplicitBounds;
    Invalidate;
    Perform(WM_WINDOWPOSCHANGED, 0, 0);
    RequestAlign;
    if not (csLoading in ComponentState) then Resize;
  end;
end;

{$IF DEFINED(CLR)}
procedure TControl.SetTextBuf(Buffer: string);
{$ELSE}
procedure TControl.SetTextBuf(Buffer: PChar);
{$IFEND}
begin
  Perform(WM_SETTEXT, 0, Buffer);
  Perform(CM_TEXTCHANGED, 0, 0);
end;

procedure TControl.UpdateAnchorRules;
var
  Anchors: TAnchors;
begin
  if not FAnchorMove and not (csLoading in ComponentState) then
  begin
    Anchors := FAnchors;
    FAnchorOrigin := Point(
      Margins.ControlLeft + Margins.ControlWidth div 2,
      Margins.ControlTop + Margins.ControlHeight div 2);
    if Anchors = [akLeft, akTop] then
    begin
      FOriginalParentSize.X := 0;
      FOriginalParentSize.Y := 0;
      Exit;
    end;
    if akRight in Anchors then
      if akLeft in Anchors then
        FAnchorRules.X := Margins.ControlWidth else
        FAnchorRules.X := Margins.ControlLeft
    else
      FAnchorRules.X := Margins.ControlLeft + Margins.ControlWidth div 2;
    if akBottom in Anchors then
      if akTop in Anchors then
        FAnchorRules.Y := Margins.ControlHeight else
        FAnchorRules.Y := Margins.ControlTop
    else
      FAnchorRules.Y := Margins.ControlTop + Margins.ControlHeight div 2;
    if Parent <> nil then
      Parent.UpdateControlOriginalParentSize(Self, FOriginalParentSize);
  end;
end;

procedure TControl.SetLeft(Value: Integer);
begin
  SetBounds(Value, FTop, FWidth, FHeight);
  Include(FScalingFlags, sfLeft);
  if csReading in ComponentState then
    FExplicitLeft := FLeft;
end;

procedure TControl.SetTop(Value: Integer);
begin
  SetBounds(FLeft, Value, FWidth, FHeight);
  Include(FScalingFlags, sfTop);
  if csReading in ComponentState then
    FExplicitTop := FTop;
end;

procedure TControl.SetWidth(Value: Integer);
begin
  SetBounds(FLeft, FTop, Value, FHeight);
  Include(FScalingFlags, sfWidth);
  if csReading in ComponentState then
    FExplicitWidth := FWidth;
end;

procedure TControl.SetHeight(Value: Integer);
begin
  SetBounds(FLeft, FTop, FWidth, Value);
  Include(FScalingFlags, sfHeight);
  if csReading in ComponentState then
    FExplicitHeight := FHeight;
end;

procedure TControl.Dock(NewDockSite: TWinControl; ARect: TRect);
var
  PrevDockSite: TWinControl;
begin
  if HostDockSite <> NewDockSite then
  begin
    if (FHostDockSite <> nil) and (FHostDockSite.FDockClients <> nil) then
    begin
      FHostDockSite.FDockClients.Remove(Self);
      FHostDockSite.RemoveFreeNotification(Self);
    end;
    if (NewDockSite <> nil) and (NewDockSite <> NullDockSite) and
      (NewDockSite.FDockClients <> nil) then
      NewDockSite.FDockClients.Add(Self);
  end;
  Include(FControlState, csDocking);
  try
    if NewDockSite <> NullDockSite then
      DoDock(NewDockSite, ARect);
    if FHostDockSite <> NewDockSite then
    begin
      PrevDockSite := FHostDockSite;
      if NewDockSite <> NullDockSite then
      begin
        FHostDockSite := NewDockSite;
        if NewDockSite <> nil then
        begin
          NewDockSite.DoAddDockClient(Self, ARect);
          FHostDockSite.FreeNotification(Self);
        end;
      end
      else
        FHostDockSite := nil;
      if PrevDockSite <> nil then PrevDockSite.DoRemoveDockClient(Self);
    end;
  finally
    Exclude(FControlState, csDocking);
  end;
end;

procedure TControl.DoDock(NewDockSite: TWinControl; var ARect: TRect);
begin
  { Erase TControls before UpdateboundsRect modifies position }
  if not (Self is TWinControl) then InvalidateControl(Visible, False);
  if Parent <> NewDockSite then
    UpdateBoundsRect(ARect) else
    BoundsRect := ARect;
  if (NewDockSite = nil) or (NewDockSite = NullDockSite) then Parent := nil;
end;

procedure TControl.SetHelpContext(const Value: THelpContext);
begin
  if not (csLoading in ComponentState) then FHelpType := htContext;
  FHelpContext := Value;
end;

procedure TControl.SetHelpKeyword(const Value: String);
begin
  if not (csLoading in ComponentState) then FHelpType := htKeyword;
  FHelpKeyword := Value;
end;

procedure TControl.SetHostDockSite(Value: TWinControl);
begin
  Dock(Value, BoundsRect);
end;

function TControl.GetBoundsRect: TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
end;

procedure TControl.SetBoundsRect(const Rect: TRect);
begin
  with Rect do SetBounds(Left, Top, Right - Left, Bottom - Top);
end;

function TControl.GetClientRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width;
  Result.Bottom := Height;
end;

function TControl.GetClientWidth: Integer;
begin
  Result := ClientRect.Right;
end;

procedure TControl.SetClientWidth(Value: Integer);
begin
  SetClientSize(Point(Value, ClientHeight));
end;

function TControl.GetClientHeight: Integer;
begin
  Result := ClientRect.Bottom;
end;

procedure TControl.SetClientHeight(Value: Integer);
begin
  SetClientSize(Point(ClientWidth, Value));
end;

function TControl.GetClientOrigin: TPoint;
begin
  if Parent = nil then
    raise EInvalidOperation.CreateFmt(SParentRequired, [Name]);
  Result := Parent.ClientOrigin;
  Inc(Result.X, FLeft);
  Inc(Result.Y, FTop);
end;

function TControl.ClientToScreen(const Point: TPoint): TPoint;
var
  Origin: TPoint;
begin
  Origin := ClientOrigin;
  Result.X := Point.X + Origin.X;
  Result.Y := Point.Y + Origin.Y;
end;

procedure TControl.ScaleConstraints(M, D: Integer);
begin
  with Constraints do
  begin
    if MinWidth > 0 then
      MinWidth := MulDiv(MinWidth, M, D);
    if MaxWidth > 0 then
      MaxWidth := MulDiv(MaxWidth, M, D);
    if MinHeight > 0 then
      MinHeight := MulDiv(MinHeight, M, D);
    if MaxHeight > 0 then
      MaxHeight := MulDiv(MaxHeight, M, D);
  end;
end;

procedure TControl.ScaleMargins(M, D: Integer);
begin
  with Margins do
  begin
    if Left > 0 then
      Left := MulDiv(Left, M, D);
    if Top > 0 then
      Top := MulDiv(Top, M, D);
    if Right > 0 then
      Right := MulDiv(Right, M, D);
    if Bottom > 0 then
      Bottom := MulDiv(Bottom, M, D);
  end;
end;

function TControl.ScreenToClient(const Point: TPoint): TPoint;
var
  Origin: TPoint;
begin
  Origin := ClientOrigin;
  Result.X := Point.X - Origin.X;
  Result.Y := Point.Y - Origin.Y;
end;

procedure TControl.SendCancelMode(Sender: TControl);
var
  Control: TControl;
begin
  Control := Self;
  while Control <> nil do
  begin
    if Control is TCustomForm then
      TCustomForm(Control).SendCancelMode(Sender);
    Control := Control.Parent;
  end;
end;

procedure TControl.SendDockNotification(Msg: Cardinal; WParam, LParam: THandle);
var
  NotifyRec: TDockNotifyRec;
{$IF DEFINED(CLR)}
  DockMsg: TCMDockNotification;
{$IFEND}
begin
  if (FHostDockSite <> nil) and (DragObject = nil) and
    (ComponentState * [csLoading, csDestroying] = []) then
  begin
    with NotifyRec do
    begin
      ClientMsg := Msg;
      MsgWParam := WParam;
      MsgLParam := LParam;
    end;
{$IF DEFINED(CLR)}
    DockMsg := TCMDockNotification.Create;
    DockMsg.Client := Self;
    DockMsg.NotifyRec := NotifyRec;
    FHostDockSite.Perform(CM_DOCKNOTIFICATION, DockMsg.OriginalMessage.WParam, DockMsg.OriginalMessage.LParam);
{$ELSE}
    FHostDockSite.Perform(CM_DOCKNOTIFICATION, Winapi.Windows.WPARAM(Self), Winapi.Windows.LPARAM(@NotifyRec));
{$IFEND}
  end;
end;

procedure TControl.Changed;
begin
{$IF DEFINED(CLR)}
  Perform(CM_CHANGED, 0, GetHashCode);
{$ELSE}
  Perform(CM_CHANGED, 0, Winapi.Windows.LPARAM(Self));
{$IFEND}
end;

procedure TControl.ChangeScale(M, D: Integer);
var
  X, Y, W, H: Integer;
  Flags: TScalingFlags;
begin
  if M <> D then
  begin
    if csLoading in ComponentState then
      Flags := ScalingFlags else
      Flags := [sfLeft, sfTop, sfWidth, sfHeight, sfFont];
    if sfLeft in Flags then
      X := MulDiv(FLeft, M, D) else
      X := FLeft;
    if sfTop in Flags then
      Y := MulDiv(FTop, M, D) else
      Y := FTop;
    if (sfWidth in Flags) and not (csFixedWidth in ControlStyle) then
      if sfLeft in Flags then
        W := MulDiv(FLeft + FWidth, M, D) - X else
        W := MulDiv(FWidth, M, D)
    else W := FWidth;
    if (sfHeight in Flags) and not (csFixedHeight in ControlStyle) then
      if sfTop in Flags then
        H := MulDiv(FTop + FHeight, M, D) - Y else
        H := MulDiv(FHeight, M, D)
    else H := FHeight;
    ScaleConstraints(M, D);
    ScaleMargins(M, D);
    SetBounds(X, Y, W, H);
    if [sfLeft, sfWidth] * Flags <> [] then
      FOriginalParentSize.X := MulDiv(FOriginalParentSize.X, M, D);
    if [sfTop, sfHeight] * Flags <> [] then
      FOriginalParentSize.Y := MulDiv(FOriginalParentSize.Y, M, D);
    if not ParentFont and (sfFont in Flags) then
      Font.Size := MulDiv(Font.Size, M, D);
  end;
  FScalingFlags := [];
end;

procedure TControl.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if Value then AdjustSize;
  end;
end;

procedure TControl.SetName(const Value: TComponentName);
var
  ChangeText: Boolean;
begin
  ChangeText := (csSetCaption in ControlStyle) and
    not (csLoading in ComponentState) and (Name = Text) and
    ((Owner = nil) or not (Owner is TControl) or
    not (csLoading in TControl(Owner).ComponentState));
  inherited SetName(Value);
  if ChangeText then Text := Value;
end;

procedure TControl.SetClientSize(Value: TPoint);
var
  Client: TRect;
begin
  Client := GetClientRect;
  SetBounds(FLeft, FTop, Width - Client.Right + Value.X, Height -
    Client.Bottom + Value.Y);
end;

{$IF DEFINED(CLR)}
[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
function TControl.get_Parent: TWinControl;
begin
  Result := FParent;
end;
{$IFEND}

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TControl.SetParent(AParent: TWinControl);
begin
  if FParent <> AParent then
  begin
    if AParent = Self then
      raise EInvalidOperation.CreateRes({$IFNDEF CLR}@{$ENDIF}SControlParentSetToSelf);
    if FParent <> nil then
      FParent.RemoveControl(Self);
    if AParent <> nil then
    begin
      AParent.InsertControl(Self);
      UpdateAnchorRules;
    end;
  end;
end;

procedure TControl.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    VisibleChanging;
    FVisible := Value;
    Perform(CM_VISIBLECHANGED, Ord(Value), 0);
    RequestAlign;
  end;
end;

procedure TControl.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Perform(CM_ENABLEDCHANGED, 0, 0);
  end;
end;

function TControl.GetTextLen: Integer;
begin
{$IF DEFINED(CLR)}
  Result := Length(FText);
{$ELSE}
  Result := Perform(WM_GETTEXTLENGTH, 0, 0);
{$IFEND}
end;

{$IF NOT DEFINED(CLR)}
function TControl.GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
begin
  Result := Perform(WM_GETTEXT, BufSize, Buffer);
end;
{$IFEND}

function TControl.GetUndockHeight: Integer;
begin
  if FUndockHeight > 0 then Result := FUndockHeight
  else Result := Height;
end;

function TControl.GetUndockWidth: Integer;
begin
  if FUndockWidth > 0 then Result := FUndockWidth
  else Result := Width;
end;

function TControl.GetTBDockHeight: Integer;
begin
  if FTBDockHeight > 0 then Result := FTBDockHeight
  else Result := UndockHeight;
end;

function TControl.GetLRDockWidth: Integer;
begin
  if FLRDockWidth > 0 then Result := FLRDockWidth
  else Result := UndockWidth;
end;

procedure TControl.SetPopupMenu(Value: TPopupMenu);
begin
  FPopupMenu := Value;
  if Value <> nil then
  begin
    Value.ParentBiDiModeChanged(Self);
    Value.FreeNotification(Self);
  end;
end;

{$IF DEFINED(CLR)}
function TControl.GetTextPiece(Size: Integer): TCaption;
begin
  Result := Copy(FText, 1, Size);
end;
{$IFEND}

function TControl.GetText: TCaption;
{$IF DEFINED(CLR)}
begin
  Result := GetTextPiece(GetTextLen);
end;
{$ELSE}
var
  Len: Integer;
begin
  Len := GetTextLen;
  SetString(Result, PChar(nil), Len);
  if Len <> 0 then
  begin
    Len := Len - GetTextBuf(PChar(Result), Len + 1);
    if Len > 0 then
      SetLength(Result, Length(Result) - Len);
  end;
end;
{$IFEND}

procedure TControl.SetText(const Value: TCaption);
begin
  if GetText <> Value then
{$IF DEFINED(CLR)}
  begin
    FText := Value;
    Perform(CM_TEXTCHANGED, 0, 0);
  end;
{$ELSE}
  SetTextBuf(PChar(Value));
{$IFEND}
end;

procedure TControl.SetBiDiMode(Value: TBiDiMode);
begin
  if FBiDiMode <> Value then
  begin
    FBiDiMode := Value;
    FParentBiDiMode := False;
    Perform(CM_BIDIMODECHANGED, 0, 0);
  end;
end;

procedure TControl.FontChanged(Sender: TObject);
begin
  FParentFont := False;
  FDesktopFont := False;
  if Font.Height <> FFontHeight then
  begin
    Include(FScalingFlags, sfFont);
    FFontHeight := Font.Height;
  end;
  Perform(CM_FONTCHANGED, 0, 0);
end;

{$IF DEFINED(CLR)}
function TControl.GetFont: TFont;
begin
  Result := FFont;
end;
{$IFEND}

procedure TControl.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

function TControl.IsFontStored: Boolean;
begin
  Result := not ParentFont and not DesktopFont;
end;

function TControl.IsShowHintStored: Boolean;
begin
  Result := not ParentShowHint;
end;

function TControl.IsBiDiModeStored: Boolean;
begin
  Result := not ParentBiDiMode;
end;

procedure TControl.SetParentFont(Value: Boolean);
begin
  if FParentFont <> Value then
  begin
    FParentFont := Value;
    if (FParent <> nil) and not (csReading in ComponentState) then
      Perform(CM_PARENTFONTCHANGED, 0, 0);
  end;
end;

procedure TControl.SetDesktopFont(Value: Boolean);
begin
  if FDesktopFont <> Value then
  begin
    FDesktopFont := Value;
    Perform(CM_SYSFONTCHANGED, 0, 0);
  end;
end;

procedure TControl.SetShowHint(Value: Boolean);
begin
  if FShowHint <> Value then
  begin
    FShowHint := Value;
    FParentShowHint := False;
    Perform(CM_SHOWHINTCHANGED, 0, 0);
  end;
end;

procedure TControl.SetParentShowHint(Value: Boolean);
begin
  if FParentShowHint <> Value then
  begin
    FParentShowHint := Value;
    if (FParent <> nil) and not (csReading in ComponentState) then
      Perform(CM_PARENTSHOWHINTCHANGED, 0, 0);
  end;
end;

procedure TControl.SetParentCustomHint(Value: Boolean);
begin
  if FParentCustomHint <> Value then
  begin
    FParentCustomHint := Value;
  end;
end;

procedure TControl.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FParentColor := False;
    if (csDesigning in ComponentState) and not (csReading in ComponentState) and (Self is TWinControl) then
      TWinControl(Self).ParentBackground := False;
    Perform(CM_COLORCHANGED, 0, 0);
  end;
end;

function TControl.IsColorStored: Boolean;
begin
  Result := not ParentColor;
end;

procedure TControl.SetParentColor(Value: Boolean);
begin
  if FParentColor <> Value then
  begin
    FParentColor := Value;
    if (FParent <> nil) and not (csReading in ComponentState) then
      Perform(CM_PARENTCOLORCHANGED, 0, 0);
  end;
end;

procedure TControl.SetParentBiDiMode(Value: Boolean);
begin
  if FParentBiDiMode <> Value then
  begin
    FParentBiDiMode := Value;
    if (FParent <> nil) and not (csReading in ComponentState) then
      Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  end;
end;

procedure TControl.SetCursor(Value: TCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    Perform(CM_CURSORCHANGED, 0, 0);
  end;
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
function TControl.GetMouseCapture: Boolean;
begin
  Result := GetCaptureControl = Self;
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TControl.SetMouseCapture(Value: Boolean);
begin
  if MouseCapture <> Value then
    if Value then SetCaptureControl(Self) else SetCaptureControl(nil);
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.SafeTopLevelWindows)]
procedure TControl.BringToFront;
begin
  SetZOrder(True);
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.SafeSubWindows)]
procedure TControl.SendToBack;
begin
  SetZOrder(False);
end;

procedure TControl.SetZOrderPosition(Position: Integer);
var
  I, Count: Integer;
  ParentForm: TCustomForm;
begin
  if FParent <> nil then
  begin
    I := FParent.FControls.IndexOf(Self);
    if I >= 0 then
    begin
      Count := FParent.FControls.Count;
      if Position < 0 then Position := 0;
      if Position >= Count then Position := Count - 1;
      if Position <> I then
      begin
        FParent.FControls.Delete(I);
        FParent.FControls.Insert(Position, Self);
        InvalidateControl(Visible, True);
        if not (csLoading in ComponentState) then
        begin
          ParentForm := ValidParentForm(Self);
          if csPalette in ParentForm.ControlState then
            TControl(ParentForm).PaletteChanged(True);
        end;
      end;
    end;
  end;
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TControl.SetZOrder(TopMost: Boolean);
begin
  if FParent <> nil then
    if TopMost then
      SetZOrderPosition(FParent.FControls.Count - 1) else
      SetZOrderPosition(0);
end;

procedure TControl.SetCustomHint(Value: TCustomHint);
begin
  FCustomHint := Value;
  if Value <> nil then
  begin
    Value.FreeNotification(Self);
  end;

  if ParentCustomHint then
  begin
    if FCustomHint <> CustomHint then
      FParentCustomHint := False;
  end;
end;

function TControl.GetDeviceContext(var WindowHandle: HWND): HDC;
begin
  if Parent = nil then
    raise EInvalidOperation.CreateFmt(SParentRequired, [Name]);
  Result := Parent.GetDeviceContext(WindowHandle);
  SetViewportOrgEx(Result, Left, Top, nil);
  IntersectClipRect(Result, 0, 0, Width, Height);
end;

{$IF DEFINED(CLR)}
function TControl.GetDeviceContext(var WindowHandle: THWndWrapper): HDC;
begin
  if Parent = nil then
    raise EInvalidOperation.CreateFmt(SParentRequired, [Name]);
  Result := Parent.GetDeviceContext(WindowHandle);
  SetViewportOrgEx(Result, Left, Top, nil);
  IntersectClipRect(Result, 0, 0, Width, Height);
end;
{$IFEND}

procedure TControl.InvalidateControl(IsVisible, IsOpaque: Boolean);
var
  Rect: TRect;

  function BackgroundClipped: Boolean;
  var
    R: TRect;
    List: TList;
    I: Integer;
    C: TControl;
  begin
    Result := True;
    List := FParent.FControls;
    I := List.IndexOf(Self);
    while I > 0 do
    begin
      Dec(I);
      C := TControl(List[I]);
      with C do
        if C.Visible and (csOpaque in ControlStyle) then
        begin
          IntersectRect(R, Rect, BoundsRect);
          if EqualRect(R, Rect) then Exit;
        end;
    end;
    Result := False;
  end;

begin
  if (IsVisible or ((csDesigning in ComponentState) and
    not (csDesignerHide in ControlState)) and
    not (csNoDesignVisible in ControlStyle)) and (Parent <> nil) and
    Parent.HandleAllocated then
  begin
    Rect := BoundsRect;
    InvalidateRect(Parent.Handle, Rect, not (IsOpaque or
      (csOpaque in Parent.ControlStyle) or BackgroundClipped));
  end;
end;

procedure TControl.Invalidate;
begin
  InvalidateControl(Visible, csOpaque in ControlStyle);
end;

function TControl.MouseActivate(Button: TMouseButton; Shift: TShiftState;
  X, Y, HitTest: Integer): TMouseActivate;
begin
  Result := maDefault;
  if Assigned(FOnMouseActivate) then
    FOnMouseActivate(Self, Button, Shift, X, Y, HitTest, Result);
end;

procedure TControl.MouseWheelHandler(var Message: TMessage);
var
  Form: TCustomForm;
  Capture: TControl;
begin
  Form := GetParentForm(Self);
  Capture := GetCaptureControl;
  if Assigned(Capture) and (Capture <> Form) and (Capture <> Self) and (Capture.Parent = nil) then
    Capture.WndProc(Message);
  if Message.Result = 0 then
  begin
    if (Form <> nil) and (Form <> Self) then
      Form.MouseWheelHandler(Message)
    else
      Message.Result := Perform(CM_MOUSEWHEEL, Message.WParam, Message.LParam);
  end;
end;

procedure TControl.Hide;
begin
  Visible := False;
end;

procedure TControl.Show;
begin
  if Parent <> nil then Parent.ShowControl(Self);
  if not (csDesigning in ComponentState) or
    (csNoDesignVisible in ControlStyle) then Visible := True;
end;

procedure TControl.Update;
begin
  if Parent <> nil then Parent.Update;
end;

procedure TControl.Refresh;
begin
  Repaint;
end;

procedure TControl.Repaint;
var
  DC: HDC;
begin
  if (Visible or (csDesigning in ComponentState) and
    not (csNoDesignVisible in ControlStyle)) and (Parent <> nil) and
    Parent.HandleAllocated then
    if csOpaque in ControlStyle then
    begin
      DC := GetDC(Parent.Handle);
      try
        IntersectClipRect(DC, Left, Top, Left + Width, Top + Height);
        Parent.PaintControls(DC, Self);
      finally
        ReleaseDC(Parent.Handle, DC);
      end;
    end else
    begin
      Invalidate;
      Update;
    end;
end;

function TControl.GetControlsAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

function TControl.IsRightToLeft: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode <> bdLeftToRight);
end;

function TControl.UseRightToLeftReading: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode <> bdLeftToRight);
end;

function TControl.UseRightToLeftAlignment: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode = bdRightToLeft);
end;

function TControl.UseRightToLeftScrollBar: Boolean;
begin
  Result := SysLocale.MiddleEast and
    (BiDiMode in [bdRightToLeft, bdRightToLeftNoAlign]);
end;

procedure TControl.BeginAutoDrag;
begin
  BeginDrag(Mouse.DragImmediate, Mouse.DragThreshold);
end;

var
  FlagControl: TControl;

procedure TControl.BeginDrag(Immediate: Boolean; Threshold: Integer);
var
  P: TPoint;
begin
  if Mouse.IsPanning then
    Mouse.PanningWindow := nil;
  if (Self is TCustomForm) and (FDragKind <> dkDock) then
    raise EInvalidOperation.CreateRes({$IFNDEF CLR}@{$ENDIF}SCannotDragForm);
  CalcDockSizes;
  if (DragControl = nil) or (DragControl = FlagControl) then
  begin
    DragControl := nil;
    if csLButtonDown in ControlState then
    begin
      GetCursorPos(P);
      Perform(WM_LBUTTONUP, 0, PointToLParam(ScreenToClient(P)));
    end;
    { Use default value when Threshold < 0 }
    if Threshold < 0 then
      Threshold := Mouse.DragThreshold;
    // prevent calling EndDrag within BeginDrag
    if (DragControl <> FlagControl) then
      DragInitControl(Self, Immediate, Threshold);
  end;
end;

procedure TControl.EndDrag(Drop: Boolean);
begin
  if Dragging then DragDone(Drop)
  // prevent calling EndDrag within BeginDrag
  else
    if DragControl = nil then
      DragControl := FlagControl;
end;

procedure TControl.DragCanceled;
begin
  // Does nothing by default
end;

function TControl.Dragging: Boolean;
begin
  Result := DragControl = Self;
end;

procedure TControl.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := False;
  if Assigned(FOnDragOver) then
  begin
    Accept := True;
    FOnDragOver(Self, Source, X, Y, State, Accept);
  end;
end;

procedure TControl.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(FOnDragDrop) then FOnDragDrop(Self, Source, X, Y);
end;

procedure TControl.DoStartDrag(var DragObject: TDragObject);
begin
  if Assigned(FOnStartDrag) then FOnStartDrag(Self, DragObject);
end;

procedure TControl.DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  // Override DoGesture to implement default behaviour
  Handled := False;
end;

procedure TControl.DoGetGestureOptions(var Gestures: TInteractiveGestures;
  var Options: TInteractiveGestureOptions);
begin
  Gestures := FTouchManager.InteractiveGestures;
  Options := FTouchManager.InteractiveGestureOptions;
end;

procedure TControl.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  if Assigned(FOnEndDrag) then FOnEndDrag(Self, Target, X, Y);
end;

procedure TControl.PositionDockRect(DragDockObject: TDragDockObject);
var
  NewWidth, NewHeight: Integer;
  TempX, TempY: Double;
begin
  with DragDockObject do
  begin
    if (DragTarget = nil) or (not TWinControl(DragTarget).UseDockManager) then
    begin
      NewWidth := Control.UndockWidth;
      NewHeight := Control.UndockHeight;
      // Drag position for dock rect is scaled relative to control's click point.
      TempX := DragPos.X - ((NewWidth) * FMouseDeltaX);
      TempY := DragPos.Y - ((NewHeight) * FMouseDeltaY);
      with FDockRect do
      begin
        Left := Round(TempX);
        Top := Round(TempY);
        Right := Left + NewWidth;
        Bottom := Top + NewHeight;
      end;
      { Allow DragDockObject final say on this new dock rect }
      AdjustDockRect(FDockRect);
    end
    else begin
      GetWindowRect(TWinControl(DragTarget).Handle, FDockRect);
      if TWinControl(DragTarget).UseDockManager and
        (TWinControl(DragTarget).DockManager <> nil) then
        TWinControl(DragTarget).DockManager.PositionDockRect(Control,
          DropOnControl, DropAlign, FDockRect);
    end;
  end;
end;

procedure TControl.DockTrackNoTarget(Source: TDragDockObject; X, Y: Integer);
begin
  PositionDockRect(Source);
end;

procedure TControl.DoEndDock(Target: TObject; X, Y: Integer);
begin
  if Assigned(FOnEndDock) then FOnEndDock(Self, Target, X, Y);
end;

procedure TControl.DoStartDock(var DragObject: TDragObject);
var
  LDragObject: TDragDockObject;
begin
  if Assigned(FOnStartDock) then
  begin
    LDragObject := TDragDockObject(DragObject);
    FOnStartDock(Self, LDragObject);
    DragObject := LDragObject;
  end;
end;

procedure TControl.DoMouseActivate(var Message: TCMMouseActivate);
begin
  with Message do
    Result := Ord(MouseActivate(MouseActivateRec.Button, MouseActivateRec.ShiftState,
      MouseActivateRec.MousePos.X, MouseActivateRec.MousePos.Y, MouseActivateRec.HitTest));
end;

function TControl.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  IsNeg: Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Shift, WheelDelta, MousePos, Result);
  if not Result then
  begin
    Inc(FWheelAccumulator, WheelDelta);
    while Abs(FWheelAccumulator) >= WHEEL_DELTA do
    begin
      IsNeg := FWheelAccumulator < 0;
      FWheelAccumulator := Abs(FWheelAccumulator) - WHEEL_DELTA;
      if IsNeg then
      begin
        if FWheelAccumulator <> 0 then FWheelAccumulator := -FWheelAccumulator;
        Result := DoMouseWheelDown(Shift, MousePos);
      end
      else
        Result := DoMouseWheelUp(Shift, MousePos);
    end;
  end;
end;

function TControl.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelDown) then
    FOnMouseWheelDown(Self, Shift, MousePos, Result);
end;

function TControl.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelUp) then
    FOnMouseWheelUp(Self, Shift, MousePos, Result);
end;

procedure TControl.DefaultDockImage(DragDockObject: TDragDockObject;
  Erase: Boolean);
var
  DesktopWindow: HWND;
  DC: HDC;
  OldBrush: HBrush;
  DrawRect: TRect;
  PenSize: Integer;
begin
  with DragDockObject do
  begin
    PenSize := FrameWidth;
    if Erase then DrawRect := FEraseDockRect
    else DrawRect := FDockRect;
  end;
  DesktopWindow := GetDesktopWindow;
  DC := GetDCEx(DesktopWindow, 0, DCX_CACHE or DCX_LOCKWINDOWUPDATE);
  try
    OldBrush := SelectObject(DC, DragDockObject.Brush.Handle);
    with DrawRect do
    begin
      PatBlt(DC, Left + PenSize, Top, Right - Left - PenSize, PenSize, PATINVERT);
      PatBlt(DC, Right - PenSize, Top + PenSize, PenSize, Bottom - Top - PenSize, PATINVERT);
      PatBlt(DC, Left, Bottom - PenSize, Right - Left - PenSize, PenSize, PATINVERT);
      PatBlt(DC, Left, Top, PenSize, Bottom - Top - PenSize, PATINVERT);
    end;
    SelectObject(DC, OldBrush);
  finally
    ReleaseDC(DesktopWindow, DC);
  end;
end;

procedure TControl.DrawDragDockImage(DragDockObject: TDragDockObject);
begin
  DefaultDockImage(DragDockObject, False);
end;

procedure TControl.EraseDragDockImage(DragDockObject: TDragDockObject);
begin
  DefaultDockImage(DragDockObject, True);
end;

procedure TControl.DoDragMsg(var DragMsg: TCMDrag);
var
  S: TObject;
  Accepts, IsDockOp: Boolean;
begin
  with DragMsg, DragRec{$IFNDEF CLR}^{$ENDIF} do
  begin
    S := Source;
    IsDockOp := S is TDragDockObject;
    if DragInternalObject and not IsDockOp then
      S := (S as TDragControlObject).Control;
    with ScreenToClient(Pos) do
      case DragMessage of
        dmDragEnter, dmDragLeave, dmDragMove:
          begin
            Accepts := True;
            if IsDockOp then
            begin
              TWinControl(Target).DockOver(TDragDockObject(S), X, Y,
                TDragState(DragMessage), Accepts)
            end
            else
              DragOver(S, X, Y, TDragState(DragMessage), Accepts);
            Result := Ord(Accepts);
          end;
        dmDragDrop:
          begin
            if IsDockOp then TWinControl(Target).DockDrop(TDragDockObject(S), X, Y)
            else DragDrop(S, X, Y);
          end;
      end;
  end;
end;

function TControl.ManualDock(NewDockSite: TWinControl; DropControl: TControl;
  ControlSide: TAlign): Boolean;
var
  R: TRect;
  DockObject: TDragDockObject;
  HostDockSiteHandle: THandle;
  LPoint: TPoint;
begin
  if (NewDockSite = nil) or (NewDockSite = NullDockSite) then
  begin
    if (HostDockSite <> nil) and HostDockSite.UseDockManager and
      (HostDockSite.DockManager <> nil) then
    begin
      HostDockSite.DockManager.GetControlBounds(Self, R);
      MapWindowPoints(HostDockSite.Handle, 0, R, 2);
    end
    else
    begin
      R.Left := Left;
      R.Top := Top;
      if Parent <> nil then
        R.TopLeft := Parent.ClientToScreen(R.TopLeft);
    end;
    R := Bounds(R.Left, R.Top, UndockWidth, UndockHeight);
    Result := ManualFloat(R);
  end
  else
  begin
    CalcDockSizes;
    Result := (HostDockSite = nil) or HostDockSite.DoUndock(NewDockSite, Self);
    if Result then
    begin
      DockObject := TDragDockObject.Create(Self);
      try
        if HostDockSite <> nil then
          HostDockSiteHandle := HostDockSite.Handle else
          HostDockSiteHandle := 0;
        R := BoundsRect;
        if HostDockSiteHandle <> 0 then
          MapWindowPoints(HostDockSiteHandle, 0, R, 2);
        with DockObject do
        begin
          FDragTarget := NewDockSite;
          FDropAlign := ControlSide;
          FDropOnControl := DropControl;
          DockRect := R;
        end;
        LPoint := R.TopLeft;
        MapWindowPoints(0, NewDockSite.Handle, LPoint, 1);

        // Confine LPoint to the range of SmallInt to avoid range check
        // errors when DockDrop converts X and Y to a TSmallPoint
        if (LPoint.X > High(SmallInt)) or (LPoint.X < Low(SmallInt)) then
          LPoint.X := SmallInt(LPoint.X shr 16);
        if (LPoint.Y > High(SmallInt)) or (LPoint.Y < Low(SmallInt)) then
          LPoint.Y := SmallInt(LPoint.Y shr 16);

        NewDockSite.DockDrop(DockObject, LPoint.X, LPoint.Y);
      finally
        DockObject.Free;
      end;
    end;
  end;
end;

function TControl.ManualFloat(ScreenPos: TRect): Boolean;
var
  FloatHost: TWinControl;
begin
  Result := (HostDockSite = nil) or HostDockSite.DoUndock(nil, Self);
  if Result then
  begin
    FloatHost := CreateFloatingDockSite(ScreenPos);
    if FloatHost <> nil then
      Dock(FloatHost, System.Types.Rect(0, 0, FloatHost.ClientWidth,
        FloatHost.ClientHeight))
    else
      Dock(FloatHost, ScreenPos);
  end;
end;

function TControl.ReplaceDockedControl(Control: TControl;
  NewDockSite: TWinControl; DropControl: TControl; ControlSide: TAlign): Boolean;
var
  OldDockSite: TWinControl;
begin
  Result := False;
  if (Control.HostDockSite = nil) or ((Control.HostDockSite.UseDockManager) and
    (Control.HostDockSite.DockManager <> nil)) then
  begin
    OldDockSite := Control.HostDockSite;
    if OldDockSite <> nil then
      OldDockSite.DockManager.SetReplacingControl(Control);
    try
      ManualDock(OldDockSite, nil, alTop);
    finally
      if OldDockSite <> nil then
        OldDockSite.DockManager.SetReplacingControl(nil);
    end;
    Result := Control.ManualDock(NewDockSite, DropControl, ControlSide);
  end
  else if (Control.HostDockSite <> nil) then
    { Give the HostDockSite a direct crack at replacing the control. This is
      used when the HostDockSite does not (or cannot) use a DockManager }
    Result := Control.HostDockSite.DockReplaceDockClient(Control, NewDockSite,
      DropControl, ControlSide, Self);
end;

procedure TControl.DoConstraintsChange(Sender: TObject);
begin
  AdjustSize;
end;

function TControl.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
end;

function TControl.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCanResize) then FOnCanResize(Self, NewWidth, NewHeight, Result);
end;

function TControl.DoCanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  W, H: Integer;
begin
  if Align <> alClient then
  begin
    W := NewWidth;
    H := NewHeight;
    Result := CanAutoSize(W, H);
    if Align in [alNone, alLeft, alRight] then
      NewWidth := W;
    if Align in [alNone, alTop, alBottom] then
      NewHeight := H;
  end
  else Result := True;
end;

function TControl.DoCanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := CanResize(NewWidth, NewHeight);
  if Result then DoConstrainedResize(NewWidth, NewHeight);
end;

procedure TControl.ConstrainedResize(var MinWidth, MinHeight, MaxWidth,
  MaxHeight: Integer);
begin
  if Assigned(FOnConstrainedResize) then FOnConstrainedResize(Self, MinWidth,
    MinHeight, MaxWidth, MaxHeight);
end;

function TControl.CalcCursorPos: TPoint;
begin
  GetCursorPos(Result);
  Result := ScreenToClient(Result);
end;

[SecurityPermission(SecurityAction.InheritanceDemand, UnmanagedCode=True)]
function TControl.DesignWndProc(var Message: TMessage): Boolean;
begin
  Result := (csDesignInteractive in ControlStyle) and ((Message.Msg = WM_RBUTTONDOWN) or
    (Message.Msg = WM_RBUTTONUP) or (Message.Msg = WM_MOUSEMOVE) or
    (Message.Msg = WM_RBUTTONDBLCLK));
end;

procedure TControl.DoConstrainedResize(var NewWidth, NewHeight: Integer);
var
  MinWidth, MinHeight, MaxWidth, MaxHeight: Integer;
begin
  if Constraints.MinWidth > 0 then
    MinWidth := Constraints.MinWidth
  else
    MinWidth := 0;
  if Constraints.MinHeight > 0 then
    MinHeight := Constraints.MinHeight
  else
    MinHeight := 0;
  if Constraints.MaxWidth > 0 then
    MaxWidth := Constraints.MaxWidth
  else
    MaxWidth := 0;
  if Constraints.MaxHeight > 0 then
    MaxHeight := Constraints.MaxHeight
  else
    MaxHeight := 0;
  { Allow override of constraints }
  ConstrainedResize(MinWidth, MinHeight, MaxWidth, MaxHeight);
  if (MaxWidth > 0) and (NewWidth > MaxWidth) then
    NewWidth := MaxWidth
  else if (MinWidth > 0) and (NewWidth < MinWidth) then
    NewWidth := MinWidth;
  if (MaxHeight > 0) and (NewHeight > MaxHeight) then
    NewHeight := MaxHeight
  else if (MinHeight > 0) and (NewHeight < MinHeight) then
    NewHeight := MinHeight;
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
function TControl.Perform(Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): LRESULT;
var
  Message: TMessage;
begin
{$IF DEFINED(CLR)}
  Message := TMessage.Create(Msg, WParam, LParam);
{$ELSE}
  Message.Msg := Msg;
  Message.WParam := WParam;
  Message.LParam := LParam;
  Message.Result := 0;
{$IFEND}
  if Self <> nil then
    WindowProc(Message);
  Result := Message.Result;
end;

{$IF DEFINED(CLR)}
[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
function TControl.Perform(Msg: Cardinal; AObjectMsg: TCMObjectMsg): LRESULT;
var
  LMessage: TMessage;
begin
  LMessage := TMessage.Create(Msg, AObjectMsg.OriginalMessage.WParam,
    AObjectMsg.OriginalMessage.LParam);
  LMessage.Result := 0;
  if Self <> nil then
    WindowProc(LMessage);
  AObjectMsg.OriginalMessage.WParam := LMessage.WParam;
  AObjectMsg.OriginalMessage.LParam := LMessage.LParam;
  Result := LMessage.Result;
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
function TControl.Perform(Msg: Cardinal; WParam: WPARAM; var LParam): LRESULT;
var
  Buffer: IntPtr;
begin
  Buffer := Marshal.AllocHGlobal(Marshal.SizeOf(TObject(LParam)));
  try
    Marshal.StructureToPtr(TObject(LParam), Buffer, False);
    Result := Perform(Msg, WParam, THandle(Buffer));
    LParam := Marshal.PtrToStructure(Buffer, TypeOf(LParam));
  finally
    Marshal.DestroyStructure(Buffer, TypeOf(LParam));
    Marshal.FreeHGlobal(Buffer);
  end;
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
function TControl.Perform(Msg: Cardinal; WParam: WPARAM; LParam: string): LRESULT;
var
  Buffer: IntPtr;
begin
  Buffer := Marshal.StringToHGlobalAuto(LParam);
  try
    Result := Perform(Msg, WParam, THandle(Buffer));
  finally
    Marshal.FreeHGlobal(Buffer);
  end;
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
function TControl.Perform(Msg: Cardinal; WParam: WPARAM; var LParam: string;
  BufLen: Integer; ResultIsLen: Boolean = False): LRESULT;
var
  Buffer: IntPtr;
begin
  Buffer := Marshal.AllocHGlobal(BufLen * Marshal.SystemDefaultCharSize);
  try
    Marshal.WriteInt16(Buffer, 0, BufLen);
    Result := Perform(Msg, WParam, THandle(Buffer));
    if ResultIsLen then
      LParam := Marshal.PtrToStringAuto(Buffer, Result)
    else
      LParam := Marshal.PtrToStringAuto(Buffer);
  finally
    Marshal.FreeHGlobal(Buffer);
  end;
end;
{$ELSE}
function TControl.Perform(Msg: Cardinal; WParam: WPARAM; LParam: PChar): LRESULT;
begin
  Result := Perform(Msg, WParam, Winapi.Windows.LPARAM(LParam));
end;

function TControl.Perform(Msg: Cardinal; WParam: WPARAM; var LParam: TRect): LRESULT;
begin
  Result := Perform(Msg, WParam, Winapi.Windows.LPARAM(@LParam));
end;
{$IFEND}

function TControl.GetCustomHint: TCustomHint;
begin
  Result := FCustomHint;
  if FParentCustomHint and (Result = nil) then
  begin
    if Parent <> nil then
      Result := Parent.CustomHint;
  end;
end;

procedure TControl.CalcDockSizes;
begin
  if Floating then
  begin
    UndockHeight := Height;
    UndockWidth := Width;
  end
  else if HostDockSite <> nil then
  begin
    if (DockOrientation = doVertical) or
      (HostDockSite.Align in [alTop, alBottom]) then
      TBDockHeight := Height
    else if (DockOrientation = doHorizontal) or
      (HostDockSite.Align in [alLeft, alRight]) then
      LRDockWidth := Width;
  end;
end;

procedure TControl.UpdateBoundsRect(const R: TRect);
begin
  FLeft := R.Left;
  FTop := R.Top;
  FWidth := R.Right - R.Left;
  FHeight := R.Bottom - R.Top;
  UpdateAnchorRules;
  UpdateExplicitBounds;
end;

procedure TControl.VisibleChanging;
begin
end;

[SecurityPermission(SecurityAction.InheritanceDemand, UnmanagedCode=True)]
procedure TControl.WndProc(var Message: TMessage);
var
  Form: TCustomForm;
  KeyState: TKeyboardState;
  WheelMsg: TCMMouseWheel;
  Panned: Boolean;
{$IF DEFINED(CLR)}
  LMsg: TMessage;
{$IFEND}
begin
  if (csDesigning in ComponentState) then
  begin
    Form := GetParentForm(Self, False);
    if (Form <> nil) and (Form.Designer <> nil) and
      Form.Designer.IsDesignMsg(Self, Message) then Exit
  end;
  if (Message.Msg >= WM_KEYFIRST) and (Message.Msg <= WM_KEYLAST) then
  begin
    Form := GetParentForm(Self);
    if (Form <> nil) and Form.WantChildKey(Self, Message) then Exit;
  end
  else if (Message.Msg >= WM_MOUSEFIRST) and (Message.Msg <= WM_MOUSELAST) then
  begin
    if not (csDoubleClicks in ControlStyle) then
      case Message.Msg of
        WM_LBUTTONDBLCLK, WM_RBUTTONDBLCLK, WM_MBUTTONDBLCLK:
          Dec(Message.Msg, WM_LBUTTONDBLCLK - WM_LBUTTONDOWN);
      end;
    case Message.Msg of
      WM_MOUSEMOVE: Application.HintMouseMessage(Self, Message);
      WM_MBUTTONDOWN:
      begin
        if (csPannable in ControlStyle) and
        (ControlState * [csDestroyingHandle, csPanning] = []) and
        not Mouse.IsDragging then
        begin
          Mouse.CreatePanningWindow;
          Panned := False;
          if Assigned(Mouse.PanningWindow) then
          begin
            if Self is TWinControl then
              Panned := Mouse.PanningWindow.StartPanning(TWinControl(Self).Handle, Self)
            else if Parent <> nil then
              Panned := Mouse.PanningWindow.StartPanning(Parent.Handle, Self)
            else
            begin
              Form := GetParentForm(Self, False);
              if Form <> nil then
                Panned := Mouse.PanningWindow.StartPanning(Form.Handle, Self);
            end;
          end;
          if Panned then
          begin
            Message.Result := 1;
            Application.HideHint;
          end
          else if Assigned(Mouse.PanningWindow) then
            Mouse.PanningWindow := nil;
        end;
      end;
      WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
        begin
          if FDragMode = dmAutomatic then
          begin
            BeginAutoDrag;
            Exit;
          end;
          Include(FControlState, csLButtonDown);
        end;
      WM_LBUTTONUP:
        Exclude(FControlState, csLButtonDown);
    else
      with Mouse do
        if WheelPresent and (RegWheelMessage <> 0) and
          (Integer(Message.Msg) = Integer(RegWheelMessage)) then
        begin
          GetKeyboardState(KeyState);
{$IF DEFINED(CLR)}
          WheelMsg := TCMMouseWheel.Create;
{$IFEND}
          with WheelMsg do
          begin
            Msg := Message.Msg;
            ShiftState := KeyboardStateToShiftState(KeyState);
            WheelDelta := Message.WParam;
            Pos := SmallPoint(LongWord(Message.LParam));
          end;
{$IF DEFINED(CLR)}
          LMsg := WheelMsg.OriginalMessage;
          MouseWheelHandler(LMsg);
{$ELSE}
          MouseWheelHandler(TMessage(WheelMsg));
{$IFEND}
          Exit;
        end;
    end;
  end
  else if Message.Msg = CM_VISIBLECHANGED then
    with Message do
      SendDockNotification(Msg, WParam, LParam);
  Dispatch(Message);
end;

{$IF DEFINED(CLR)}
procedure TControl.RestoreWndProc;
begin
  WindowProc := WndProc;
end;
{$IFEND}

[SecurityPermission(SecurityAction.InheritanceDemand, UnmanagedCode=True)]
procedure TControl.DefaultHandler(var Message);
{$IF DEFINED(CLR)}
var
  GetTextMsg: TWMGetText;
  SetTextMsg: TWMSetText;
  Msg: TMessage;
begin
  Msg := UnwrapMessage(TObject(Message));
  case Msg.Msg of
    WM_GETTEXT:
      begin
        GetTextMsg := TWMGetText.Create(Msg);
        GetTextMsg.Result := Min(Length(FText), GetTextMsg.TextMax - 1);
        GetTextMsg.Text := Copy(FText, 1, GetTextMsg.Result);
      end;
    WM_GETTEXTLENGTH:
      Msg.Result := Length(FText);
    WM_SETTEXT:
      begin
        SetTextMsg := TWMSetText.Create(Msg);
        FText := SetTextMsg.Text;
        SendDockNotification(WM_SETTEXT, Msg.WParam, Msg.LParam);
      end;
  end;
{$ELSE}
var
  P: PChar;
begin
  with TMessage(Message) do
    case Msg of
      WM_GETTEXT:
        begin
          if FText <> nil then P := FText else P := '';
          Result := StrLen(StrLCopy(PChar(LParam), P, WParam - 1));
        end;
      WM_GETTEXTLENGTH:
        if FText = nil then Result := 0 else Result := StrLen(FText);
      WM_SETTEXT:
        begin
          P := StrNew(PChar(LParam));
          StrDispose(FText);
          FText := P;
          SendDockNotification(Msg, WParam, LParam);
        end;
    end;
{$IFEND}
end;

procedure TControl.ReadIsControl(Reader: TReader);
begin
  FIsControl := Reader.ReadBoolean;
end;

procedure TControl.WriteIsControl(Writer: TWriter);
begin
  Writer.WriteBoolean(FIsControl);
end;

procedure TControl.DefineProperties(Filer: TFiler);
type
  TExplicitDimension = (edLeft, edTop, edWidth, edHeight);

  function DoWriteIsControl: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := TControl(Filer.Ancestor).IsControl <> IsControl else
      Result := IsControl;
  end;

  function DoWriteExplicit(Dim: TExplicitDimension): Boolean;
  begin
    case Dim of
      edLeft: Result := ((Filer.Ancestor <> nil) and (TControl(Filer.Ancestor).ExplicitLeft <> FExplicitLeft)) or
        ((Filer.Ancestor = nil) and ((Align <> alNone) or ((Anchors * [akLeft]) = [])) and (FExplicitLeft <> FLeft));
      edTop: Result := ((Filer.Ancestor <> nil) and (TControl(Filer.Ancestor).ExplicitTop <> FExplicitTop)) or
        ((Filer.Ancestor = nil) and ((Align <> alNone) or ((Anchors * [akTop]) = [])) and (FExplicitTop <> FTop));
      edWidth: Result := ((Filer.Ancestor <> nil) and (TControl(Filer.Ancestor).ExplicitWidth <> FExplicitWidth)) or
        ((Filer.Ancestor = nil) and ((Align <> alNone) or ((Anchors * [akLeft, akRight]) = [akLeft, akRight])) and (FExplicitWidth <> FWidth));
      edHeight: Result := ((Filer.Ancestor <> nil) and (TControl(Filer.Ancestor).ExplicitHeight <> FExplicitHeight)) or
        ((Filer.Ancestor = nil) and ((Align <> alNone) or ((Anchors * [akTop, akBottom]) = [akTop, akBottom])) and (FExplicitHeight <> FHeight));
    else
      Result := False;
    end;
  end;

begin
  { The call to inherited DefinedProperties is omitted since the Left and
    Top special properties are redefined with real properties }
  Filer.DefineProperty('IsControl', ReadIsControl, WriteIsControl, DoWriteIsControl);
  Filer.DefineProperty('ExplicitLeft', ReadExplicitLeft, WriteExplicitLeft, not (csReading in ComponentState) and DoWriteExplicit(edLeft));
  Filer.DefineProperty('ExplicitTop', ReadExplicitTop, WriteExplicitTop, not (csReading in ComponentState) and DoWriteExplicit(edTop));
  Filer.DefineProperty('ExplicitWidth', ReadExplicitWidth, WriteExplicitWidth, not (csReading in ComponentState) and DoWriteExplicit(edWidth));
  Filer.DefineProperty('ExplicitHeight', ReadExplicitHeight, WriteExplicitHeight, not (csReading in ComponentState) and DoWriteExplicit(edHeight));
end;

procedure TControl.Click;
begin
  { Call OnClick if assigned and not equal to associated action's OnExecute.
    If associated action's OnExecute assigned then call it, otherwise, call
    OnClick. }
  if Assigned(FOnClick) and (Action <> nil) and not DelegatesEqual(@FOnClick, @Action.OnExecute) then
    FOnClick(Self)
  else if not (csDesigning in ComponentState) and (ActionLink <> nil) then
    ActionLink.Execute(Self)
  else if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TControl.DblClick;
begin
  if Assigned(FOnDblClick) then FOnDblClick(Self);
end;

procedure TControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TControl.DoMouseDown(var Message: TWMMouse; Button: TMouseButton;
  Shift: TShiftState);
begin
  if not (csNoStdEvents in ControlStyle) then
    with Message do
      if (Width > 32768) or (Height > 32768) then
        with CalcCursorPos do
          MouseDown(Button, KeysToShiftState(Keys) + Shift + MouseOriginToShiftState, X, Y)
      else
        MouseDown(Button, KeysToShiftState(Keys) + Shift + MouseOriginToShiftState, XPos, YPos);
end;

procedure TControl.WMLButtonDown(var Message: TWMLButtonDown);
begin
  SendCancelMode(Self);
  inherited;
  if csCaptureMouse in ControlStyle then
    MouseCapture := True;
  if csClickEvents in ControlStyle then
    Include(FControlState, csClicked);
  DoMouseDown(Message, mbLeft, []);
end;

procedure TControl.WMNCLButtonDown(var Message: TWMNCLButtonDown);
begin
  SendCancelMode(Self);
  inherited;
end;

procedure TControl.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  SendCancelMode(Self);
  inherited;
  if csCaptureMouse in ControlStyle then MouseCapture := True;
  if csClickEvents in ControlStyle then DblClick;
  DoMouseDown(Message, mbLeft, [ssDouble]);
end;

function TControl.GetPopupMenu: TPopupMenu;
begin
  Result := FPopupMenu;
end;

function TControl.CheckNewSize(var NewWidth, NewHeight: Integer): Boolean;
var
  W, H, W2, H2: Integer;
begin
  Result := False;
  W := NewWidth;
  H := NewHeight;
  if DoCanResize(W, H) then
  begin
    W2 := W;
    H2 := H;
    Result := not AutoSize or (DoCanAutoSize(W2, H2) and (W2 = W) and (H2 = H)) or
      DoCanResize(W2, H2);
    if Result then
    begin
      NewWidth := W2;
      NewHeight := H2;
    end;
  end;
end;

procedure TControl.WMRButtonDown(var Message: TWMRButtonDown);
begin
  inherited;
  DoMouseDown(Message, mbRight, []);
end;

procedure TControl.WMRButtonDblClk(var Message: TWMRButtonDblClk);
begin
  inherited;
  DoMouseDown(Message, mbRight, [ssDouble]);
end;

procedure TControl.WMMButtonDown(var Message: TWMMButtonDown);
begin
  inherited;
  DoMouseDown(Message, mbMiddle, []);
end;

procedure TControl.WMMButtonDblClk(var Message: TWMMButtonDblClk);
begin
  inherited;
  DoMouseDown(Message, mbMiddle, [ssDouble]);
end;

procedure TControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then FOnMouseMove(Self, Shift, X, Y);
end;

procedure TControl.WMMouseMove(var Message: TWMMouseMove);
begin
  inherited;
  if not (csNoStdEvents in ControlStyle) then
    with Message do
      if (Width > 32768) or (Height > 32768) then
        with CalcCursorPos do
          MouseMove(KeysToShiftState(Keys) + MouseOriginToShiftState, X, Y)
      else
        MouseMove(KeysToShiftState(Keys) + MouseOriginToShiftState, XPos, YPos);
end;

procedure TControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TControl.DoMouseUp(var Message: TWMMouse; Button: TMouseButton);
begin
  if not (csNoStdEvents in ControlStyle) then
    with Message do MouseUp(Button, KeysToShiftState(Keys) + MouseOriginToShiftState, XPos, YPos);
end;

procedure TControl.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  if csCaptureMouse in ControlStyle then MouseCapture := False;
  if csClicked in ControlState then
  begin
    Exclude(FControlState, csClicked);
    if PtInRect(ClientRect, SmallPointToPoint(Message.Pos)) then
      Click;
  end;
  DoMouseUp(Message, mbLeft);
end;

procedure TControl.WMRButtonUp(var Message: TWMRButtonUp);
begin
  inherited;
  DoMouseUp(Message, mbRight);
end;

procedure TControl.WMMButtonUp(var Message: TWMMButtonUp);
begin
  inherited;
  DoMouseUp(Message, mbMiddle);
end;

procedure TControl.WMMouseWheel(var Message: TWMMouseWheel);
{$IF DEFINED(CLR)}
var
  LMsg: TMessage;
{$IFEND}
begin
  if not Mouse.WheelPresent then
  begin
    Mouse.FWheelPresent := True;
    Mouse.SettingChanged(SPI_GETWHEELSCROLLLINES);
  end;
{$IF DEFINED(CLR)}
  TCMMouseWheel.Create(Message.OriginalMessage).ShiftState := KeysToShiftState(Message.Keys);
  LMsg := Message.OriginalMessage;
  MouseWheelHandler(LMsg);
{$ELSE}
  TCMMouseWheel(Message).ShiftState := KeysToShiftState(Message.Keys);
  MouseWheelHandler(TMessage(Message));
{$IFEND}
  if Message.Result = 0 then inherited;
end;

procedure TControl.WMCancelMode(var Message: TWMCancelMode);
begin
  inherited;
  if MouseCapture then
  begin
    MouseCapture := False;
    if csLButtonDown in ControlState then
    begin

      Perform(WM_LBUTTONUP, 0, Winapi.Windows.LPARAM(Integer($FFFFFFFF)));
    end;
  end
  else
    Exclude(FControlState, csLButtonDown);
end;

procedure TControl.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  { Update min/max width/height to actual extents control will allow }
  if ComponentState * [csReading, csLoading] = [] then
  begin
    with Constraints do
    begin
      if (MaxWidth > 0) and (Width > MaxWidth) then
        FMaxWidth := Width
      else if (MinWidth > 0) and (Width < MinWidth) then
        FMinWidth := Width;
      if (MaxHeight > 0) and (Height > MaxHeight) then
        FMaxHeight := Height
      else if (MinHeight > 0) and (Height < MinHeight) then
        FMinHeight := Height;
    end;
{$IF DEFINED(CLR)}

    if not Message.WindowPosIsNil then
      with Message.WindowPos do
{$ELSE}
    if Message.WindowPos <> nil then
      with Message.WindowPos^ do
{$IFEND}
        if (FHostDockSite <> nil) and not (csDocking in ControlState)  and
          (Flags and SWP_NOSIZE = 0) and (cx <> 0) and (cy <> 0) then
          CalcDockSizes;
  end;
end;

procedure TControl.CMVisibleChanged(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) or
    (csNoDesignVisible in ControlStyle) then
    InvalidateControl(True, FVisible and (csOpaque in ControlStyle));
end;

procedure TControl.CMEnabledChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TControl.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TControl.CMColorChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TControl.CMParentColorChanged(var Message: TMessage);

  // Same as SetColor but doesn't set ParentBackground to False
  procedure SetParentColor(Value: TColor);
  begin
    if FColor <> Value then
    begin
      FColor := Value;
      FParentColor := False;
      Perform(CM_COLORCHANGED, 0, 0);
    end;
  end;

begin
  if FParentColor then
  begin
    if Message.wParam <> 0 then
      SetParentColor(TColor(Message.lParam)) else
      SetParentColor(FParent.FColor);
    FParentColor := True;
  end;
end;

procedure TControl.CMParentBiDiModeChanged(var Message: TMessage);
begin
  if FParentBiDiMode then
  begin
    if FParent <> nil then BiDiMode := FParent.BiDiMode;
    FParentBiDiMode := True;
  end;
end;

procedure TControl.CMMouseWheel(var Message: TCMMouseWheel);
begin
  with Message do
  begin
    Result := 0;
    if DoMouseWheel(ShiftState, WheelDelta, SmallPointToPoint(Pos)) then
      Message.Result := 1
    else if Parent <> nil then
{$IF DEFINED(CLR)}
      with UnwrapMessage(Message) do
{$ELSE}
      with TMessage(Message) do
{$IFEND}
        Result := Parent.Perform(CM_MOUSEWHEEL, WParam, LParam);
  end;
end;

procedure TControl.CMGesture(var Message: TCMGesture);
var
  Handled: Boolean;
begin
  Handled := False;
{$IF NOT DEFINED(CLR)}
  if Assigned(FOnGesture) then
    FOnGesture(Self, Message.Info^, Handled);
  if not Handled then
    DoGesture(Message.Info^, Handled);
  if Handled then
    Message.Result := 1
  else
    // Pass notification on to parent if it hasn't been handled
    if (Message.Info^.GestureID = sgiNoGesture) and (FParent <> nil) then
      FParent.Perform(CM_GESTURE, 0, LPARAM(Message.Info));
{$IFEND}
end;

procedure TControl.CMBiDiModeChanged(var Message: TMessage);
begin
  if (SysLocale.MiddleEast) and (Message.wParam = 0) then
    Invalidate;
end;

procedure TControl.CMParentShowHintChanged(var Message: TMessage);
begin
  if FParentShowHint and
     ((csDesigning in ComponentState) = (csDesigning in FParent.ComponentState)) then
  begin
    SetShowHint(FParent.FShowHint);
    FParentShowHint := True;
  end;
end;

procedure TControl.CMParentFontChanged(var Message: TCMParentFontChanged);
begin
  if FParentFont then
  begin
    if Message.WParam <> 0 then
      SetFont(Message.Font)
    else
      SetFont(FParent.FFont);
    FParentFont := True;
  end;
end;

procedure TControl.CMSysFontChanged(var Message: TMessage);
begin
  if FDesktopFont then
  begin
    SetFont(Screen.IconFont);
    FDesktopFont := True;
  end;
end;

procedure TControl.CMHitTest(var Message: TCMHitTest);
begin
  Message.Result := HTCLIENT;
end;

{$IF DEFINED(CLR)}
procedure TControl.DoMouseEnter;
begin
  FParent.Perform(CM_MOUSEENTER, meTControl, FParent.FControls.IndexOf(Self));
end;

procedure TControl.DoMouseLeave;
begin
  FParent.Perform(CM_MOUSELEAVE, meTControl, FParent.FControls.IndexOf(Self));
end;
{$IFEND}

procedure TControl.CMMouseEnter(var Message: TMessage);
begin
  if FParent <> nil then
{$IF DEFINED(CLR)}
    DoMouseEnter;
  if (Message.WParam = 0) then
{$ELSE}
    FParent.Perform(CM_MOUSEENTER, 0, Winapi.Windows.LPARAM(Self));
  if (Message.LParam = 0) then
{$IFEND}
  begin
    if Assigned(FOnMouseEnter) then
      FOnMouseEnter(Self);

    if ShowHint and not (csDesigning in ComponentState) then
      if CustomHint <> nil then
        CustomHint.ShowHint(Self);
  end;
end;

procedure TControl.CMMouseLeave(var Message: TMessage);
begin
 if FParent <> nil then
{$IF DEFINED(CLR)}
   DoMouseLeave;
  if (Message.WParam = 0) then
{$ELSE}
    FParent.Perform(CM_MOUSELEAVE, 0, Winapi.Windows.LPARAM(Self));
  if (Message.LParam = 0) then
{$IFEND}
  begin
    if Assigned(FOnMouseLeave) then
      FOnMouseLeave(Self);

    if ShowHint and not (csDesigning in ComponentState) then
      if CustomHint <> nil then
        CustomHint.HideHint(Self);
  end;
end;

procedure TControl.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  Message.Result := 0;
end;

procedure TControl.CMParentTabletOptionsChanged(var Message: TMessage);
begin
  if Touch.FParentTabletOptions then
  begin
    if FParent <> nil then
      Touch.TabletOptions := FParent.Touch.TabletOptions;
    Touch.FParentTabletOptions := True;
  end;
end;

function TControl.CreateFloatingDockSite(Bounds: TRect): TWinControl;
begin
  Result := nil;
  if (FloatingDockSiteClass <> nil) and
    (FloatingDockSiteClass <> TWinControlClass(ClassType)) then
  begin
    Result := FloatingDockSiteClass.Create(Application);
    with Bounds do
    begin
      Result.Top := Top;
      Result.Left := Left;
      Result.ClientWidth := Right - Left;
      Result.ClientHeight := Bottom - Top;
    end;
  end;
end;

procedure TControl.CreateTouchManager;
begin
  FTouchManager := TTouchManager.Create(Self);
end;

{$IF DEFINED(CLR)}
procedure TControl.FloatControl(DockSource: TDragDockObject);
{$ELSE}
procedure TControl.CMFloat(var Message: TCMFloat);
{$IFEND}
var
  FloatHost: TWinControl;

  procedure UpdateFloatingDockSitePos;
  var
    P: TPoint;
  begin
    P := Parent.ClientToScreen(Point(Left, Top));
{$IF DEFINED(CLR)}
    with DockSource.DockRect do
{$ELSE}
    with Message.DockSource.DockRect do
{$IFEND}
      Parent.BoundsRect := Bounds(Left + Parent.Left - P.X,
        Top + Parent.Top - P.Y,
        Right - Left + Parent.Width - Width,
        Bottom - Top + Parent.Height - Height);
  end;

begin
  if Floating and (Parent <> nil) then
    UpdateFloatingDockSitePos
  else
{$IF NOT DEFINED(CLR)}
  with Message do
{$IFEND}
  begin
    FloatHost := CreateFloatingDockSite(DockSource.DockRect);
    if FloatHost <> nil then
    begin
      DockSource.DragTarget := FloatHost;
      DockSource.DragHandle := FloatHost.Handle;
    end;
  end;
end;

procedure TControl.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.Caption = '') or (Self.Caption = Self.Name) then
        Self.Caption := Caption;
      if not CheckDefaults or (Self.Enabled = True) then
        Self.Enabled := Enabled;
      if not CheckDefaults or (Self.Hint = '') then
        Self.Hint := Hint;
      if not CheckDefaults or (Self.Visible = True) then
        Self.Visible := Visible;
      if not CheckDefaults or not Assigned(Self.OnClick) then
        Self.OnClick := OnExecute;
    end;
end;

procedure TControl.DoActionChange(Sender: TObject);
begin
  if Sender = Action then ActionChange(Sender, False);
end;

function TControl.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TControlActionLink;
end;

function TControl.IsCaptionStored: Boolean;
begin
  Result := (ActionLink = nil) or not ActionLink.IsCaptionLinked;
end;

function TControl.IsEnabledStored: Boolean;
begin
  Result := (ActionLink = nil) or not ActionLink.IsEnabledLinked;
end;

function TControl.IsHintStored: Boolean;
begin
  Result := (ActionLink = nil) or not ActionLink.IsHintLinked;
end;

function TControl.IsHelpContextStored: Boolean;
begin
  Result := (ActionLink = nil) or not ActionLink.IsHelpContextLinked;
end;

function TControl.IsVisibleStored: Boolean;
begin
  Result := (ActionLink = nil) or not ActionLink.IsVisibleLinked;
end;

function TControl.IsOnClickStored: Boolean;
begin
  Result := (ActionLink = nil) or not ActionLink.IsOnExecuteLinked;
end;

procedure TControl.Loaded;
begin
  inherited Loaded;
  if Action <> nil then ActionChange(Action, True);
  UpdateAnchorRules;
end;

procedure TControl.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomAction then
    with TCustomAction(Dest) do
    begin
      Enabled := Self.Enabled;
      Hint := Self.Hint;
      Caption := Self.Caption;
      Visible := Self.Visible;
      OnExecute := Self.OnClick;
    end
  else inherited AssignTo(Dest);
end;

function TControl.GetDockEdge(MousePos: TPoint): TAlign;

  function MinVar(const Data: array of Double): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := Low(Data) + 1 to High(Data) do
      if Data[I] < Data[Result] then Result := I;
  end;

var
  T, L, B, R: Integer;
begin
  Result := alNone;
  R := Width;
  B := Height;
  // if Point is outside control, then we can determine side quickly
  if MousePos.X <= 0 then Result := alLeft
  else if MousePos.X >= R then Result := alRight
  else if MousePos.Y <= 0 then Result := alTop
  else if MousePos.Y >= B then Result := alBottom
  else begin
    // if MousePos is inside the control, then we need to figure out which side
    // MousePos is closest to.
    T := MousePos.Y;
    B := B - MousePos.Y;
    L := MousePos.X;
    R := R - MousePos.X;
    case MinVar([L, R, T, B]) of
      0: Result := alLeft;
      1: Result := alRight;
      2: Result := alTop;
      3: Result := alBottom;
    end;
  end;
end;

function TControl.GetFloating: Boolean;
begin
  Result := (HostDockSite <> nil) and (HostDockSite is FloatingDockSiteClass);
end;

function TControl.GetFloatingDockSiteClass: TWinControlClass;
begin
  Result := FFloatingDockSiteClass;
end;

procedure TControl.AdjustSize;
begin
  if not (csLoading in ComponentState) then SetBounds(Left, Top, Width, Height);
end;

function TControl.DrawTextBiDiModeFlags(Flags: Longint): Longint;
begin
  Result := Flags;
  { do not change center alignment }
  if UseRightToLeftAlignment then
    if Result and DT_RIGHT = DT_RIGHT then
      Result := Result and not DT_RIGHT { removing DT_RIGHT, makes it DT_LEFT }
    else if not (Result and DT_CENTER = DT_CENTER) then
      Result := Result or DT_RIGHT;
  Result := Result or DrawTextBiDiModeFlagsReadingOnly;
end;

function TControl.DrawTextBiDiModeFlagsReadingOnly: Longint;
begin
  if UseRightToLeftReading then
    Result := DT_RTLREADING
  else
    Result := 0;
end;

procedure TControl.InitiateAction;
begin
  if ActionLink <> nil then ActionLink.Update;
end;

procedure TControl.CMHintShow(var Message: TCMHintShow);
var
{$IF DEFINED(CLR)}
  HintInfo: THintInfo;
{$ELSE}
  HintInfo: PHintInfo;
{$IFEND}
begin
  if (ActionLink <> nil) then
  begin
    HintInfo := Message.HintInfo;
    if not ActionLink.DoShowHint(HintInfo{$IFNDEF CLR}^{$ENDIF}.HintStr) then
      Message.Result := 1;
{$IF DEFINED(CLR)}
    Message.HintInfo := HintInfo;
{$IFEND}
  end;
end;

procedure TControl.WMContextMenu(var Message: TWMContextMenu);
var
  Pt, Temp: TPoint;
  Handled: Boolean;
  PopupMenu: TPopupMenu;
begin
  if Message.Result <> 0 then Exit;
  if csDesigning in ComponentState then
  begin
    inherited;
    Exit;
  end;

  Pt := SmallPointToPoint(Message.Pos);
  if InvalidPoint(Pt) then
    Temp := Pt
  else
  begin
    Temp := ScreenToClient(Pt);
    if not PtInRect(ClientRect, Temp) then
    begin
      inherited;
      Exit;
    end;
  end;

  Handled := False;
  DoContextPopup(Temp, Handled);
  Message.Result := Ord(Handled);
  if Handled then Exit;

  PopupMenu := GetPopupMenu;
  if (PopupMenu <> nil) and PopupMenu.AutoPopup then
  begin
    SendCancelMode(Self);
    PopupMenu.PopupComponent := Self;
    if InvalidPoint(Pt) then
      Pt := ClientToScreen(Point(0, 0));
    PopupMenu.Popup(Pt.X, Pt.Y);
    Message.Result := 1;
  end;

  if Message.Result = 0 then
    inherited;
end;

procedure TControl.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(FOnContextPopup) then FOnContextPopup(Self, MousePos, Handled);
end;

procedure TControl.SetConstraints(const Value: TSizeConstraints);
begin
  FConstraints.Assign(Value);
end;

function TControl.ClientToParent(const Point: TPoint; AParent: TWinControl): TPoint;
var
  LParent: TWinControl;
begin
  if AParent = nil then
    AParent := Parent;
  if AParent = nil then
    raise EInvalidOperation.CreateFmt(SParentRequired, [Name]);
  Result := Point;
  Inc(Result.X, Left);
  Inc(Result.Y, Top);
  LParent := Parent;
  while (LParent <> nil) and (LParent <> AParent) do
  begin
    if LParent.Parent <> nil then
    begin
      Inc(Result.X, LParent.Left);
      Inc(Result.Y, LParent.Top);
    end;
    LParent := LParent.Parent;
  end;
  if LParent = nil then
    raise EInvalidOperation.CreateFmt(SParentGivenNotAParent, [Name]);
end;

function TControl.ParentToClient(const Point: TPoint; AParent: TWinControl): TPoint;
var
  LParent: TWinControl;
begin
  if AParent = nil then
    AParent := Parent;
  if AParent = nil then
    raise EInvalidOperation.CreateFmt(SParentRequired, [Name]);
  Result := Point;
  Dec(Result.X, Left);
  Dec(Result.Y, Top);
  LParent := Parent;
  while (LParent <> nil) and (LParent <> AParent) do
  begin
    if LParent.Parent <> nil then
    begin
      Dec(Result.X, LParent.Left);
      Dec(Result.Y, LParent.Top);
    end;
    LParent := LParent.Parent;
  end;
  if LParent = nil then
    raise EInvalidOperation.CreateFmt(SParentGivenNotAParent, [Name]);
end;

procedure TControl.CMMouseActivate(var Message: TCMMouseActivate);
begin
  DoMouseActivate(Message);
  if Message.Result = 0 then
    DefaultHandler(Message);
end;

procedure TControl.DoMarginChange(Sender: TObject);
begin
  RequestAlign;
end;

procedure TControl.SetMargins(const Value: TMargins);
begin
  FMargins.Assign(Value);
end;

procedure TControl.SetTouchManager(const Value: TTouchManager);
begin
  FTouchManager.Assign(Value);
end;

function TControl.GetAlignWithMargins: Boolean;
begin
  Result := csAlignWithMargins in FControlStyle;
end;

procedure TControl.SetAlignWithMargins(Value: Boolean);
begin
  if Value <> GetAlignWithMargins then
  begin
    if Value then
      Include(FControlStyle, csAlignWithMargins)
    else
      Exclude(FControlStyle, csAlignWithMargins);
    RequestAlign;
  end;
end;

procedure TControl.UpdateExplicitBounds;
begin
  if not (csAligning in ControlState) and not (csReading in ComponentState) then
  begin
    FExplicitLeft := FLeft;
    FExplicitTop := FTop;
    FExplicitWidth := FWidth;
    FExplicitHeight := FHeight;
  end;
end;

procedure TControl.WriteExplicitTop(Writer: TWriter);
begin
  Writer.WriteInteger(FExplicitTop);
end;

procedure TControl.WriteExplicitHeight(Writer: TWriter);
begin
  Writer.WriteInteger(FExplicitHeight);
end;

procedure TControl.WriteExplicitLeft(Writer: TWriter);
begin
  Writer.WriteInteger(FExplicitLeft);
end;

procedure TControl.ReadExplicitWidth(Reader: TReader);
begin
  FExplicitWidth := Reader.ReadInteger;
end;

procedure TControl.WriteExplicitWidth(Writer: TWriter);
begin
  Writer.WriteInteger(FExplicitWidth);
end;

procedure TControl.ReadExplicitTop(Reader: TReader);
begin
  FExplicitTop := Reader.ReadInteger;
end;

procedure TControl.ReadExplicitHeight(Reader: TReader);
begin
  FExplicitHeight := Reader.ReadInteger;
end;

procedure TControl.ReadExplicitLeft(Reader: TReader);
begin
  FExplicitLeft := Reader.ReadInteger;
end;


{ TWinControlActionLink }

procedure TWinControlActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TWinControl;
end;

function TWinControlActionLink.IsHelpContextLinked: Boolean;
begin
  { maintained for backwards compatability}
  Result := IsHelpLinked;
end;

procedure TWinControlActionLink.SetHelpContext(Value: THelpContext);
begin
  inherited SetHelpContext(Value);
end;

  { THwndWrapper }

{$IF DEFINED(CLR)}
procedure THWndWrapper.RegisterFinalizeNotify(Proc: TFinalizeHWNDNotify);
begin
  if FNotifies = nil then
    FNotifies := TList.Create;
  FNotifies.Add(@Proc);
end;

procedure THWndWrapper.UnregisterFinalizeNotify(Proc: TFinalizeHWNDNotify);
begin
  if Assigned(FNotifies) then
    FNotifies.Remove(@Proc);
end;

procedure THWndWrapper.CallNotifies;
var
  I: Integer;
begin
  if Assigned(FNotifies) then
  begin
    for I := 0 to FNotifies.Count - 1 do
      TFinalizeHWNDNotify(FNotifies[I])(Handle);
    FreeAndNil(FNotifies);
  end;
end;

procedure THWndWrapper.Finalize;
begin
  if Handle <> 0 then
  begin
    CallNotifies;
    DestroyWindow(Handle);
    Handle := 0;
  end;
  if Assigned(FObjInstance) then
  begin
    FreeObjectInstance(@FObjInstance);
    FObjInstance := nil;
  end;
end;
{$IFEND}

{ TWinControl }

constructor TWinControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IF DEFINED(CLR)}
  if FPendingParentWindow <> 0 then
  begin
    FParentWindow := FPendingParentWindow;
    FPendingParentWindow := 0;
  end;
  FHandle := THWndWrapper.Create;
  FHandle.FObjInstance := MakeObjectInstance(MainWndProc);
{$ELSE}
  FObjectInstance := MakeObjectInstance(MainWndProc);
{$IFEND}
  FBrush := TBrush.Create;
  FBrush.Color := FColor;
  FDoubleBuffered := False;
  FParentDoubleBuffered := True;
  FParentCtl3D := True;
  FTabOrder := -1;
  FImeMode := imDontCare;
  if SysLocale.PriLangID = LANG_JAPANESE then
    FImeName := ''
  else
    FImeName := Screen.DefaultIme;
  FUseDockManager := False;
  FBevelEdges := [beLeft, beTop, beRight, beBottom];
  FBevelInner := bvRaised;
  FBevelOuter := bvLowered;
  FBevelWidth := 1;
  FHelpType := htContext;
  FPadding := TPadding.Create(Self);
  FPadding.OnChange := DoPaddingChange;
  FPerformingShowingChanged := False;
end;

function TWinControl.GetAlignDisabled: Boolean;
begin
  Result := FAlignLevel > 0;
end;

{$IF DEFINED(CLR)}
class function TWinControl.CreateParented(AParentWindow: HWND): TWinControl;
var
  LPreviousPendingParentWindow: HWND;
begin
  LPreviousPendingParentWindow := FPendingParentWindow;
  try
    FPendingParentWindow := AParentWindow;
    Result := Self.Create(nil);
  finally
    FPendingParentWindow := LPreviousPendingParentWindow;
  end;
end;

class function TWinControl.CreateParentedControl(ParentWindow: HWND): TWinControl;
begin
  Result := TWinControl.CreateParented(ParentWindow);
end;
{$ELSE}
constructor TWinControl.CreateParented(ParentWindow: HWnd);
begin
  FParentWindow := ParentWindow;
  Create(nil);
end;

class function TWinControl.CreateParentedControl(ParentWindow: HWnd): TWinControl;
begin
  Result := TWinControl(NewInstance);
  Result.FParentWindow := ParentWindow;
  Result.Create(nil);
end;
{$IFEND}

destructor TWinControl.Destroy;
var
  I: Integer;
  Instance: TControl;
begin
  Destroying;
  if FDockSite then
  begin
    FDockSite := False;
    RegisterDockSite(Self, False);
  end;
  FDockManager := nil;
  FreeAndNil(FDockClients);
  if (Touch <> nil) and (Touch.GestureEngine <> nil) then
    Touch.GestureEngine.Free;
  if Parent <> nil then
    RemoveFocus(True);
  if WindowHandle <> 0 then
    DestroyWindowHandle;
  I := ControlCount;
  while I <> 0 do
  begin
    Instance := Controls[I - 1];
    Remove(Instance);
    Instance.Destroy;
    I := ControlCount;
  end;
  FreeAndNil(FBrush);
{$IF DEFINED(CLR)}
  if Assigned(FHandle.FObjInstance) then
  begin
    FreeObjectInstance(@FHandle.FObjInstance);
    FHandle.FObjInstance := nil;
  end;
{$ELSE}
  if FObjectInstance <> nil then FreeObjectInstance(FObjectInstance);
{$IFEND}
  FreeAndNil(FPadding);
  if ((sfHandleMessages) in TStyleManager.Flags) then
    TStyleManager.Notification(snControlDestroyed, Self);
  inherited Destroy;
end;

procedure TWinControl.FixupTabList;
var
  Count, I, J: Integer;
  List: TList;
  Control: TWinControl;
begin
  if FWinControls <> nil then
  begin
    List := TList.Create;
    try
      Count := FWinControls.Count;
      List.Count := Count;
      for I := 0 to Count - 1 do
      begin
        Control := TWinControl(FWinControls[I]);
        J := Control.FTabOrder;
        if (J >= 0) and (J < Count) then List[J] := Control;
      end;
      for I := 0 to Count - 1 do
      begin
        Control := TWinControl(List[I]);
        if Control <> nil then Control.UpdateTabOrder(I);
      end;
    finally
      List.Free;
    end;
  end;
end;

procedure TWinControl.ReadState(Reader: TReader);
begin
  DisableAlign;
  try
    inherited ReadState(Reader);
  finally
    EnableAlign;
  end;
  FixupTabList;
  if FParent <> nil then Perform(CM_PARENTCTL3DCHANGED, 0, 0);
  UpdateControlState;
end;

procedure TWinControl.AdjustClientRect(var Rect: TRect);
begin
  { WM_NCCALCSIZE performs our BorderWidth logic }
  Inc(Rect.Left, Padding.Left);
  Inc(Rect.Top, Padding.Top);
  Dec(Rect.Right, Padding.Right);
  Dec(Rect.Bottom, Padding.Bottom);
end;

procedure TWinControl.ArrangeControl(AControl: TControl; const ParentSize: TPoint;
  AAlign: TAlign; AAlignInfo: TAlignInfo; var Rect: TRect; UpdateAnchorOrigin: Boolean);
var
  NewLeft, NewTop, NewWidth, NewHeight: Integer;
begin
  with Rect do
  begin
    if (AAlign = alNone) or (AControl.Anchors <> AnchorAlign[AAlign]) then
    begin
      with AControl do
        if (FOriginalParentSize.X <> 0) and (FOriginalParentSize.Y <> 0) then
        begin
          NewLeft := Margins.ControlLeft;
          NewTop := Margins.ControlTop;
          NewWidth := Margins.ControlWidth;
          NewHeight := Margins.ControlHeight;
          if akRight in Anchors then
            if akLeft in Anchors then
              // The AnchorRules.X is the original width
              NewWidth := ParentSize.X - (FOriginalParentSize.X - FAnchorRules.X)
            else
              // The AnchorRules.X is the original left
              NewLeft := ParentSize.X - (FOriginalParentSize.X - FAnchorRules.X)
          else if not (akLeft in Anchors) then
            // The AnchorRules.X is the original middle of the AControl
            NewLeft := MulDiv(FAnchorRules.X, ParentSize.X, FOriginalParentSize.X) -
              NewWidth div 2;
          if akBottom in Anchors then
            if akTop in Anchors then
              // The AnchorRules.Y is the original height
              NewHeight := ParentSize.Y - (FOriginalParentSize.Y - FAnchorRules.Y)
            else
              // The AnchorRules.Y is the original top
              NewTop := ParentSize.Y - (FOriginalParentSize.Y - FAnchorRules.Y)
          else if not (akTop in Anchors) then
            // The AnchorRules.Y is the original middle of the AControl
            NewTop := MulDiv(FAnchorRules.Y, ParentSize.Y, FOriginalParentSize.Y) -
              NewHeight div 2;
          if UpdateAnchorOrigin then
          begin
            if (Anchors * [akLeft, akRight]) <> [] then
              NewLeft := MulDiv(FAnchorOrigin.X, ParentSize.X, FOriginalParentSize.X) - NewWidth div 2;
            if (Anchors * [akTop, akBottom]) <> [] then
              NewTop := MulDiv(FAnchorOrigin.Y, ParentSize.Y, FOriginalParentSize.Y) - NewHeight div 2;
          end;
          Margins.SetControlBounds(NewLeft, NewTop, NewWidth, NewHeight, True);
        end;
      if AAlign = alNone then Exit;
    end;

    NewWidth := Right - Left;
    if (NewWidth < 0) or (AAlign in [alLeft, alRight, alCustom]) then
      NewWidth := AControl.Margins.ControlWidth;
    NewHeight := Bottom - Top;
    if (NewHeight < 0) or (AAlign in [alTop, alBottom, alCustom]) then
      NewHeight := AControl.Margins.ControlHeight;
    NewLeft := Left;
    NewTop := Top;
    case AAlign of
      alTop:
        Inc(Top, NewHeight);
      alBottom:
        begin
          Dec(Bottom, NewHeight);
          NewTop := Bottom;
        end;
      alLeft:
        Inc(Left, NewWidth);
      alRight:
        begin
          Dec(Right, NewWidth);
          NewLeft := Right;
        end;
      alCustom:
        begin
          NewLeft := AControl.Left;
          NewTop := AControl.Top;
          CustomAlignPosition(AControl, NewLeft, NewTop, NewWidth,
            NewHeight, Rect, AAlignInfo);
        end;
    end;
  end;
  AControl.Margins.SetControlBounds(NewLeft, NewTop, NewWidth, NewHeight, True);
  { Adjust client rect if AControl didn't resize as we expected }
  if (AControl.Margins.ControlWidth <> NewWidth) or (AControl.Margins.ControlHeight <> NewHeight) then
    with Rect do
      case AAlign of
        alTop: Dec(Top, NewHeight - AControl.Margins.ControlHeight);
        alBottom: Inc(Bottom, NewHeight - AControl.Margins.ControlHeight);
        alLeft: Dec(Left, NewWidth - AControl.Margins.ControlWidth);
        alRight: Inc(Right, NewWidth - AControl.Margins.ControlWidth);
        alClient:
          begin
            Inc(Right, NewWidth - AControl.Margins.ControlWidth);
            Inc(Bottom, NewHeight - AControl.Margins.ControlHeight);
          end;
      end;
end;

procedure TWinControl.AlignControls(AControl: TControl; var Rect: TRect);
var
  AlignList: TList;
  LAlignControlList: TList;

  function GetClientSize(Control: TWinControl): TPoint; {inline;}
  begin
    if Control.HandleAllocated then
      Result := Control.ClientRect.BottomRight
    else
      Result := Point(Control.Width, Control.Height);
    Dec(Result.X, Control.Padding.Left + Control.Padding.Right);
    Dec(Result.Y, Control.Padding.Top + Control.Padding.Bottom);
  end;

  function InsertBefore(C1, C2: TControl; AAlign: TAlign): Boolean;
  begin
    Result := False;
    case AAlign of
      alTop: Result := C1.Margins.ControlTop < C2.Margins.ControlTop;
      alBottom: Result := (C1.Margins.ControlTop + C1.Margins.ControlHeight) >= (C2.Margins.ControlTop + C2.Margins.ControlHeight);
      alLeft: Result := C1.Margins.ControlLeft < C2.Margins.ControlLeft;
      alRight: Result := (C1.Margins.ControlLeft + C1.Margins.ControlWidth) >= (C2.Margins.ControlLeft + C2.Margins.ControlWidth);
      alCustom: Result := CustomAlignInsertBefore(C1, C2);
    end;
  end;

  procedure DoPosition(Control: TControl; AAlign: TAlign; AlignInfo: TAlignInfo);
  begin
    ArrangeControl(Control, GetClientSize(Control.Parent), AAlign, AlignInfo, Rect);
  end;

(*  procedure DoPosition(Control: TControl; AAlign: TAlign; AlignInfo: TAlignInfo);
  var
    NewLeft, NewTop, NewWidth, NewHeight: Integer;
    ParentSize: TPoint;
  begin
    with Rect do
    begin
      if (AAlign = alNone) or (Control.Anchors <> AnchorAlign[AAlign]) then
      begin
        with Control do
          if (FOriginalParentSize.X <> 0) and (FOriginalParentSize.Y <> 0) then
          begin
            NewLeft := Left;
            NewTop := Top;
            NewWidth := Width;
            NewHeight := Height;
            ParentSize := GetClientSize(Parent);
            if akRight in Anchors then
              if akLeft in Anchors then
                // The AnchorRules.X is the original width
                NewWidth := ParentSize.X - (FOriginalParentSize.X - FAnchorRules.X)
              else
                // The AnchorRules.X is the original left
                NewLeft := ParentSize.X - (FOriginalParentSize.X - FAnchorRules.X)
            else if not (akLeft in Anchors) then
              // The AnchorRules.X is the original middle of the control
              NewLeft := MulDiv(FAnchorRules.X, ParentSize.X, FOriginalParentSize.X) -
                NewWidth div 2;
            if akBottom in Anchors then
              if akTop in Anchors then
                // The AnchorRules.Y is the original height
                NewHeight := ParentSize.Y - (FOriginalParentSize.Y - FAnchorRules.Y)
              else
                // The AnchorRules.Y is the original top
                NewTop := ParentSize.Y - (FOriginalParentSize.Y - FAnchorRules.Y)
            else if not (akTop in Anchors) then
              // The AnchorRules.Y is the original middle of the control
              NewTop := MulDiv(FAnchorRules.Y, ParentSize.Y, FOriginalParentSize.Y) -
                NewHeight div 2;
            FAnchorMove := True;
            Include(FControlState, csAligning);
            try
              Margins.SetControlBounds(NewLeft, NewTop, NewWidth, NewHeight);
            finally
              FAnchorMove := False;
              Exclude(FControlState, csAligning);
            end;
          end;
        if AAlign = alNone then Exit;
      end;

      NewWidth := Right - Left;
      if (NewWidth < 0) or (AAlign in [alLeft, alRight, alCustom]) then
        NewWidth := Control.Margins.ControlWidth;
      NewHeight := Bottom - Top;
      if (NewHeight < 0) or (AAlign in [alTop, alBottom, alCustom]) then
        NewHeight := Control.Margins.ControlHeight;
      NewLeft := Left;
      NewTop := Top;
      case AAlign of
        alTop:
          Inc(Top, NewHeight);
        alBottom:
          begin
            Dec(Bottom, NewHeight);
            NewTop := Bottom;
          end;
        alLeft:
          Inc(Left, NewWidth);
        alRight:
          begin
            Dec(Right, NewWidth);
            NewLeft := Right;
          end;
        alCustom:
          begin
            NewLeft := Control.Left;
            NewTop := Control.Top;
            CustomAlignPosition(Control, NewLeft, NewTop, NewWidth,
              NewHeight, Rect, AlignInfo);
          end;
      end;
    end;
    Control.FAnchorMove := True;
    Include(Control.FControlState, csAligning);
    try
      Control.Margins.SetControlBounds(NewLeft, NewTop, NewWidth, NewHeight);
    finally
      Control.FAnchorMove := False;
      Exclude(Control.FControlState, csAligning);
    end;
    { Adjust client rect if control didn't resize as we expected }
    if (Control.Margins.ControlWidth <> NewWidth) or (Control.Margins.ControlHeight <> NewHeight) then
      with Rect do
        case AAlign of
          alTop: Dec(Top, NewHeight - Control.Margins.ControlHeight);
          alBottom: Inc(Bottom, NewHeight - Control.Margins.ControlHeight);
          alLeft: Dec(Left, NewWidth - Control.Margins.ControlWidth);
          alRight: Inc(Right, NewWidth - Control.Margins.ControlWidth);
          alClient:
            begin
              Inc(Right, NewWidth - Control.Margins.ControlWidth);
              Inc(Bottom, NewHeight - Control.Margins.ControlHeight);
            end;
        end;
  end; *)

  function Anchored(Align: TAlign; Anchors: TAnchors): Boolean;
  begin
    case Align of
      alLeft: Result := akLeft in Anchors;
      alTop: Result := akTop in Anchors;
      alRight: Result := akRight in Anchors;
      alBottom: Result := akBottom in Anchors;
      alClient: Result := Anchors = [akLeft, akTop, akRight, akBottom];
    else
      Result := False;
    end;
  end;

  procedure DoAlign(AAlign: TAlign);
  var
    I, J: Integer;
    Control: TControl;
    AlignInfo: TAlignInfo;
  begin
    AlignList.Clear;
    if (AControl <> nil) and ((AAlign = alNone) or AControl.Visible or
      (csDesigning in AControl.ComponentState) and
      not (csNoDesignVisible in AControl.ControlStyle)) and
      (AControl.Align = AAlign) then
      AlignList.Add(AControl);
    for I := 0 to ControlCount - 1 do
    begin
      Control := Controls[I];
      if (Control.Align = AAlign) and ((AAlign = alNone) or (Control.Visible or
        (Control.ControlStyle * [csAcceptsControls, csNoDesignVisible] =
          [csAcceptsControls, csNoDesignVisible])) or
        (csDesigning in Control.ComponentState) and
        not (csNoDesignVisible in Control.ControlStyle)) and
        (not (Control is TCustomForm) or not (csDesigning in Control.ComponentState)) then
      begin
        if Control = AControl then Continue;
        J := 0;
        while (J < AlignList.Count) and not InsertBefore(Control,
          TControl(AlignList[J]), AAlign) do Inc(J);
        AlignList.Insert(J, Control);
      end;
    end;
    for I := 0 to AlignList.Count - 1 do
    begin
      AlignInfo.AlignList := AlignList;
      AlignInfo.ControlIndex := I;
      AlignInfo.Align := AAlign;
      DoPosition(TControl(AlignList[I]), AAlign, AlignInfo);
    end;
  end;

  function AlignWork: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := ControlCount - 1 downto 0 do
      if (Controls[I].Align <> alNone) or
        (Controls[I].Anchors <> [akLeft, akTop]) then Exit;
    Result := False;
  end;

  procedure AlignNestedControls;
  var
    TempList: TList;
    ResizeList: TList;
    ResizeCounts: array of Integer;
    Ctrl: TWinControl;
    I, FirstIndex: Integer;
  begin
    { Convert the recursion into an iteration to prevent the kernel stack overflow
      that occurs if SetWindowPos is called recursively. }
    ResizeList := TList.Create;
    SetLength(ResizeCounts, 0);
    try
      { The controls in the OwnAlignControlList must be added to the ResizeList in
        reverse order. Otherwise the OnResize events are fired in the wrong order }
      FAlignControlList := TList.Create;
      try
        repeat
          try
            for I := LAlignControlList.Count - 1 downto 0 do
            begin
              Ctrl := TWinControl(LAlignControlList[I]);
              FirstIndex := ResizeList.IndexOf(Ctrl);

              { An endless resizing component was stopped by the kernel stack
                overflow bug. So we must catch this condition to prevent an
                endless loop. }
              if (FirstIndex = -1) or (ResizeCounts[FirstIndex] < 10) then
              begin
                Ctrl.FAlignControlList := FAlignControlList;
                try
                  Ctrl.Realign;
                finally
                  if Ctrl <> Self then
                    Ctrl.FAlignControlList := nil;
                end;

                if FirstIndex <> -1 then
                  Inc(ResizeCounts[FirstIndex]);
                ResizeList.Add(Ctrl);
                if Length(ResizeCounts) <> ResizeList.Capacity then
                  SetLength(ResizeCounts, ResizeList.Capacity); // grow with ResizeList
              end
            end;
          finally
            LAlignControlList.Clear;

            { Switch lists. All received WM_SIZE messages put their control into
              the second FAlignControlList. }
            TempList := FAlignControlList;
            FAlignControlList := LAlignControlList;
            LAlignControlList := TempList;
          end;
        until LAlignControlList.Count = 0;
      finally
        { Let another FAlignControlList handle any alignment that came from the
          Resize methods. }
        FreeAndNil(FAlignControlList);
      end;

      { Call the Resize methods }
      for I := ResizeList.Count - 1 downto 0 do
      begin
        Ctrl := TWinControl(ResizeList[I]);
        if not (csLoading in Ctrl.ComponentState) then
          Ctrl.Resize;
      end;
    finally
      ResizeList.Free;
    end;
  end;

  procedure DoAdjustSize;
  var
    Rect: TRect;
    NewWidth, NewHeight: Integer;
  begin
    // When a top level window is maximized the call to SetWindowPos
    // isn't needed unless the size of the window has changed.
    if IsZoomed(Handle) and (GetParent(Handle) = 0) then
    begin
      GetWindowRect(Handle, Rect);
      NewWidth := Width;
      NewHeight := Height;
      CheckNewSize(NewWidth, NewHeight);
      if (Width <> Rect.Right-Rect.Left) or (Height <> Rect.Bottom-Rect.Top) or
         (NewWidth <> Width) or (NewHeight <> Height) then
        SetWindowPos(Handle, 0, 0, 0, Width, Height, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOZORDER);
      RequestAlign;
    end
    else
      AdjustSize;
  end;

var
  UsingParentList: Boolean;
begin
  if FDockSite and FUseDockManager and (FDockManager <> nil) then
    FDockManager.ResetBounds(False);
  { D5 VCL Change (ME): Aligned controls that are not dock clients now
    get realigned.  Previously the code below was "else if AlignWork". }
  if AlignWork then
  begin
    UsingParentList := False;
    LAlignControlList := nil;
    try
      if (FAlignControlList = nil) then
      begin
        if (Parent <> nil) and (Parent.FAlignControlList <> nil) then
        begin
          // Use align list from parent
          UsingParentList := True;
          FAlignControlList := Parent.FAlignControlList
        end
        else
        begin
          // Create list of controls to be aligned
          LAlignControlList := TList.Create;
          FAlignControlList := LAlignControlList;
        end;
      end;

      AdjustClientRect(Rect);
      AlignList := TList.Create;
      try
        DoAlign(alTop);
        DoAlign(alBottom);
        DoAlign(alLeft);
        DoAlign(alRight);
        DoAlign(alClient);
        DoAlign(alCustom);
        DoAlign(alNone);// Move anchored controls
        ControlsAligned;
      finally
        AlignList.Free;
      end;
      // Apply any constraints
      if Showing then
        DoAdjustSize;

      if (LAlignControlList <> nil) and (LAlignControlList.Count > 0) then
        AlignNestedControls;
    finally
      if LAlignControlList <> nil then
      begin
        FAlignControlList := nil;
        FreeAndNil(LAlignControlList);
      end
      else
        if UsingParentList then
          FAlignControlList := nil;
    end;
  end
  else
    // Apply any constraints
    if FAutoSize and Showing then
      DoAdjustSize;
end;

procedure TWinControl.AlignControl(AControl: TControl);
var
  Rect: TRect;
begin
  if not HandleAllocated or (csDestroying in ComponentState) then Exit;
  if FAlignLevel <> 0 then
    Include(FControlState, csAlignmentNeeded)
  else
  begin
    DisableAlign;
    try
      Rect := GetClientRect;
      AlignControls(AControl, Rect);
    finally
      Exclude(FControlState, csAlignmentNeeded);
      EnableAlign;
    end;
  end;
end;

procedure TWinControl.DisableAlign;
begin
  Inc(FAlignLevel);
end;

procedure TWinControl.EnableAlign;
begin
  Dec(FAlignLevel);
  if FAlignLevel = 0 then
  begin
    if csAlignmentNeeded in ControlState then Realign;
  end;
end;

procedure TWinControl.Realign;
begin
  AlignControl(nil);
end;

procedure TWinControl.DoFlipChildren;
var
  Loop: Integer;
  TheWidth: Integer;
  FlippedList: TList;
begin
  FlippedList := TList.Create;
  try
    TheWidth := ClientWidth;
    for Loop := 0 to ControlCount - 1 do with Controls[Loop] do
      if (Owner = Self.Owner) then
      begin
        FlippedList.Add(Controls[Loop]);
        Left := TheWidth - Width - Left;
      end;
    { Allow controls that have associations to realign themselves }
    for Loop := 0 to FlippedList.Count - 1 do
      TControl(FlippedList[Loop]).Perform(CM_ALLCHILDRENFLIPPED, 0, 0);
  finally
    FlippedList.Free;
  end;
end;

function TWinControl.DoHandleStyleMessage(var Message: TMessage): Boolean;
begin
  Result := TStyleManager.HandleMessage(Self, Message, FWindowProc)
end;

procedure TWinControl.FlipChildren(AllLevels: Boolean);
var
  Loop: Integer;
  AlignList: TList;
begin
  if ControlCount = 0 then Exit;
  AlignList := TList.Create;
  DisableAlign;
  try
    { Collect all the Right and Left alignments }
    for Loop := 0 to ControlCount - 1 do with Controls[Loop] do
      if Align in [alLeft, alRight] then AlignList.Add(Controls[Loop]);
    { Flip 'em }
    DoFlipChildren;
  finally
    { Reverse the Right and Left alignments }
    while AlignList.Count > 0 do
    begin
      with TControl(AlignList.Items[AlignList.Count - 1]) do
        if Align = alLeft then
          Align := alRight
        else
          Align := alLeft;
      AlignList.Delete(AlignList.Count - 1);
    end;
    AlignList.Free;
    EnableAlign;
  end;
  if AllLevels then
    for Loop := 0 to ControlCount - 1 do
      if Controls[Loop] is TWinControl then
        TWinControl(Controls[Loop]).FlipChildren(True);
end;

function TWinControl.ContainsControl(Control: TControl): Boolean;
begin
  while (Control <> nil) and (Control <> Self) do Control := Control.Parent;
  Result := Control <> nil;
end;

procedure TWinControl.RemoveFocus(Removing: Boolean);
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then Form.DefocusControl(Self, Removing);
end;

procedure TWinControl.Insert(AControl: TControl);
begin
  if AControl <> nil then
  begin
    if AControl is TWinControl then
    begin
      ListAdd(FWinControls, AControl);
      ListAdd(FTabList, AControl);
    end else
      ListAdd(FControls, AControl);
    AControl.FParent := Self;
  end;
end;

procedure TWinControl.Remove(AControl: TControl);
begin
  if AControl is TWinControl then
  begin
    ListRemove(FTabList, AControl);
    ListRemove(FWinControls, AControl);
  end else
    ListRemove(FControls, AControl);
  AControl.FParent := nil;
end;

procedure TWinControl.InsertControl(AControl: TControl);
var
  Item: TControlListItem;
begin
  AControl.ValidateContainer(Self);
  Item.Control := AControl;
  Item.Parent := Self;
{$IF DEFINED(CLR)}
  ControlListChanging(True, Item);
  if Item.Parent <> Self then Exit;
  ControlListChange(True, AControl);
{$ELSE}
  Perform(CM_CONTROLLISTCHANGING, Winapi.Windows.WPARAM(@Item), Winapi.Windows.LPARAM(True));
  if Item.Parent <> Self then Exit;
  Perform(CM_CONTROLLISTCHANGE, Winapi.Windows.WPARAM(AControl), Winapi.Windows.LPARAM(True));
{$IFEND}
  Insert(AControl);
  if not (csReading in AControl.ComponentState) then
  begin
    AControl.Perform(CM_PARENTCOLORCHANGED, 0, 0);
    AControl.Perform(CM_PARENTFONTCHANGED, 0, 0);
    AControl.Perform(CM_PARENTSHOWHINTCHANGED, 0, 0);
    AControl.Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
    AControl.Perform(CM_PARENTDOUBLEBUFFEREDCHANGED, 0, 0);
    AControl.Perform(CM_PARENTTABLETOPTIONSCHANGED, 0, 0);
    if AControl is TWinControl then
    begin
      AControl.Perform(CM_PARENTCTL3DCHANGED, 0, 0);
      UpdateControlState;
    end else
      if HandleAllocated then AControl.Invalidate;
    AlignControl(AControl);
  end;
{$IF DEFINED(CLR)}
  ControlChange(True, AControl);
{$ELSE}
  Perform(CM_CONTROLCHANGE, Winapi.Windows.WPARAM(AControl), Winapi.Windows.LPARAM(True));
{$IFEND}
end;

procedure TWinControl.RemoveControl(AControl: TControl);
{$IF NOT DEFINED(CLR)}
var
  Item: TControlListItem;
{$IFEND}
begin
{$IF DEFINED(CLR)}
  ControlChange(False, AControl);
{$ELSE}
  Item.Control := AControl;
  Item.Parent := Self;
  Perform(CM_CONTROLCHANGE, Winapi.Windows.WPARAM(AControl), Winapi.Windows.LPARAM(False));
{$IFEND}
  if AControl is TWinControl then
    with TWinControl(AControl) do
    begin
      RemoveFocus(True);
      DestroyHandle;
    end
  else
    if HandleAllocated then
      AControl.InvalidateControl(AControl.Visible, False);
  Remove(AControl);
{$IF DEFINED(CLR)}
  ControlListChange(False, AControl);
  ControlListChanging(False, AControl, Self);
{$ELSE}
  Perform(CM_CONTROLLISTCHANGE, Winapi.Windows.WPARAM(AControl), Winapi.Windows.LPARAM(False));
  Perform(CM_CONTROLLISTCHANGING, Winapi.Windows.WPARAM(@Item), Winapi.Windows.LPARAM(False));
{$IFEND}
  Realign;
end;

function TWinControl.GetControl(Index: Integer): TControl;
var
  N: Integer;
begin
  if FControls <> nil then N := FControls.Count else N := 0;
  if Index < N then
    Result := TControl(FControls[Index]) else
    Result := TControl(FWinControls[Index - N]);
end;

function TWinControl.GetControlCount: Integer;
begin
  Result := 0;
  if FControls <> nil then Inc(Result, FControls.Count);
  if FWinControls <> nil then Inc(Result, FWinControls.Count);
end;

[SecurityPermission(SecurityAction.LinkDemand, UnmanagedCode=True)]
procedure TWinControl.Broadcast(var Message);
{$IF DEFINED(CLR)}
var
  I: Integer;
  Msg: TMessage;
begin
  Msg := UnwrapMessage(TObject(Message));
  for I := 0 to ControlCount - 1 do
  begin
    Controls[I].WindowProc(Msg);
    if Msg.Result <> 0 then Exit;
  end;
{$ELSE}
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
  begin
    Controls[I].WindowProc(TMessage(Message));
    if TMessage(Message).Result <> 0 then Exit;
  end;
{$IFEND}
end;

procedure TWinControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FMouseControl) and (Operation = opRemove) then
    FMouseControl := nil;
end;

procedure TWinControl.NotifyControls(Msg: Word);
var
  Message: TMessage;
begin
{$IF DEFINED(CLR)}
  Message := TMessage.Create(Msg, 0, 0);
{$ELSE}
  Message.Msg := Msg;
  Message.WParam := 0;
  Message.LParam := 0;
  Message.Result := 0;
{$IFEND}
  Broadcast(Message);
end;

{$IF DEFINED(CLR)}
procedure TWinControl.CreateSubClass(var Params: TCreateParams;
  ControlClassName: string);
{$ELSE}
procedure TWinControl.CreateSubClass(var Params: TCreateParams;
  ControlClassName: PChar);
{$IFEND}
const
  CS_OFF = CS_OWNDC or CS_CLASSDC or CS_PARENTDC or CS_GLOBALCLASS;
  CS_ON = CS_VREDRAW or CS_HREDRAW;
var
  SaveInstance: HINST;
begin
  if ControlClassName <> nil then
    with Params do
    begin
      {We need to save the hInstance, because GetClassInfo changes it
       and the hInstance must be correct later when we check whether the
       class is already registered}

      SaveInstance := WindowClass.hInstance;
      if not GetClassInfo(HInstance, ControlClassName, WindowClass) and
{$IF NOT DEFINED(CLR)}
        not GetClassInfo(MainInstance, ControlClassName, WindowClass) and
{$IFEND}
        not GetClassInfo(0, ControlClassName, WindowClass) then
        GetClassInfo(WindowClass.hInstance, ControlClassName, WindowClass);

      WindowClass.hInstance := SaveInstance;
{$IF DEFINED(CLR)}
      WndProc := nil;
{$IFEND}
      WindowClass.style := WindowClass.style and not CS_OFF or CS_ON;
    end;
end;

procedure TWinControl.AddBiDiModeExStyle(var ExStyle: DWORD);
begin
  if UseRightToLeftReading then
    ExStyle := ExStyle or WS_EX_RTLREADING;
  if UseRightToLeftScrollbar then
    ExStyle := ExStyle or WS_EX_LEFTSCROLLBAR;
  if UseRightToLeftAlignment then
    if GetControlsAlignment = taLeftJustify then
      ExStyle := ExStyle or WS_EX_RIGHT
    else if GetControlsAlignment = taRightJustify then
      ExStyle := ExStyle or WS_EX_LEFT;
end;

procedure TWinControl.CreateParams(var Params: TCreateParams);
begin
{$IF NOT DEFINED(CLR)}
  FillChar(Params, SizeOf(Params), 0);
{$IFEND}
  with Params do
  begin
    Caption := FText;
    Style := WS_CHILD or WS_CLIPSIBLINGS;
    AddBiDiModeExStyle(ExStyle);
    if csAcceptsControls in ControlStyle then
    begin
      Style := Style or WS_CLIPCHILDREN;
      ExStyle := ExStyle or WS_EX_CONTROLPARENT;
    end;
    if not (csDesigning in ComponentState) and not Enabled then
      Style := Style or WS_DISABLED;
    if FTabStop then Style := Style or WS_TABSTOP;
    X := FLeft;
    Y := FTop;
    Width := FWidth;
    Height := FHeight;
    if Parent <> nil then
      WndParent := Parent.GetHandle else
      WndParent := FParentWindow;
    WindowClass.style := CS_VREDRAW + CS_HREDRAW + CS_DBLCLKS;
    WindowClass.hCursor := LoadCursor(0, IDC_ARROW);
    WindowClass.hbrBackground := 0;
    WindowClass.hInstance := HInstance;
{$IF DEFINED(CLR)}
    WindowClass.hIcon := 0;
    WndProc := WindowProcDelegate;
    WinClassName := ClassName;
{$ELSE}
    WindowClass.lpfnWndProc := @DefWindowProc;
    StrPCopy(WinClassName, ClassName);
{$IFEND}
  end;
end;

{$IF DEFINED(CLR)}
type
  TIntPtrToWndProc = class
    FWndProc: IntPtr;
    function WndProc(p1: HWND; p2: UINT; p3: WPARAM; p4: LPARAM): LRESULT;
    constructor Create(WndProc: IntPtr);
  end;

constructor TIntPtrToWndProc.Create(WndProc: IntPtr);
begin
  inherited Create;
  FWndProc := WndProc;
end;

function TIntPtrToWndProc.WndProc(p1: HWND; p2: UINT; p3: WPARAM; p4: LPARAM): LRESULT;
begin
  Result := CallWindowProc(FWndProc, p1, p2, p3, p4);
end;

function IntPtrToWndProc(WndProc: IntPtr): TFNWndProc;
var
  Instance: TIntPtrToWndProc;
begin
  Instance := TIntPtrToWndProc.Create(WndProc);
  Result := Instance.WndProc;
end;

function WndClassFromWndClassInfo(WndClassInfo: TWndClassInfo): TWndClass;
begin
  Result.style := WndClassInfo.Style;
  Result.cbClsExtra := WndClassInfo.cbClsExtra;
  Result.cbWndExtra := WndClassInfo.cbWndExtra;
  Result.hInstance := WndClassInfo.hInstance;
  Result.hIcon := WndClassInfo.hIcon;
  Result.hCursor := WndClassInfo.hCursor;
  Result.hbrBackground := WndClassInfo.hbrBackground;
  Result.lpszMenuName := Marshal.PtrToStringAuto(WndClassInfo.lpszMenuName);

  // lpszClassName isn't used (WinClassName is instead, see CreateWnd)
  //Result.lpszClassName := Marshal.PtrToStringAuto(WndClassInfo.lpszClassName);
end;
{$IFEND}

[UIPermission(SecurityAction.InheritanceDemand, Window=UIPermissionWindow.AllWindows),
UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TWinControl.CreateWnd;
var
  Params: TCreateParams;
  ClassRegistered: Boolean;
{$IF DEFINED(CLR)}
  TempClass: TWndClassInfo;
  WndClass: TWndClass;
{$ELSE}
  TempClass: TWndClass;
{$IFEND}
begin
  CreateParams(Params);
{$IF DEFINED(CLR)}
  Params.WinClassName := Format('%s.%d',
    [Params.WinClassName, AppDomain.CurrentDomain.GetHashCode]);
{$IFEND}
  with Params do
  begin
    if (WndParent = 0) and (Style and WS_CHILD <> 0) then
      if (Owner <> nil) and (csReading in Owner.ComponentState) and
        (Owner is TWinControl) then
        WndParent := TWinControl(Owner).Handle
      else
        raise EInvalidOperation.CreateFmt(SParentRequired, [Name]);
{$IF DEFINED(CLR)}
    if Assigned(WndProc) then
      FDefWndProc := WndProc
    else
      FDefWndProc := IntPtrToWndProc(WindowClass.lpfnWndProc);
    ClassRegistered := GetClassInfo(WindowClass.hInstance, WinClassName, TempClass);
    if not ClassRegistered {or not DelegatesEqual(TempClass.lpfnWndProc, InitProcDelegate)} then
    begin
      if ClassRegistered then
        Winapi.Windows.UnregisterClass(WinClassName, WindowClass.hInstance);
      WndClass := WndClassFromWndClassInfo(WindowClass);
      WndClass.lpfnWndProc := InitProcDelegate;
      WndClass.lpszClassName := WinClassName;
      //WndClass.lpszMenuName := '';
      if Winapi.Windows.RegisterClass(WndClass) = 0 then
        RaiseLastOSError;
    end;
{$ELSE}
    FDefWndProc := WindowClass.lpfnWndProc;
    ClassRegistered := GetClassInfo(WindowClass.hInstance, WinClassName, TempClass);
    if not ClassRegistered or (TempClass.lpfnWndProc <> @InitWndProc) then
    begin
      if ClassRegistered then
        Winapi.Windows.UnregisterClass(WinClassName, WindowClass.hInstance);
      WindowClass.lpfnWndProc := @InitWndProc;
      WindowClass.lpszClassName := WinClassName;
      if Winapi.Windows.RegisterClass(WindowClass) = 0 then
        RaiseLastOSError;
    end;
{$IFEND}
    CreationControl := Self;
    CreateWindowHandle(Params);
    if WindowHandle = 0 then
      RaiseLastOSError;
    if (GetWindowLong(WindowHandle, GWL_STYLE) and WS_CHILD <> 0) and
      (GetWindowLong(WindowHandle, GWL_ID) = 0) then
      SetWindowLong(WindowHandle, GWL_ID, WindowHandle);
  end;
{$IF NOT DEFINED(CLR)}
  StrDispose(FText);
{$IFEND}
  FText := nil;
  UpdateBounds;
  Perform(WM_SETFONT, FFont.Handle, 1);
  if AutoSize then
    AdjustSize;
  if (Touch.GestureEngine <> nil) and (csGestures in ControlStyle) then
    Touch.GestureEngine.Active := True;
end;

[UIPermission(SecurityAction.InheritanceDemand, Window=UIPermissionWindow.AllWindows),
UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TWinControl.CreateWindowHandle(const Params: TCreateParams);
begin
  with Params do
    WindowHandle := CreateWindowEx(ExStyle, WinClassName, Caption, Style,
      X, Y, Width, Height, WndParent, 0, WindowClass.hInstance, Param);
end;

procedure TWinControl.ReadDesignSize(Reader: TReader);
begin
  Reader.ReadListBegin;
  FDesignSize.X := Reader.ReadInteger;
  FDesignSize.Y := Reader.ReadInteger;
  Include(FScalingFlags, sfDesignSize);
  Reader.ReadListEnd;
end;

procedure TWinControl.WriteDesignSize(Writer: TWriter);
begin
  FDesignSize := ClientRect.BottomRight;
  Writer.WriteListBegin;
  Writer.WriteInteger(FDesignSize.X);
  Writer.WriteInteger(FDesignSize.Y);
  Writer.WriteListEnd;
end;

procedure TWinControl.DefineProperties(Filer: TFiler);

  function PointsEqual(const P1, P2: TPoint): Boolean;
  begin
    Result := ((P1.X = P2.X) and (P1.Y = P2.Y));
  end;

  function DoWriteDesignSize: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    if (Filer.Ancestor = nil) or not PointsEqual(FDesignSize,
      TWinControl(Filer.Ancestor).FDesignSize) then
    begin
      if FControls <> nil then
        for I := 0 to FControls.Count - 1 do
          with TControl(FControls[I]) do
            if (Align = alNone) and (Anchors <> [akLeft, akTop]) then
              Exit;
      if FWinControls <> nil then
        for I := 0 to FWinControls.Count - 1 do
          with TControl(FWinControls[I]) do
            if (Align = alNone) and (Anchors <> [akLeft, akTop]) then
              Exit;
    end;
    Result := False;
  end;

begin
  inherited;
  Filer.DefineProperty('DesignSize', ReadDesignSize, WriteDesignSize,
    DoWriteDesignSize);
end;

procedure TWinControl.DestroyWnd;
{$IF NOT DEFINED(CLR)}
var
  Len: Integer;
{$IFEND}
begin
  if FText = nil then
{$IF DEFINED(CLR)}
    FText := GetText;
{$ELSE}
  begin
    Len := GetTextLen;
    if Len < 1 then FText := StrNew('') else
    begin
      FText := StrAlloc(Len + 1);
      GetTextBuf(FText, StrBufSize(FText));
    end;
  end;
{$IFEND}
  FreeDeviceContexts;
  DestroyWindowHandle;
end;

procedure TWinControl.DestroyWindowHandle;
begin
  if Touch.GestureEngine <> nil then
    Touch.GestureEngine.Active := False;
  Include(FControlState, csDestroyingHandle);
  try
    if not Winapi.Windows.DestroyWindow(WindowHandle) then
      RaiseLastOSError;
  finally
    Exclude(FControlState, csDestroyingHandle);
  end;
  WindowHandle := 0;
end;

function TWinControl.PrecedingWindow(Control: TWinControl): HWND;
var
  I: Integer;
begin
  for I := FWinControls.IndexOf(Control) + 1 to FWinControls.Count - 1 do
  begin
    Result := TWinControl(FWinControls[I]).WindowHandle;
    if Result <> 0 then Exit;
  end;
  Result := HWND_TOP;
end;

[UIPermission(SecurityAction.InheritanceDemand, Window=UIPermissionWindow.AllWindows),
UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TWinControl.CreateHandle;
var
  I: Integer;
begin
  if WindowHandle = 0 then
  begin
    CreateWnd;
{$IF NOT DEFINED(CLR)}
    SetProp(FHandle, MakeIntAtom(ControlAtom), THandle(Self));
    SetProp(FHandle, MakeIntAtom(WindowAtom), THandle(Self));
{$IFEND}
    if Parent <> nil then
      SetWindowPos(WindowHandle, Parent.PrecedingWindow(Self), 0, 0, 0, 0,
        SWP_NOMOVE + SWP_NOSIZE + SWP_NOACTIVATE);
    for I := 0 to ControlCount - 1 do
      Controls[I].UpdateAnchorRules;
  end;
end;

function TWinControl.CustomAlignInsertBefore(C1, C2: TControl): Boolean;
begin
  if Assigned(FOnAlignInsertBefore) then
    Result := FOnAlignInsertBefore(Self, C1, C2)
  else
    Result := False;
end;

procedure TWinControl.CustomAlignPosition(Control: TControl; var NewLeft,
  NewTop, NewWidth, NewHeight: Integer; var AlignRect: TRect;
  AlignInfo: TAlignInfo);
begin
  if Assigned(FOnAlignPosition) then
    FOnAlignPosition(Self, Control, NewLeft, NewTop, NewWidth, NewHeight,
      AlignRect, AlignInfo);
end;

type
{$IF DEFINED(CLR)}
  TDestroyChildren = class
    FParent: HWND;
    function DestroyChildWindow(Window: HWND; Data: LPARAM): BOOL;
  end;
{$ELSE}
  PDestroyChildData = ^TDestroyChildData;
  TDestroyChildData = record
    Parent: HWND;
    Recreating: Boolean;
  end;
{$IFEND}

{$IF DEFINED(CLR)}
function TDestroyChildren.DestroyChildWindow(Window: HWND; Data: LPARAM): BOOL;
begin
  if (Window <> FParent) and (FindControl(Window) = nil) and
    (GetWindowLong(Window, GWL_HWNDPARENT) = FParent) then
    SendMessage(Window, CM_DESTROYHANDLE, Data, 0);
  Result := True;
end;
{$ELSE}
function DestroyChildWindow(Window: HWND; Data: PDestroyChildData): BOOL; stdcall;
begin
  if (Window <> Data^.Parent) and (FindControl(Window) = nil) and
    (HWND(GetWindowLong(Window, GWL_HWNDPARENT)) = Data^.Parent) then
    SendMessage(Window, CM_DESTROYHANDLE, Byte(Data^.Recreating), 0);
  Result := True;
end;
{$IFEND}

procedure TWinControl.DestroyHandle;
var
  I: Integer;
{$IF DEFINED(CLR)}
  DestroyChildren: TDestroyChildren;
{$ELSE}
  LData: TDestroyChildData;
{$IFEND}
begin
  if WindowHandle <> 0 then
  begin
    if FWinControls <> nil then
      for I := 0 to FWinControls.Count - 1 do
        TWinControl(FWinControls[I]).DestroyHandle;
{$IF DEFINED(CLR)}
    DestroyChildren := TDestroyChildren.Create;
    DestroyChildren.FParent := WindowHandle;
    EnumChildWindows(WindowHandle, DestroyChildren.DestroyChildWindow,
      Byte(csRecreating in ControlState));
{$ELSE}
    LData.Parent := FHandle;
    LData.Recreating := csRecreating in ControlState;
    EnumChildWindows(FHandle, @DestroyChildWindow, Winapi.Windows.LPARAM(@LData));
{$IFEND}
    DestroyWnd;
  end;
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TWinControl.RecreateWnd;
begin
  if WindowHandle <> 0 then Perform(CM_RECREATEWND, 0, 0);
end;

procedure TWinControl.CMRecreateWnd(var Message: TMessage);
var
  WasFocused: Boolean;
begin
  WasFocused := Focused;
  UpdateRecreatingFlag(True);
  try
    DestroyHandle;
    UpdateControlState;
  finally
    UpdateRecreatingFlag(False);
  end;
  if WasFocused and (WindowHandle <> 0) then
    Winapi.Windows.SetFocus(WindowHandle);
end;

procedure TWinControl.UpdateShowing;
var
  ShowControl: Boolean;
  I: Integer;
begin
  ShowControl := (FVisible and (not (csDesigning in ComponentState) or not (csDesignerHide in ControlState)) or
    ((csDesigning in ComponentState) and not (csDesignerHide in ControlState)) and
    not (csNoDesignVisible in ControlStyle)) and
    not (csReadingState in ControlState) and not (csDestroying in ComponentState);
  if ShowControl then
  begin
    if WindowHandle = 0 then CreateHandle;
    if FWinControls <> nil then
      for I := 0 to FWinControls.Count - 1 do
        TWinControl(FWinControls[I]).UpdateShowing;
  end;
  if WindowHandle <> 0 then
    if FShowing <> ShowControl then
    begin
      FShowing := ShowControl;
      try
        FPerformingShowingChanged := True;
        try
          Perform(CM_SHOWINGCHANGED, 0, 0);
        finally
          FPerformingShowingChanged := False;
        end;
      except
        FShowing := not ShowControl;
        raise;
      end;
    end;
end;

procedure TWinControl.UpdateControlState;
var
  Control: TWinControl;
begin
  Control := Self;
  while Control.Parent <> nil do
  begin
    Control := Control.Parent;
    if not Control.Showing then
    begin
      if FShowing and not FPerformingShowingChanged then
      begin
        FPerformingShowingChanged := True;
        try
          FShowing := False;
          Perform(CM_SHOWINGCHANGED, 0, 0);
        finally
          FPerformingShowingChanged := False;
        end;
      end;
      Exit;
    end;
  end;
  if (Control is TCustomForm) or
     (not (csDestroying in ComponentState) and (Control.FParentWindow <> 0)) then UpdateShowing;
end;

procedure TWinControl.SetParentDoubleBuffered(Value: Boolean);
begin
  if Value <> FParentDoubleBuffered then
  begin
    FParentDoubleBuffered := Value;
    if (FParent <> nil) and not (csReading in ComponentState) then
      Perform(CM_PARENTDOUBLEBUFFEREDCHANGED, 0, 0);
  end;
end;

procedure TWinControl.SetParentWindow(Value: HWND);
begin
  if (FParent = nil) and (FParentWindow <> Value) then
  begin
    if (WindowHandle <> 0) and (FParentWindow <> 0) and (Value <> 0) then
    begin
      FParentWindow := Value;
      Winapi.Windows.SetParent(WindowHandle, Value);
      if (Win32MajorVersion >= 5) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
        Perform(WM_CHANGEUISTATE, MakeWParam(UIS_INITIALIZE, UISF_HIDEACCEL or UISF_HIDEFOCUS), 0);
      UpdateControlState;
    end else
    begin
      UpdateRecreatingFlag(True);
      try
        DestroyHandle;
        FParentWindow := Value;
        UpdateControlState;
      finally
        UpdateRecreatingFlag(False);
      end;
    end;
  end;
end;

procedure TWinControl.MainWndProc(var Message: TMessage);
begin
  try
    try
      WindowProc(Message);
    finally
      FreeDeviceContexts;
      FreeMemoryContexts;
    end;
  except
    Application.HandleException(Self);
  end;
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
function TWinControl.ControlAtPos(const Pos: TPoint; AllowDisabled,
  AllowWinControls, AllLevels: Boolean): TControl;
var
  I: Integer;
  P: TPoint;
  LControl: TControl;

  function GetControlAtPos(AControl: TControl): Boolean;
  begin
    with AControl do
    begin
      P := Point(Pos.X - Left, Pos.Y - Top);
      Result := PtInRect(ClientRect, P) and
                ((csDesigning in ComponentState) and (Visible or
                not (csNoDesignVisible in ControlStyle)) or
                (Visible and (Enabled or AllowDisabled) and
                (Perform(CM_HITTEST, 0, PointToLParam(P)) <> 0)));
      if Result then
        LControl := AControl;
    end;
  end;

begin
  LControl := nil;
  if AllowWinControls and (FWinControls <> nil) then
    for I := FWinControls.Count - 1 downto 0 do
    begin
      if AllLevels then
        LControl := TWinControl(FWinControls[I]).ControlAtPos(Pos, AllowDisabled, True, True);
      if (LControl = nil) and GetControlAtPos(TWinControl(FWinControls[I])) then
        Break;
    end;
  if (FControls <> nil) and
     (LControl = nil) then
    for I := FControls.Count - 1 downto 0 do
      if GetControlAtPos(TControl(FControls[I]) ) then
        Break;
  Result := LControl;
end;

function TWinControl.IsControlMouseMsg(var Message: TWMMouse): Boolean;
var
  Control: TControl;
  P: TPoint;
begin
  if GetCapture = Handle then
  begin
    if (CaptureControl <> nil) and (CaptureControl.Parent = Self) then
      Control := CaptureControl
    else
      Control := nil;
  end
  else
    Control := ControlAtPos(SmallPointToPoint(Message.Pos), False);
  Result := False;
  if Control <> nil then
  begin
    P.X := Message.XPos - Control.Left;
    P.Y := Message.YPos - Control.Top;
    Message.Result := Control.Perform(Message.Msg, Message.Keys, PointToLParam(P));
    Result := True;
  end;
end;

function TWinControl.IsControlActivateMsg(var Message: TWMMouseActivate; Control: TControl = nil): Boolean;
var
  P: TPoint;
  KeyState: TKeyboardState;
  MouseActivateRec: TMouseActivateRec;
{$IF DEFINED(CLR)}
  ActivateMsg: TCMMouseActivate;
{$IFEND}
begin
  P := ScreenToClient(SmallPointToPoint(System.Types.SmallPoint(GetMessagePos)));
  if Control = nil then
    Control := ControlAtPos(P, False);
  if Control <> nil then
  begin
    with MouseActivateRec do
    begin
      if Control <> Self then
      begin
        MousePos.X := P.X - Control.Left;
        MousePos.Y := P.Y - Control.Top;
      end else
        MousePos := P;
      HitTest := Message.HitTestCode;
      TopLevel := Message.TopLevel;
      case Message.MouseMsg of
        WM_LBUTTONDOWN, WM_LBUTTONUP, WM_NCLBUTTONDOWN, WM_NCLBUTTONUP:
          Button := mbLeft;
        WM_MBUTTONDOWN, WM_MBUTTONUP, WM_NCMBUTTONDOWN, WM_NCMBUTTONUP:
          Button := mbMiddle;
        WM_RBUTTONDOWN, WM_RBUTTONUP, WM_NCRBUTTONDOWN, WM_NCRBUTTONUP:
          Button := mbRight;
      else
        Button := mbLeft;
      end;
      GetKeyboardState(KeyState);
      ShiftState := KeyboardStateToShiftState(KeyState) + MouseOriginToShiftState;
    end;
{$IF DEFINED(CLR)}
    ActivateMsg := TCMMouseActivate.Create;
    ActivateMsg.Msg := CM_MOUSEACTIVATE;
    ActivateMsg.MouseActivateRec := MouseActivateRec;
    Message.Result := Control.Perform(CM_MOUSEACTIVATE, ActivateMsg);
{$ELSE}
    Message.Result := Control.Perform(CM_MOUSEACTIVATE, 0, LPARAM(@MouseActivateRec));
{$IFEND}
    Result := True;
  end else
    Result := False;
end;

procedure TWinControl.WndProc(var Message: TMessage);
var
  Form: TCustomForm;
  LMouseEvent: TTrackMouseEvent;
  P: TPoint;
  Target: TControl;
{$IF DEFINED(CLR)}
  MouseMsg: TWMMouse;
  ActivateMsg: TWMMouseActivate;
{$IFEND}
begin
  if (FHandle <> 0) {HandleAllocated} and
     ((sfHandleMessages) in TStyleManager.Flags) and
     not (csDestroying in ComponentState) and
     not (csDestroyingHandle in ControlState) and
     not (csOverrideStylePaint in ControlStyle) and
     DoHandleStyleMessage(Message) then
    Exit;

  case Message.Msg of
    CM_UNTHEMECONTROL:
      if (csDesigning in ComponentState) and StyleServices.Available then
      begin
        SetWindowTheme(Handle, ' ', ' ');
        SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_FRAMECHANGED);
      end;
    CM_SETACTIVECONTROL:
      begin
        Form := GetParentForm(Self);
        if (Form <> nil) and (Form <> Self) then
          Form.Perform(CM_SETACTIVECONTROL, Message.WParam, Message.LParam);
      end;
    WM_SETFOCUS:
      begin
        Form := GetParentForm(Self);
        if (Form <> nil) and (not (csDesigning in Form.ComponentState) or (Form.Parent = nil)) then
          if not Form.SetFocusedControl(Self) then Exit;
      end;
    WM_KILLFOCUS:
      if csFocusing in ControlState then Exit;
    WM_NCHITTEST:
      begin
        inherited WndProc(Message);
        if (Message.Result = HTTRANSPARENT) and (ControlAtPos(ScreenToClient(
          SmallPointToPoint(TWMNCHitTest(Message).Pos)), False) <> nil) then
          Message.Result := HTCLIENT;
        Exit;
      end;
    WM_MOUSELEAVE:
      begin
        FMouseInClient := False;
        if FMouseControl <> nil then
          FMouseControl.Perform(CM_MOUSELEAVE, 0, 0)
        else
          Perform(CM_MOUSELEAVE, 0, 0);
        FMouseControl := nil;
      end;
    WM_MOUSEFIRST..WM_MOUSELAST:
      begin
{$IF DEFINED(CLR)}
        MouseMsg := TWMMouse.Create(Message);
{$ELSE}
        with FTouchManager do
          if (GestureEngine <> nil) and (efMouseEvents in GestureEngine.Flags) then
            GestureEngine.Notification(Message);
{$IFEND}
        if Message.Msg = WM_MOUSEMOVE then
        begin
{$IF DEFINED(CLR)}
          P := ClientToScreen(Point(MouseMsg.XPos, MouseMsg.YPos));
{$ELSE}
          P := ClientToScreen(Point(TWMMouse(Message).XPos, TWMMouse(Message).YPos));
{$IFEND}
          CaptureControl := GetCaptureControl;
          if CaptureControl = nil then
            Target := FindDragTarget(P, True)
          else
            Target := CaptureControl;
          if (FMouseControl <> Target) then
          begin
            if ((FMouseControl <> nil) and (CaptureControl = nil)) or
               ((CaptureControl <> nil) and (FMouseControl = CaptureControl)) or
               ((CaptureControl is TControl) and (CaptureControl.Parent = FMouseControl)) then
              FMouseControl.Perform(CM_MOUSELEAVE, 0, 0);
            if FMouseControl <> nil then
              FMouseControl.RemoveFreeNotification(Self);
            FMouseControl := Target;
            if FMouseControl <> nil then
              FMouseControl.FreeNotification(Self);
            if ((FMouseControl <> nil) and (CaptureControl = nil)) or
               ((CaptureControl <> nil) and (FMouseControl = CaptureControl)) then
              FMouseControl.Perform(CM_MOUSEENTER, 0, 0);
          end;
          if not FMouseInClient then
          begin
            FMouseInClient := True;
            // Register for a WM_MOUSELEAVE message which ensures CM_MOUSELEAVE
            // is called when the mouse leaves the TWinControl
            LMouseEvent.dwFlags := TME_LEAVE;
            LMouseEvent.hwndTrack := Handle;
            LMouseEvent.dwHoverTime := HOVER_DEFAULT;
{$IF DEFINED(CLR)}
            LMouseEvent.cbSize := Marshal.SizeOf(TypeOf(LMouseEvent));
            TrackMouseEvent(LMouseEvent);
{$ELSE}
            LMouseEvent.cbSize := SizeOf(LMouseEvent);
            _TrackMouseEvent(@LMouseEvent);
{$IFEND}
          end;
        end;
{$IF DEFINED(CLR)}
        if IsControlMouseMsg(MouseMsg) then
{$ELSE}
        if IsControlMouseMsg(TWMMouse(Message)) then
{$IFEND}
        begin
          { Check HandleAllocated because IsControlMouseMsg might have freed the
            window if user code executed something like Parent := nil. }
          if (Message.Result = 0) and HandleAllocated then
            DefWindowProc(Handle, Message.Msg, Message.wParam, Message.lParam);
          Exit;
        end;
      end;
    WM_MOUSEACTIVATE:
      begin
{$IF DEFINED(CLR)}
        ActivateMsg := TWMMouseActivate.Create(Message);
        if IsControlActivateMsg(ActivateMsg) then
{$ELSE}
        if IsControlActivateMsg(TWMMouseActivate(Message)) then
{$IFEND}
        begin
          if (Message.Result = 0) and HandleAllocated then
            inherited WndProc(Message);
          Exit;
        end;
      end;
    WM_KEYFIRST..WM_KEYLAST:
      if Dragging then Exit;
    WM_CANCELMODE:
      if (GetCapture = Handle) and (CaptureControl <> nil) and
        (CaptureControl.Parent = Self) then
        CaptureControl.Perform(WM_CANCELMODE, 0, 0);
    CM_DESTROYHANDLE:
      begin
        if Boolean(Message.WParam) then // Sender has csRecreating set
          UpdateRecreatingFlag(True);
        try
          DestroyHandle;
        finally
          if Boolean(Message.WParam) then
            UpdateRecreatingFlag(False);
        end;
        Exit;
      end;
    CM_THEMECHANGED:
      Broadcast(Message);
    WM_TOUCH:
      with FTouchManager do
        if (GestureEngine <> nil) and (efTouchEvents in GestureEngine.Flags) then
          GestureEngine.Notification(Message);
  end;
  inherited WndProc(Message);

  if Message.Msg = WM_UPDATEUISTATE then
    Invalidate; // Ensure control is repainted
end;

procedure TWinControl.DefaultHandler(var Message);
{$IF DEFINED(CLR)}
var
  Msg: TMessage;
{$IFEND}
begin
  if WindowHandle <> 0 then
  begin
{$IF DEFINED(CLR)}
    Msg := UnwrapMessage(TObject(Message));
    with Msg do
{$ELSE}
    with TMessage(Message) do
{$IFEND}
    begin
      if (Msg = WM_CONTEXTMENU) and (Parent <> nil) then
      begin
        Result := Parent.Perform(Msg, WParam, LParam);
        if Result <> 0 then Exit;
      end;
      case Msg of
        WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
          Result := SendMessage(LParam, CN_BASE + Msg, WParam, LParam);
        CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
          begin
            SetTextColor(WParam, ColorToRGB(FFont.Color));
            SetBkColor(WParam, ColorToRGB(FBrush.Color));
            Result := LRESULT(FBrush.Handle);
          end;
      else
{$IF NOT DEFINED(CLR)}
        if Msg = RM_GetObjectInstance then
          Result := Winapi.Windows.LRESULT(Self)
        else
{$IFEND}
          Result := CallWindowProc(FDefWndProc, WindowHandle, Msg, WParam, LParam);
      end;
      if Msg = WM_SETTEXT then
        SendDockNotification(Msg, WParam, LParam);
    end;
  end
  else
    inherited DefaultHandler(Message);
end;

function DoControlMsg(ControlHandle: HWND; var Message): Boolean;
var
  Control: TWinControl;
{$IF DEFINED(CLR)}
  Msg: TMessage;
{$IFEND}
begin
  DoControlMsg := False;
  Control := FindControl(ControlHandle);
  if Control <> nil then
  begin
{$IF DEFINED(CLR)}
    Msg := UnwrapMessage(TObject(Message));
    with Msg do
{$ELSE}
    with TMessage(Message) do
{$IFEND}
    begin
      Result := Control.Perform(Msg + CN_BASE, WParam, LParam);
      DoControlMsg := True;
    end;
  end;
end;

procedure TWinControl.PaintHandler(var Message: TWMPaint);
var
  I, Clip, SaveIndex: Integer;
  DC: HDC;
  PS: TPaintStruct;
begin
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  try
    if FControls = nil then PaintWindow(DC) else
    begin
      SaveIndex := SaveDC(DC);
      try
        Clip := SimpleRegion;
        for I := 0 to FControls.Count - 1 do
          with TControl(FControls[I]) do
            if (Visible and (not (csDesigning in ComponentState) or not (csDesignerHide in ControlState)) or
              ((csDesigning in ComponentState) and not (csDesignerHide in ControlState)) and
              not (csNoDesignVisible in ControlStyle)) and
              (csOpaque in ControlStyle) then
            begin
              Clip := ExcludeClipRect(DC, Left, Top, Left + Width, Top + Height);
              if Clip = NullRegion then Break;
            end;
        if Clip <> NullRegion then PaintWindow(DC);
      finally
        RestoreDC(DC, SaveIndex);
      end;
    end;
    PaintControls(DC, nil);
  finally
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

procedure TWinControl.PaintWindow(DC: HDC);
const
  PaintMessage: array[Boolean] of Cardinal = (WM_PAINT, WM_PRINTCLIENT);
var
  Message: TMessage;
begin
{$IF DEFINED(CLR)}
  Message := TMessage.Create(PaintMessage[csPrintClient in ControlState], DC, 0);
{$ELSE}
  Message.Msg := PaintMessage[csPrintClient in ControlState];
  Message.WParam := DC;
  Message.LParam := 0;
  Message.Result := 0;
{$IFEND}
  DefaultHandler(Message);
end;

procedure TWinControl.PaintControls(DC: HDC; First: TControl);
var
  I, Count, SaveIndex: Integer;
  FrameBrush: HBRUSH;
begin
  if DockSite and UseDockManager and (DockManager <> nil) then
    DockManager.PaintSite(DC);
  if FControls <> nil then
  begin
    I := 0;
    if First <> nil then
    begin
      I := FControls.IndexOf(First);
      if I < 0 then I := 0;
    end;
    Count := FControls.Count;
    while I < Count do
    begin
      with TControl(FControls[I]) do
        if (Visible and (not (csDesigning in ComponentState) or not (csDesignerHide in ControlState)) or
          ((csDesigning in ComponentState) and not (csDesignerHide in ControlState)) and
          not (csNoDesignVisible in ControlStyle)) and
          RectVisible(DC, System.Types.Rect(Left, Top, Left + Width,
            Top + Height)) then
        begin
          if csPaintCopy in Self.ControlState then
            Include(FControlState, csPaintCopy);
          SaveIndex := SaveDC(DC);
          try
            MoveWindowOrg(DC, Left, Top);
            IntersectClipRect(DC, 0, 0, Width, Height);
            Perform(WM_PAINT, DC, 0);
          finally
            RestoreDC(DC, SaveIndex);
          end;
          Exclude(FControlState, csPaintCopy);
        end;
      Inc(I);
    end;
  end;
  if FWinControls <> nil then
    for I := 0 to FWinControls.Count - 1 do
      with TWinControl(FWinControls[I]) do
        if FCtl3D and (csFramed in ControlStyle) and
          (((not (csDesigning in ComponentState)) and Visible) or
          ((csDesigning in ComponentState) and
          not (csNoDesignVisible in ControlStyle) and not (csDesignerHide in ControlState))) then
        begin
          FrameBrush := CreateSolidBrush(ColorToRGB(clBtnShadow));
          FrameRect(DC, System.Types.Rect(Left - 1, Top - 1,
            Left + Width, Top + Height), FrameBrush);
          DeleteObject(FrameBrush);
          FrameBrush := CreateSolidBrush(ColorToRGB(clBtnHighlight));
          FrameRect(DC, System.Types.Rect(Left, Top, Left + Width + 1,
            Top + Height + 1), FrameBrush);
          DeleteObject(FrameBrush);
        end;
end;

procedure TWinControl.PaintTo(Canvas: TCanvas; X, Y: Integer);
begin
  Canvas.Lock;
  try
    PaintTo(Canvas.Handle, X, Y);
  finally
    Canvas.Unlock;
  end;
end;

procedure TWinControl.PaintTo(DC: HDC; X, Y: Integer);

  procedure DrawThemeEdge(DC: HDC; var DrawRect: TRect);
  var
    Details: TThemedElementDetails;
    Save: Integer;
  begin
    Save := SaveDC(DC);
    try
      with DrawRect do
        ExcludeClipRect(DC, Left + 2, Top + 2, Right - 2, Bottom - 2);
      Details := StyleServices.GetElementDetails(teEditTextNormal);
      StyleServices.DrawElement(DC, Details, DrawRect);
    finally
      RestoreDC(DC, Save);
    end;
    InflateRect(DrawRect, -2, -2);
  end;

var
  I, EdgeFlags, BorderFlags, SaveIndex: Integer;
  R: TRect;
begin
  Include(FControlState, csPaintCopy);
  SaveIndex := SaveDC(DC);
  try
    MoveWindowOrg(DC, X, Y);
    IntersectClipRect(DC, 0, 0, Width, Height);
    BorderFlags := 0;
    EdgeFlags := 0;
    if GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_CLIENTEDGE <> 0 then
    begin
      EdgeFlags := EDGE_SUNKEN;
      BorderFlags := BF_RECT or BF_ADJUST
    end else
    if GetWindowLong(Handle, GWL_STYLE) and WS_BORDER <> 0 then
    begin
      EdgeFlags := BDR_OUTER;
      BorderFlags := BF_RECT or BF_ADJUST or BF_MONO;
    end;
    if (EdgeFlags = EDGE_SUNKEN) and StyleServices.Enabled and
      not ((csDesigning in ComponentState) and UnthemedDesigner(Self)) then
    begin
      // Paint borders themed.
      SetRect(R, 0, 0, Width, Height);
      if csNeedsBorderPaint in ControlStyle then
        DrawThemeEdge(DC, R)
      else
      begin
        ControlStyle := ControlStyle + [csNeedsBorderPaint];
        DrawThemeEdge(DC, R);
        ControlStyle := ControlStyle - [csNeedsBorderPaint];
      end;
      MoveWindowOrg(DC, R.Left, R.Top);
      IntersectClipRect(DC, 0, 0, R.Right - R.Left, R.Bottom - R.Top);
    end
    else if BorderFlags <> 0 then
    begin
      SetRect(R, 0, 0, Width, Height);
      DrawEdge(DC, R, EdgeFlags, BorderFlags);
      MoveWindowOrg(DC, R.Left, R.Top);
      IntersectClipRect(DC, 0, 0, R.Right - R.Left, R.Bottom - R.Top);
    end;

    Perform(WM_ERASEBKGND, DC, 0);
    Perform(WM_PAINT, DC, 0);
    if FWinControls <> nil then
      for I := 0 to FWinControls.Count - 1 do
        with TWinControl(FWinControls[I]) do
          if Visible then
            PaintTo(DC, Left, Top);
  finally
    RestoreDC(DC, SaveIndex);
  end;
  Exclude(FControlState, csPaintCopy);
end;

procedure TWinControl.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  PaintBuffer: HPAINTBUFFER;
begin
  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    if DwmCompositionEnabled then
    begin
      DC := BeginPaint(Handle, PS);
      try
        PaintBuffer := BeginBufferedPaint(DC, PS.rcPaint, BPBF_COMPOSITED, nil, MemDC);
        if PaintBuffer <> 0 then
          try
            Perform(WM_ERASEBKGND, MemDC, MemDC);
            Perform(WM_PRINTCLIENT, MemDC, PRF_CLIENT);
           if not (csPaintBlackOpaqueOnGlass in FControlStyle) then
              BufferedPaintMakeOpaque(PaintBuffer, PS.rcPaint);
          finally
            EndBufferedPaint(PaintBuffer, True);
          end;
      finally
        EndPaint(Handle, PS);
      end;
    end
    else
    begin
      DC := BeginPaint(Handle, PS);
      MemBitmap := CreateCompatibleBitmap(DC, PS.rcPaint.Right - PS.rcPaint.Left,
        PS.rcPaint.Bottom - PS.rcPaint.Top);
      try
        MemDC := CreateCompatibleDC(DC);
        OldBitmap := SelectObject(MemDC, MemBitmap);
        try
          SetWindowOrgEx(MemDC, PS.rcPaint.Left, PS.rcPaint.Top, nil);
          Perform(WM_ERASEBKGND, MemDC, MemDC);
          Message.DC := MemDC;
          if TStyleManager.IsCustomStyleActive then
            WndProc(TMessage(Message))
          else
            WMPaint(Message);
          Message.DC := 0;
          BitBlt(DC, PS.rcPaint.Left, PS.rcPaint.Top,
            PS.rcPaint.Right - PS.rcPaint.Left,
            PS.rcPaint.Bottom - PS.rcPaint.Top,
            MemDC,
            PS.rcPaint.Left, PS.rcPaint.Top,
            SRCCOPY);
        finally
          SelectObject(MemDC, OldBitmap);
        end;
      finally
        EndPaint(Handle, PS);
        DeleteDC(MemDC);
        DeleteObject(MemBitmap);
      end;
    end;
  end;
end;

procedure TWinControl.WMCommand(var Message: TWMCommand);
begin
  if not DoControlMsg(Message.Ctl, Message) then inherited;
end;

procedure TWinControl.WMNotify(var Message: TWMNotify);
begin
  if not DoControlMsg(Message.NMHdr.hWndFrom, Message) then inherited;
end;

procedure TWinControl.WMSysColorChange(var Message: TWMSysColorChange);
begin
  Vcl.Graphics.PaletteChanged;
  Perform(CM_SYSCOLORCHANGE, 0, 0);
end;

procedure TWinControl.WMWinIniChange(var Message: TMessage);
begin
  Perform(CM_WININICHANGE, Message.wParam, Message.lParam);
end;

procedure TWinControl.WMFontChange(var Message: TMessage);
begin
  Perform(CM_FONTCHANGE, 0, 0);
end;

procedure TWinControl.WMTimeChange(var Message: TMessage);
begin
  Perform(CM_TIMECHANGE, 0, 0);
end;

procedure TWinControl.WMHScroll(var Message: TWMHScroll);
begin
  if not DoControlMsg(Message.ScrollBar, Message) then inherited;
end;

procedure TWinControl.WMVScroll(var Message: TWMVScroll);
begin
  if not DoControlMsg(Message.ScrollBar, Message) then inherited;
end;

procedure TWinControl.WMCompareItem(var Message: TWMCompareItem);
begin
  if not DoControlMsg(Message.CompareItemStruct.hwndItem, Message) then inherited;
end;

procedure TWinControl.WMDeleteItem(var Message: TWMDeleteItem);
begin
  if not DoControlMsg(Message.DeleteItemStruct.hwndItem, Message) then inherited;
end;

procedure TWinControl.WMDrawItem(var Message: TWMDrawItem);
begin
  if not DoControlMsg(Message.DrawItemStruct.hwndItem, Message) then inherited;
end;

procedure TWinControl.WMMeasureItem(var Message: TWMMeasureItem);
begin
  if not DoControlMsg(Message.IDCtl, Message) then inherited;
end;

procedure TWinControl.WMMouseActivate(var Message: TWMMouseActivate);
begin
  if IsControlActivateMsg(Message, Self) then
  begin
    if (Message.Result = 0) and HandleAllocated then
      inherited;
  end;
end;

procedure TWinControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if StyleServices.Enabled and Assigned(Parent) and (csParentBackground in FControlStyle) then
  begin
    { Get the parent to draw its background into the control's background. }
    if Parent.DoubleBuffered then
      PerformEraseBackground(Self, Message.DC)
    else
      StyleServices.DrawParentBackground(Handle, Message.DC, nil, False);
  end
  else
  begin
    { Only erase background if we're not doublebuffering or painting to memory. }
    if not FDoubleBuffered or
{$IF DEFINED(CLR)}
       (Message.OriginalMessage.WParam = Message.OriginalMessage.LParam) then
{$ELSE}
       (TMessage(Message).wParam = WPARAM(TMessage(Message).lParam)) then
{$IFEND}
      FillRect(Message.DC, ClientRect, FBrush.Handle);
  end;
  Message.Result := 1;
end;

procedure TWinControl.WMWindowPosChanged(var Message: TWMWindowPosChanged);
var
  Framed, Moved, Sized: Boolean;
{$IF DEFINED(CLR)}
  WindowPos: TWindowPos;
{$ELSE}
  WindowPos: PWindowPos;
{$IFEND}
begin
  WindowPos := Message.WindowPos;
  Framed := FCtl3D and (csFramed in ControlStyle) and (Parent <> nil) and
    (WindowPos.flags and SWP_NOREDRAW = 0);
  Moved := (WindowPos.flags and SWP_NOMOVE = 0) and
    IsWindowVisible(WindowHandle);
  Sized := (WindowPos.flags and SWP_NOSIZE = 0) and
    IsWindowVisible(WindowHandle);
  if Framed and (Moved or Sized) then
    InvalidateFrame;
  if not (csDestroyingHandle in ControlState) then
    UpdateBounds;
  inherited;
  if Framed and ((Moved or Sized) or (WindowPos.flags and
    (SWP_SHOWWINDOW or SWP_HIDEWINDOW) <> 0)) then
    InvalidateFrame;
end;

procedure TWinControl.WMWindowPosChanging(var Message: TWMWindowPosChanging);
var
{$IF DEFINED(CLR)}
  WindowPos: TWindowPos;
{$ELSE}
  WindowPos: PWindowPos;
{$IFEND}
begin
  if ComponentState * [csReading, csDestroying] = [] then
  begin
    WindowPos := Message.WindowPos;
    with WindowPos{$IFNDEF CLR}^{$ENDIF} do
      if (flags and SWP_NOSIZE = 0) and not CheckNewSize(cx, cy) then
        flags := flags or SWP_NOSIZE;
{$IF DEFINED(CLR)}
    Message.WindowPos := WindowPos;
{$IFEND}
  end;
  inherited;
end;

procedure TWinControl.WMSize(var Message: TWMSize);
var
  LList: TList;
begin
  UpdateBounds;
  UpdateExplicitBounds;
  inherited;

  LList := nil;
  if (Parent <> nil) and (Parent.FAlignControlList <> nil) then
    LList := Parent.FAlignControlList
  else if FAlignControlList <> nil then
    LList := FAlignControlList;

  if LList <> nil then
  begin
    if LList.IndexOf(Self) = -1 then
      LList.Add(Self);
  end
  else
  begin
    Realign;
    if not (csLoading in ComponentState) then
      Resize;
  end;
end;

procedure TWinControl.WMMove(var Message: TWMMove);
begin
  inherited;
  UpdateBounds;
  UpdateExplicitBounds;
end;

procedure TWinControl.WMSetCursor(var Message: TWMSetCursor);
var
  Cursor: TCursor;
  Control: TControl;
  P: TPoint;
begin
  with Message do
    if CursorWnd = WindowHandle then
      case HitTest of
        HTCLIENT:
          begin
            Cursor := Screen.Cursor;
            if Cursor = crDefault then
            begin
              GetCursorPos(P);
              Control := ControlAtPos(ScreenToClient(P), False);
              if (Control <> nil) then
                if csDesigning in Control.ComponentState then
                  Cursor := crArrow
                else
                  Cursor := Control.FCursor;
              if Cursor = crDefault then
                if csDesigning in ComponentState then
                  Cursor := crArrow
                else
                  Cursor := FCursor;
            end;
            if Cursor <> crDefault then
            begin
              Winapi.Windows.SetCursor(Screen.Cursors[Cursor]);
              Result := 1;
              Exit;
            end;
          end;
        HTERROR:
          if (MouseMsg = WM_LBUTTONDOWN) and (Application.Handle <> 0) and
            (GetForegroundWindow <> GetLastActivePopup(Application.Handle)) then
            Application.BringToFront;
      end;
  inherited;
end;

procedure TWinControl.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  SetIme;
  InvalidateDockHostSite(False);
end;

procedure TWinControl.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  ResetIme;
  InvalidateDockHostSite(True);
end;

procedure TWinControl.WMIMEStartComp(var Message: TMessage);
begin
  FInImeComposition := True;
  inherited;
end;

procedure TWinControl.WMInputLangChange(var Message: TMessage);
var
  LParent: TWinControl;
begin
  if Application <> nil then
  begin
    with Message do
      PostMessage(Application.Handle, CM_INPUTLANGCHANGE, WParam, LParam);
  end
  else
  begin
    LParent := Parent;
    while LParent <> nil do
    begin
      if LParent.Parent = nil then
        Break;
      LParent := LParent.Parent;
    end;
    if LParent <> nil then
      with Message do
        LParent.Perform(CM_INPUTLANGCHANGE, WParam, LParam);
  end;
end;

procedure TWinControl.WMIMEEndComp(var Message: TMessage);
begin
  FInImeComposition := False;
  inherited;
end;

function TWinControl.SetImeCompositionWindow(Font: TFont;
  XPos, YPos: Integer): Boolean;
var
  H: HIMC;
  CForm: TCompositionForm;
  LFont: TLogFont;
begin
  Result := False;
  H := Imm32GetContext(Handle);
  if H <> 0 then
  begin
    with CForm do
    begin
      dwStyle := CFS_POINT;
      ptCurrentPos.x := XPos;
      ptCurrentPos.y := YPos;
    end;
    Imm32SetCompositionWindow(H, CForm);
    if Assigned(Font) then
    begin
{$IF DEFINED(CLR)}
      GetObject(Font.Handle, Marshal.SizeOf(TypeOf(TLogFont)), LFont);
{$ELSE}
      GetObject(Font.Handle, SizeOf(TLogFont), @LFont);
{$IFEND}
      Imm32SetCompositionFont(H, LFont);
    end;
    Imm32ReleaseContext(Handle, H);
{$IFNDEF LINUX}
    Result := True;
{$ELSE}
    // By current implementation of WINE, HideCaret/ShowCaret controls XIM with
    // caret. So, we returns False always, for to avoid disabling XIM with
    // caret at caller side. - kna
    //Result := True;
{$ENDIF}
  end;
end;

function TWinControl.ResetImeComposition(Action: DWORD): Boolean;
var
  H: HIMC;
begin
  Result := False;
  if FInImeComposition then
  begin
    H := Imm32GetContext(Handle);
    if H <> 0 then
    begin
      Result := Imm32NotifyIME(H, NI_COMPOSITIONSTR, Action, 0);
      Imm32ReleaseContext(Handle, H);
    end;
  end;
end;

procedure TWinControl.RequestAlign;
var
  ParentListAssigned: Boolean;
begin
  if Parent <> nil then
  begin
    ParentListAssigned := False;
    if (FAlignControlList <> nil) and (Parent.FAlignControlList = nil) then
    begin
      Parent.FAlignControlList := FAlignControlList;
      ParentListAssigned := True;
    end;

    try
      inherited RequestAlign;
    finally
      if ParentListAssigned then
        Parent.FAlignControlList := nil;
    end;
  end;
end;

procedure TWinControl.SetIme;
var
  I: Integer;
  HandleToSet: HKL;
begin
  if not SysLocale.FarEast then Exit;
  if FImeName <> '' then
  begin
    if (AnsiCompareText(FImeName, Screen.DefaultIme) <> 0) and (Screen.Imes.Count <> 0) then
    begin
      HandleToSet := Screen.DefaultKbLayout;
      if FImeMode <> imDisable then
      begin
        I := Screen.Imes.IndexOf(FImeName);
        if I >= 0 then
          HandleToSet := HKL(Screen.Imes.Objects[I]);
      end;
      ActivateKeyboardLayout(HandleToSet, KLF_ACTIVATE);
    end;
  end;
  SetImeMode(Handle, FImeMode);
end;

procedure TWinControl.ResetIme;
begin
  if not SysLocale.FarEast then Exit;
  if FImeName <> '' then
  begin
    if AnsiCompareText(FImeName, Screen.DefaultIme) <> 0 then
      ActivateKeyboardLayout(Screen.DefaultKbLayout, KLF_ACTIVATE);
  end;
  if FImeMode = imDisable then Win32NLSEnableIME(Handle, TRUE);
end;

procedure TWinControl.DoAddDockClient(Client: TControl; const ARect: TRect);
begin
  Client.Parent := Self;
end;

procedure TWinControl.DoRemoveDockClient(Client: TControl);
begin
  // do nothing by default
end;

procedure TWinControl.DoEnter;
begin
  if Assigned(FOnEnter) then FOnEnter(Self);
end;

procedure TWinControl.DoExit;
begin
  if Assigned(FOnExit) then FOnExit(Self);
end;

{$IF DEFINED(CLR)}
procedure TWinControl.DoMouseEnter;
begin
  FParent.Perform(CM_MOUSEENTER, meTWinControl, FParent.FWinControls.IndexOf(Self));
end;

procedure TWinControl.DoMouseLeave;
begin
  FParent.Perform(CM_MOUSELEAVE, meTWinControl, FParent.FWinControls.IndexOf(Self));
end;
{$IFEND}

procedure TWinControl.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
{$IF DEFINED(CLR)}
  if (DockClient(Source, Point(X, Y)) >= 0) and
{$ELSE}

  if (Perform(CM_DOCKCLIENT, Winapi.Windows.WPARAM(Source), Winapi.Windows.LPARAM(MakeLParam(X, Y))) >= 0) and
  //if (Perform(CM_DOCKCLIENT, Winapi.Windows.WPARAM(Source), Winapi.Windows.LPARAM(SmallPoint(X, Y))) >= 0) and
{$IFEND}
      Assigned(FOnDockDrop) then
    FOnDockDrop(Self, Source, X, Y);
end;

procedure TWinControl.DoDockOver(Source: TDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Assigned(FOnDockOver) then
    FOnDockOver(Self, Source, X, Y, State, Accept);
end;

procedure TWinControl.DockOver(Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  PositionDockRect(Source);
  DoDockOver(Source, X, Y, State, Accept);
end;

function TWinControl.DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean;
begin
  Result := True;
  if Assigned(FOnUnDock) then FOnUnDock(Self, Client, NewTarget, Result);
{$IF DEFINED(CLR)}
  if Result then
    Result := UndockClient(NewTarget, Client);
{$ELSE}
  Result := Result and (Perform(CM_UNDOCKCLIENT, Winapi.Windows.WPARAM(NewTarget), Winapi.Windows.LPARAM(Client)) = 0);
{$IFEND}
end;

procedure TWinControl.ReloadDockedControl(const AControlName: string;
  var AControl: TControl);
begin
  AControl := Owner.FindComponent(AControlName) as TControl;
end;

function TWinControl.GetDockClientCount: Integer;
begin
  if FDockClients <> nil then Result := FDockClients.Count
  else Result := 0;
end;

function TWinControl.GetDockClients(Index: Integer): TControl;
begin
  if FDockClients <> nil then Result := TControl(FDockClients[Index])
  else Result := nil;
end;

procedure TWinControl.GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
  MousePos: TPoint; var CanDock: Boolean);
const
  DefExpandoRect = 10;
begin
  GetWindowRect(Handle, InfluenceRect);
  InflateRect(InfluenceRect, DefExpandoRect, DefExpandoRect);
  if Assigned(FOnGetSiteInfo) then
    FOnGetSiteInfo(Self, Client, InfluenceRect, MousePos, CanDock);
end;

function TWinControl.GetVisibleDockClientCount: Integer;
var
  I: Integer;
begin
  Result := GetDockClientCount;
  if Result > 0 then
    for I := Result - 1 downto 0 do
      if not TControl(FDockClients[I]).Visible then Dec(Result);
end;

procedure TWinControl.ControlsAligned;
begin
  { Notification }
end;

function TWinControl.CreateDockManager: IDockManager;
begin
  if (FDockManager = nil) and DockSite and UseDockManager then
    Result := DefaultDockTreeClass.Create(Self)
  else
    Result := FDockManager;
  DoubleBuffered := DoubleBuffered or (Result <> nil);
end;

procedure TWinControl.SetDesignVisible(Value: Boolean);
begin
  if (csDesigning in ComponentState) and (Value <> not (csDesignerHide in ControlState)) then
  begin
    if not Value then
      Include(FControlState, csDesignerHide)
    else
      Exclude(FControlState, csDesignerHide);
    UpdateShowing;
  end;
end;

procedure TWinControl.SetDockSite(Value: Boolean);
begin
  if Value <> FDockSite then
  begin
    FDockSite := Value;
    if not (csDesigning in ComponentState) then
    begin
      RegisterDockSite(Self, Value);
      if not Value then
      begin
        FDockClients.Free;
        FDockClients := nil;
        FDockManager := nil;
      end
      else
      begin
        if FDockClients = nil then
          FDockClients := TList.Create;
        FDockManager := CreateDockManager;
      end;
    end;
  end;
end;

procedure TWinControl.SetDoubleBuffered(Value: Boolean);
begin
  if Value <> FDoubleBuffered then
  begin
    FDoubleBuffered := Value;
    FParentDoubleBuffered := False;
    Perform(CM_DOUBLEBUFFEREDCHANGED, 0, 0);
  end;
end;

{$IF DEFINED(CLR)}
function TWinControl.DockClient(DockSource: TDragDockObject; MousePos: TPoint): Integer;
{$ELSE}
procedure TWinControl.CMDockClient(var Message: TCMDockClient);
{$IFEND}
var
  DestRect: TRect;
  Form: TCustomForm;
begin
{$IF NOT DEFINED(CLR)}
  if Message.Result = 0 then
  with Message do
{$IFEND}
  begin
    { Map DockRect to dock site's client coordinates }
    DestRect := DockSource.DockRect;
    MapWindowPoints(0, Handle, DestRect, 2);
    DisableAlign;
    try
      DockSource.Control.Dock(Self, DestRect);
      if FUseDockManager and (FDockManager <> nil) then
        FDockManager.InsertControl(DockSource.Control,
            DockSource.DropAlign, DockSource.DropOnControl);
    finally
      EnableAlign;
    end;
    Form := GetParentForm(Self);
    if Form <> nil then Form.BringToFront;
    Result := 1;
  end;
end;

{$IF DEFINED(CLR)}
function TWinControl.UndockClient(NewTarget, Client: TControl): Boolean;
begin
  Result := True;
  if FUseDockManager and (FDockManager <> nil) then
    FDockManager.RemoveControl(Client)
end;

procedure TWinControl.FloatControl(DockSource: TDragDockObject);
var
  WasVisible: Boolean;
begin
  if FloatingDockSiteClass = ClassType then
  begin
    WasVisible := Visible;
    try
      Dock(nil, DockSource.FDockRect);
    finally
      if WasVisible then BringToFront;
    end;
  end
  else
    inherited;
end;
{$ELSE}
procedure TWinControl.CMUnDockClient(var Message: TCMUnDockClient);
begin
  with Message do
  begin
    Result := 0;
    if FUseDockManager and (FDockManager <> nil) then
      FDockManager.RemoveControl(Client)
  end;
end;

procedure TWinControl.CMFloat(var Message: TCMFloat);
var
  WasVisible: Boolean;
begin
  if (FloatingDockSiteClass = ClassType) then
  begin
    WasVisible := Visible;
    try
      Dock(nil, Message.DockSource.FDockRect);
    finally
      if WasVisible then BringToFront;
    end;
  end
  else
    inherited;
end;
{$IFEND}

procedure TWinControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then FOnKeyDown(Self, Key, Shift);
end;

function TWinControl.DoKeyDown(var Message: TWMKey): Boolean;
var
  ShiftState: TShiftState;
  Form, FormParent: TCustomForm;
  LCharCode: Word;
begin
  Result := True;
  { First give the immediate parent form a try at the Message }
  Form := GetParentForm(Self, False);
  if (Form <> nil) and (Form <> Self) then
  begin
    if Form.KeyPreview and TWinControl(Form).DoKeyDown(Message) then
      Exit;
    { If that didn't work, see if that Form has a parent (ie: it is docked) }
    if Form.Parent <> nil then
    begin
      FormParent := GetParentForm(Form);
      if (FormParent <> nil) and (FormParent <> Form) and
      FormParent.KeyPreview and TWinControl(FormParent).DoKeyDown(Message) then
        Exit;
    end;
  end;
  with Message do
  begin
    ShiftState := KeyDataToShiftState(KeyData);
    if not (csNoStdEvents in ControlStyle) then
    begin
      LCharCode := CharCode;
      KeyDown(LCharCode, ShiftState);
      CharCode := LCharCode;
      if LCharCode = 0 then Exit;
    end;
  end;
  Result := False;
end;

procedure TWinControl.WMKeyDown(var Message: TWMKeyDown);
begin
  if not DoKeyDown(Message) then inherited;
  UpdateUIState(Message.CharCode);
end;

procedure TWinControl.WMSysKeyDown(var Message: TWMSysKeyDown);
begin
  if not DoKeyDown(Message) then inherited;
  UpdateUIState(Message.CharCode);
end;

procedure TWinControl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then FOnKeyUp(Self, Key, Shift);
end;

function TWinControl.DoKeyUp(var Message: TWMKey): Boolean;
var
  ShiftState: TShiftState;
  Form, FormParent: TCustomForm;
  LCharCode: Word;
begin
  Result := True;
  { First give the immediate parent form a try at the Message }
  Form := GetParentForm(Self, False);
  if (Form <> nil) and (Form <> Self) then
  begin
    if Form.KeyPreview and TWinControl(Form).DoKeyUp(Message) then
      Exit;
    { If that didn't work, see if that Form has a parent (ie: it is docked) }
     if Form.Parent <> nil then
     begin
       FormParent := GetParentForm(Form);
       if (FormParent <> nil) and (FormParent <> Form) and
       FormParent.KeyPreview and TWinControl(FormParent).DoKeyUp(Message) then
         Exit;
     end;
  end;
  with Message do
  begin
    ShiftState := KeyDataToShiftState(KeyData);
    if not (csNoStdEvents in ControlStyle) then
    begin
      LCharCode := CharCode;
      KeyUp(LCharCode, ShiftState);
      CharCode := LCharCode;
      if LCharCode = 0 then Exit;
    end;
  end;
  Result := False;
end;

procedure TWinControl.WMKeyUp(var Message: TWMKeyUp);
begin
  if not DoKeyUp(Message) then inherited;
end;

procedure TWinControl.WMSysKeyUp(var Message: TWMSysKeyUp);
begin
  if not DoKeyUp(Message) then inherited;
end;

procedure TWinControl.KeyPress(var Key: Char);
begin
  if Assigned(FOnKeyPress) then FOnKeyPress(Self, Key);
end;

function TWinControl.DoKeyPress(var Message: TWMKey): Boolean;
var
  Form: TCustomForm;
  Ch: Char;
begin
  Result := True;
  Form := GetParentForm(Self);
  if (Form <> nil) and (Form <> Self) and Form.KeyPreview and
    TWinControl(Form).DoKeyPress(Message) then Exit;
  if not (csNoStdEvents in ControlStyle) then
    with Message do
    begin
      Ch := Char(CharCode);
      KeyPress(Ch);
      CharCode := Word(Ch);
      if Char(CharCode) = #0 then Exit;
    end;
  Result := False;
end;

procedure TWinControl.WMChar(var Message: TWMChar);

{$IF DEFINED(CLR)}
procedure ProcessWMCharA;
  var
    Msg: TWMChar;
    LAnsiStr: AnsiString;
    LWideStr: WideString;
  begin
    SetLength(LAnsiStr, 1);
    LAnsiStr[1] := AnsiChar(Message.CharCode);
    LWideStr := WideString(LAnsiStr);
    Msg := TWMChar.Create(TMessage.Create(Message));
    Msg.OriginalMessage.WParamLo := Word(LWideStr[1]);
    if not DoKeyPress(Msg) then
    begin
      SetLength(LWideStr, 1);
      LWideStr[1] := Char(Msg.CharCode);
      LAnsiStr := AnsiString(LWideStr);
      if Length(LAnsiStr) = 2 then
        PostMessage(WindowHandle, WM_CHAR, Word(LAnsiStr[2]), 0);
      CallWindowProc(FDefWndProc, WindowHandle, WM_CHAR, Byte(LAnsiStr[1]), 0);
    end;
  end;
{$IFEND}

begin
{$IF DEFINED(CLR)}
  if (Marshal.SystemDefaultCharSize = 1) then
    ProcessWMCharA
  else
{$IFEND}
    if not DoKeyPress(Message) then
      inherited;
end;

{$IF DEFINED(CLR)}
procedure TWinControl.WMIMEChar(var Message: TWMChar);

  procedure ProcessWMIMECharA;
  var
    Msg: TWMChar;
    LAnsiStr: AnsiString;
    LWideStr: WideString;
  begin
    if Message.CharCode < $100 then
    begin
      SetLength(LAnsiStr, 1);
      LAnsiStr[1] := AnsiChar((Message.CharCode and $00FF));
    end
    else
    begin
      SetLength(LAnsiStr, 2);
      LAnsiStr[1] := AnsiChar((Message.CharCode and $FF00) shr 8);
      LAnsiStr[2] := AnsiChar((Message.CharCode and $00FF));
    end;
    LWideStr := WideString(LAnsiStr);
    if Length(LWideStr) >= 1 then
    begin
      Msg := TWMChar.Create(TMessage.Create(Message));
      Msg.OriginalMessage.WParamLo := Word(LWideStr[1]);
      if not DoKeyPress(Msg) then
      begin
        SetLength(LWideStr, 1);
        LWideStr[1] := Char(Msg.CharCode);
        LAnsiStr := AnsiString(LWideStr);
        if Length(LAnsiStr) = 2 then
          PostMessage(WindowHandle, WM_CHAR, Word(LAnsiStr[2]), 0);
        CallWindowProc(FDefWndProc, WindowHandle, WM_CHAR, Byte(LAnsiStr[1]), 0);
      end;
    end;
  end;

begin
  if (Marshal.SystemDefaultCharSize = 1) then
    ProcessWMIMECharA
  else
    inherited;
end;
{$IFEND}

procedure TWinControl.WMSysCommand(var Message: TWMSysCommand);
var
  Form: TCustomForm;

  function TraverseControls(Container: TWinControl): Boolean;
  var
    I: Integer;
    Control: TControl;
  begin
    Result := False;
    if Container.Showing then
      for I := 0 to Container.ControlCount - 1 do
      begin
        Control := Container.Controls[I];
        if Control.Visible and Control.Enabled then
        begin
          if (csMenuEvents in Control.ControlStyle) and
{$IF DEFINED(CLR)}
            (Control.Perform(WM_SYSCOMMAND, Message.OriginalMessage.WParam,
              Message.OriginalMessage.LParam) <> 0) or (Control is TWinControl) and
{$ELSE}
            (Control.Perform(WM_SYSCOMMAND, TMessage(Message).WParam,
              TMessage(Message).LParam) <> 0) or (Control is TWinControl) and
{$IFEND}
            TraverseControls(TWinControl(Control)) then
          begin
            Result := True;
            Exit;
          end;
        end;
      end;
  end;

begin
  with Message do
  begin
    if (CmdType and $FFF0 = SC_KEYMENU) and (Key <> VK_SPACE) and
      (Key <> Word('-')) and not IsIconic(WindowHandle) and (GetCapture = 0) and
      (Application.MainForm <> Self) then
    begin
      Form := GetParentForm(Self);
      if (Form <> nil) and
{$IF DEFINED(CLR)}
        (Form.Perform(CM_APPSYSCOMMAND, OriginalMessage.WParam, OriginalMessage.LParam) <> 0) then
{$ELSE}
        (Form.Perform(CM_APPSYSCOMMAND, 0, Winapi.Windows.LPARAM(@Message)) <> 0) then
{$IFEND}
        Exit;
    end;
    { Broadcast WMSysCommand to all controls which have a csMenuEvents style. }
    if (CmdType and $FFF0 = SC_KEYMENU) and TraverseControls(Self) then
      Exit;
  end;
  inherited;
end;

procedure TWinControl.WMCharToItem(var Message: TWMCharToItem);
begin
  if not DoControlMsg(Message.ListBox, Message) then inherited;
end;

procedure TWinControl.WMParentNotify(var Message: TWMParentNotify);
begin
  if (Message.Event <> WM_CREATE) and (Message.Event <> WM_DESTROY) or
    not DoControlMsg(Message.ChildWnd, Message) then inherited;
end;

procedure TWinControl.WMVKeyToItem(var Message: TWMVKeyToItem);
begin
  if not DoControlMsg(Message.ListBox, Message) then inherited;
end;

procedure TWinControl.WMDestroy(var Message: TWMDestroy);
{$IF NOT DEFINED(CLR)}
var
  Len: Integer;
{$IFEND}
begin
  if FText = nil then
{$IF DEFINED(CLR)}
    FText := GetText;
{$ELSE}
  begin
    Len := GetTextLen;
    if Len < 1 then
      FText := StrNew('')
    else
    begin
      FText := StrAlloc(Len + 1);
      GetTextBuf(FText, StrBufSize(FText));
    end;
  end;
{$IFEND}
  if ((sfHandleMessages) in TStyleManager.Flags) then
    TStyleManager.Notification(snControlDestroyed, Self);
  inherited;
{$IF NOT DEFINED(CLR)}
  RemoveWindowProps;
{$IFEND}
end;

procedure TWinControl.WMNCDestroy(var Message: TWMNCDestroy);
begin
  inherited;
{$IF DEFINED(CLR)}
  ControlHash[TObject(WindowHandle)] := nil;
{$IFEND}
  WindowHandle := 0;
  FShowing := False;
end;

procedure TWinControl.WMNCHitTest(var Message: TWMNCHitTest);
begin
  with Message do
    if (csDesigning in ComponentState) and (FParent <> nil) then
      Result := HTCLIENT
    else
      inherited;
end;

function TWinControl.PaletteChanged(Foreground: Boolean): Boolean;
var
  I: Integer;
begin
  Result := inherited PaletteChanged(Foreground);
  if Visible then
    for I := ControlCount - 1 downto 0 do
    begin
      if Foreground and Result then Exit;
      Result := Controls[I].PaletteChanged(Foreground) or Result;
    end;
end;

procedure TWinControl.WMQueryNewPalette(var Message: TMessage);
begin
  Include(FControlState, csPalette);
  Message.Result := Winapi.Windows.LRESULT(PaletteChanged(True));
end;

procedure TWinControl.WMPaletteChanged(var Message: TMessage);
begin
  Message.Result := Winapi.Windows.LRESULT(PaletteChanged(False));
end;

procedure TWinControl.CMShowHintChanged(var Message: TMessage);
begin
  inherited;
  NotifyControls(CM_PARENTSHOWHINTCHANGED);
end;

procedure TWinControl.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if (SysLocale.MiddleEast) and (Message.wParam = 0) then RecreateWnd;
  NotifyControls(CM_PARENTBIDIMODECHANGED);
end;

procedure TWinControl.CMDoubleBufferedChanged(var Message: TMessage);
begin
  inherited;
  NotifyControls(CM_PARENTDOUBLEBUFFEREDCHANGED);
  Invalidate;
end;

procedure TWinControl.CMEnter(var Message: TCMEnter);
begin
  if SysLocale.MiddleEast then
    if UseRightToLeftReading then
    begin
      if Application.BiDiKeyboard <> '' then
        LoadKeyboardLayout(Application.BiDiKeyboard, KLF_ACTIVATE);
    end
    else
      if Application.NonBiDiKeyboard <> '' then
        LoadKeyboardLayout(Application.NonBiDiKeyboard, KLF_ACTIVATE);
  DoEnter;
end;

procedure TWinControl.CMExit(var Message: TCMExit);
begin
  DoExit;
end;

procedure TWinControl.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  if not IsControlMouseMsg(Message) then inherited;
end;

procedure TWinControl.CMChanged(var Message: TCMChanged);
{$IF DEFINED(CLR)}
var
  LMessage: TMessage;
{$IFEND}
begin
  if FParent <> nil then
  begin
{$IF DEFINED(CLR)}
    LMessage := UnwrapMessage(Message);
    FParent.WindowProc(LMessage);
{$ELSE}
    FParent.WindowProc(TMessage(Message));
{$IFEND}
  end;
end;

procedure TWinControl.CMChildKey(var Message: TCMChildKey);
{$IF DEFINED(CLR)}
var
  Msg: TMessage;
begin
  Msg := UnwrapMessage(Message);
  if FParent <> nil then FParent.WindowProc(Msg);
  Message.Result := Msg.Result;
end;
{$ELSE}
begin
  if FParent <> nil then FParent.WindowProc(TMessage(Message));
end;
{$IFEND}

procedure TWinControl.CMDialogKey(var Message: TCMDialogKey);
begin
  Broadcast(Message);
end;

procedure TWinControl.CMDialogChar(var Message: TCMDialogChar);
begin
  Broadcast(Message);
end;

{$IF DEFINED(CLR)}
[UIPermission(SecurityAction.InheritanceDemand, Window=UIPermissionWindow.AllWindows),
UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TWinControl.FocusChanged(NewFocusControl: TWinControl);
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TWinControl then
      TWinControl(Controls[I]).FocusChanged(NewFocusControl);
end;
{$ELSE}
procedure TWinControl.CMFocusChanged(var Message: TCMFocusChanged);
begin
  Broadcast(Message);
end;
{$IFEND}

procedure TWinControl.CMVisibleChanged(var Message: TMessage);
begin
  if not FVisible and (Parent <> nil) then RemoveFocus(False);
  if not (csDesigning in ComponentState) or
    (csNoDesignVisible in ControlStyle) then UpdateControlState;
end;

procedure TWinControl.CMShowingChanged(var Message: TMessage);
const
  ShowFlags: array[Boolean] of Word = (
    SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_HIDEWINDOW,
    SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_SHOWWINDOW);
begin
  SetWindowPos(WindowHandle, 0, 0, 0, 0, 0, ShowFlags[FShowing]);
end;

procedure TWinControl.CMEnabledChanged(var Message: TMessage);
begin
  if not Enabled and (Parent <> nil) then RemoveFocus(False);
  if HandleAllocated and not (csDesigning in ComponentState) then
    EnableWindow(WindowHandle, Enabled);
end;

procedure TWinControl.CMColorChanged(var Message: TMessage);
begin
  inherited;
  FBrush.Color := FColor;
  NotifyControls(CM_PARENTCOLORCHANGED);
end;

procedure TWinControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if HandleAllocated then Perform(WM_SETFONT, FFont.Handle, 0);
  NotifyControls(CM_PARENTFONTCHANGED);
end;

procedure TWinControl.CMCursorChanged(var Message: TMessage);
var
  P: TPoint;
begin
  if GetCapture = 0 then
  begin
    GetCursorPos(P);
    if FindDragTarget(P, False) = Self then
      Perform(WM_SETCURSOR, Handle, HTCLIENT);
  end;
end;

procedure TWinControl.CMBorderChanged(var Message: TMessage);
begin
  inherited;
  if HandleAllocated then
  begin
    SetWindowPos(Handle, 0, 0,0,0,0, SWP_NOACTIVATE or
    SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED);
    if Visible then
      Invalidate;
  end;
end;

procedure TWinControl.CMCtl3DChanged(var Message: TMessage);
begin
  if (csFramed in ControlStyle) and (Parent <> nil) and HandleAllocated and
    IsWindowVisible(WindowHandle) then InvalidateFrame;
  NotifyControls(CM_PARENTCTL3DCHANGED);
end;

procedure TWinControl.CMParentCtl3DChanged(var Message: TMessage);
begin
  if FParentCtl3D then
  begin
    if Message.wParam <> 0 then
      SetCtl3D(Message.lParam <> 0) else
      SetCtl3D(FParent.FCtl3D);
    FParentCtl3D := True;
  end;
end;

procedure TWinControl.CMParentDoubleBufferedChanged(var Message: TMessage);
begin
  if FParentDoubleBuffered then
  begin
    if FParent <> nil then
      DoubleBuffered := FParent.DoubleBuffered;
    FParentDoubleBuffered := True;
  end;
end;

procedure TWinControl.CMSysColorChange(var Message: TMessage);
begin
  Broadcast(Message);
end;

procedure TWinControl.CMWinIniChange(var Message: TWMWinIniChange);
begin
  Broadcast(Message);
end;

procedure TWinControl.CMFontChange(var Message: TMessage);
begin
  Broadcast(Message);
end;

procedure TWinControl.CMTabletOptionsChanged(var Message: TMessage);
begin
  NotifyControls(CM_PARENTTABLETOPTIONSCHANGED);
  Invalidate;
end;

procedure TWinControl.CMTimeChange(var Message: TMessage);
begin
  Broadcast(Message);
end;

procedure TWinControl.CMDrag(var Message: TCMDrag);

{$IF DEFINED(CLR)}
var
  Ctrl: TControl;
  LDragRec: TDragRec;
begin
  LDragRec := Message.DragRec;
  case Message.DragMessage of
    dmDragEnter, dmDragLeave, dmDragMove, dmDragDrop:
      if LDragRec.Target <> nil then
        TControl(LDragRec.Target).DoDragMsg(Message);
    dmFindTarget:
      begin
        Ctrl := ControlAtPos(ScreenToClient(LDragRec.Pos), False);
        if not Assigned(Ctrl) then
          Ctrl := Self;
        LDragRec.Target := Ctrl;
        Message.DragRec := LDragRec;
      end;
  end;
{$ELSE}
begin
  with Message, DragRec^ do
    case DragMessage of
      dmDragEnter, dmDragLeave, dmDragMove, dmDragDrop:
        if Target <> nil then TControl(Target).DoDragMsg(Message);
      dmFindTarget:
        begin
          Result := Winapi.Windows.LRESULT(ControlAtPos(ScreenToClient(Pos), False));
          if Result = 0 then Result := Winapi.Windows.LRESULT(Self);
        end;
    end;
{$IFEND}
end;

{$IF DEFINED(CLR)}
procedure TWinControl.ControlListChange(Inserting: Boolean; Child: TControl);
begin
  if FParent <> nil then
    FParent.ControlListChange(Inserting, Child);
end;

procedure TWinControl.ControlListChanging(Inserting: Boolean; Child: TControl; AParent: TWinControl);
begin
  if FParent <> nil then
    FParent.ControlListChanging(Inserting, Child, AParent);
end;

procedure TWinControl.ControlListChanging(Inserting: Boolean; var Item: TControlListItem);
begin
  ControlListChanging(Inserting, Item.Control, Item.Parent);
end;

{$ELSE}
procedure TWinControl.CMControlListChange(var Message: TMessage);
begin
  if FParent <> nil then FParent.WindowProc(Message);
end;

procedure TWinControl.CMControlListChanging(var Message: TMessage);
begin
  if FParent <> nil then FParent.WindowProc(Message);
end;
{$IFEND}

procedure TWinControl.CMSysFontChanged(var Message: TMessage);
begin
  inherited;
  Broadcast(Message);
end;

function TWinControl.IsMenuKey(var Message: TWMKey): Boolean;
var
  Control: TWinControl;
  Form: TCustomForm;
  LocalPopupMenu: TPopupMenu;
begin
  Result := True;
  if not (csDesigning in ComponentState) then
  begin
    Control := Self;
    while Control <> nil do
    begin
      LocalPopupMenu := Control.GetPopupMenu;
      if Assigned(LocalPopupMenu) and (LocalPopupMenu.WindowHandle <> 0) and
        LocalPopupMenu.IsShortCut(Message) then Exit;
      Control := Control.Parent;
    end;
    Form := GetParentForm(Self);
    if (Form <> nil) and Form.IsShortCut(Message) then Exit;
  end;
  with Message do
    if SendAppMessage(CM_APPKEYDOWN, CharCode, KeyData) <> 0 then Exit;
  Result := False;
end;

procedure TWinControl.CNKeyDown(var Message: TWMKeyDown);
var
  Mask: Integer;
begin
  with Message do
  begin
    Result := 1;
    UpdateUIState(Message.CharCode);
    if IsMenuKey(Message) then Exit;
    if not (csDesigning in ComponentState) then
    begin
{$IF DEFINED(CLR)}
      if Perform(CM_CHILDKEY, CharCode, Handle) <> 0 then
{$ELSE}
      if Perform(CM_CHILDKEY, CharCode, Winapi.Windows.LPARAM(Self)) <> 0 then
{$IFEND}
        Exit;
      Mask := 0;
      case CharCode of
        VK_TAB:
          Mask := DLGC_WANTTAB;
        VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN:
          Mask := DLGC_WANTARROWS;
        VK_RETURN, VK_EXECUTE, VK_ESCAPE, VK_CANCEL:
          Mask := DLGC_WANTALLKEYS;
      end;
      if (Mask <> 0) and
        (Perform(CM_WANTSPECIALKEY, CharCode, 0) = 0) and
        (Perform(WM_GETDLGCODE, 0, 0) and Mask = 0) and
        (GetParentForm(Self).Perform(CM_DIALOGKEY,
        CharCode, KeyData) <> 0) then Exit;
    end;
    Result := 0;
  end;
end;

procedure TWinControl.CNKeyUp(var Message: TWMKeyUp);
begin
  if not (csDesigning in ComponentState) then
    with Message do
      case CharCode of
        VK_TAB, VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN,
        VK_RETURN, VK_EXECUTE, VK_ESCAPE, VK_CANCEL:
          Result := Perform(CM_WANTSPECIALKEY, CharCode, 0);
      end;
end;

procedure TWinControl.CNChar(var Message: TWMChar);
begin
  if not (csDesigning in ComponentState) then
    with Message do
    begin
      Result := 1;
      if (Perform(WM_GETDLGCODE, 0, 0) and DLGC_WANTCHARS = 0) and
        (GetParentForm(Self).Perform(CM_DIALOGCHAR,
        CharCode, KeyData) <> 0) then Exit;
      Result := 0;
    end;
end;

procedure TWinControl.CNSysKeyDown(var Message: TWMKeyDown);
begin
  with Message do
  begin
    Result := 1;
    if IsMenuKey(Message) then Exit;
    if not (csDesigning in ComponentState) then
    begin
{$IF DEFINED(CLR)}
      if Perform(CM_CHILDKEY, CharCode, Handle) <> 0 then
{$ELSE}
      if Perform(CM_CHILDKEY, CharCode, Winapi.Windows.LPARAM(Self)) <> 0 then
{$IFEND}
        Exit;
      if GetParentForm(Self).Perform(CM_DIALOGKEY,
        CharCode, KeyData) <> 0 then Exit;
    end;
    Result := 0;
  end;
end;

procedure TWinControl.CNSysChar(var Message: TWMChar);
begin
  if not (csDesigning in ComponentState) then
    with Message do
      if CharCode <> VK_SPACE then
        Result := GetParentForm(Self).Perform(CM_DIALOGCHAR,
          CharCode, KeyData);
end;

procedure TWinControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  WindowPlacement: TWindowPlacement;
begin
  if (ALeft <> FLeft) or (ATop <> FTop) or
    (AWidth <> FWidth) or (AHeight <> FHeight) then
  begin
    if HandleAllocated and not IsIconic(WindowHandle) then
      SetWindowPos(WindowHandle, 0, ALeft, ATop, AWidth, AHeight,
        SWP_NOZORDER + SWP_NOACTIVATE)
    else
    begin
      FLeft := ALeft;
      FTop := ATop;
      FWidth := AWidth;
      FHeight := AHeight;
      if HandleAllocated then
      begin
        WindowPlacement.Length := SizeOf(WindowPlacement);
        GetWindowPlacement(WindowHandle, WindowPlacement);
        WindowPlacement.rcNormalPosition := BoundsRect;
        SetWindowPlacement(WindowHandle, WindowPlacement);
      end;
    end;
    UpdateAnchorRules;
    UpdateExplicitBounds;
    RequestAlign;
  end;
end;

procedure TWinControl.ScaleControls(M, D: Integer);
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
    Controls[I].ChangeScale(M, D);
end;

procedure TWinControl.ScalePadding(M, D: Integer);
begin
  with Padding do
  begin
    if Left > 0 then
      Left := MulDiv(Left, M, D);
    if Top > 0 then
      Top := MulDiv(Top, M, D);
    if Right > 0 then
      Right := MulDiv(Right, M, D);
    if Bottom > 0 then
      Bottom := MulDiv(Bottom, M, D);
  end;
end;

procedure TWinControl.ChangeScale(M, D: Integer);
begin
  DisableAlign;
  try
    ScaleControls(M, D);
    if (M <> D) and (csReading in ComponentState) and
      (sfDesignSize in ScalingFlags) then
    begin
      FDesignSize.X := MulDiv(FDesignSize.X, M, D);
      FDesignSize.Y := MulDiv(FDesignSize.Y, M, D);
    end;
    inherited ChangeScale(M, D);
    ScalePadding(M, D);
  finally
    EnableAlign;
  end;
end;

procedure TWinControl.ScaleBy(M, D: Integer);
const
  SWP_HIDE = SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_HIDEWINDOW;
  SWP_SHOW = SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_SHOWWINDOW;
var
  IsVisible: Boolean;
  R: TRect;
begin
  IsVisible := HandleAllocated and IsWindowVisible(Handle);
  if IsVisible then SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_HIDE);
  R := BoundsRect;
  ChangeScale(M, D);
  SetBounds(R.Left, R.Top, Width, Height);
  if IsVisible then SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_SHOW);
end;

procedure TWinControl.ScrollBy(DeltaX, DeltaY: Integer);
var
  IsVisible: Boolean;
  I: Integer;
  Control: TControl;
begin
  IsVisible := (WindowHandle <> 0) and IsWindowVisible(WindowHandle);
  if IsVisible then ScrollWindow(WindowHandle, DeltaX, DeltaY, nil, nil);
  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    if not (Control is TWinControl) or (TWinControl(Control).WindowHandle = 0) then
    begin
      Inc(Control.FLeft, DeltaX);
      Inc(Control.FTop, DeltaY);
    end else
      if not IsVisible then
        with TWinControl(Control) do
          SetWindowPos(WindowHandle, 0, FLeft + DeltaX, FTop + DeltaY,
            FWidth, FHeight, SWP_NOZORDER + SWP_NOACTIVATE);
  end;
  Realign;
end;

procedure TWinControl.ShowControl(AControl: TControl);
begin
  if Parent <> nil then Parent.ShowControl(Self);
end;

procedure TWinControl.SetZOrderPosition(Position: Integer);
var
  I, Count: Integer;
  Pos: HWND;
begin
  if FParent <> nil then
  begin
    if FParent.FControls <> nil then
      Dec(Position, FParent.FControls.Count);
    I := FParent.FWinControls.IndexOf(Self);
    if I >= 0 then
    begin
      Count := FParent.FWinControls.Count;
      if Position < 0 then Position := 0;
      if Position >= Count then Position := Count - 1;
      if Position <> I then
      begin
        FParent.FWinControls.Delete(I);
        FParent.FWinControls.Insert(Position, Self);
      end;
    end;
    if WindowHandle <> 0 then
    begin
      if Position = 0 then Pos := HWND_BOTTOM
      else if Position = FParent.FWinControls.Count - 1 then Pos := HWND_TOP
      else if Position > I then
        Pos := TWinControl(FParent.FWinControls[Position + 1]).Handle
      else if Position < I then
        Pos := TWinControl(FParent.FWinControls[Position]).Handle
      else Exit;
      SetWindowPos(WindowHandle, Pos, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE);
    end;
  end;
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TWinControl.SetZOrder(TopMost: Boolean);
const
  WindowPos: array[Boolean] of Word = (HWND_BOTTOM, HWND_TOP);
var
  N, M: Integer;
begin
  if FParent <> nil then
  begin
    if TopMost then N := FParent.FWinControls.Count - 1 else N := 0;
    M := 0;
    if FParent.FControls <> nil then M := FParent.FControls.Count;
    SetZOrderPosition(M + N);
  end
  else if WindowHandle <> 0 then
    SetWindowPos(WindowHandle, WindowPos[TopMost], 0, 0, 0, 0,
      SWP_NOMOVE + SWP_NOSIZE);
end;

{$IF DEFINED(CLR)}
function TWinControl.GetDeviceContext(var WindowHandle: HWND): HDC;
var
  Wrapper: THWndWrapper;
begin
  Result := GetDeviceContext(Wrapper);
  WindowHandle := Wrapper.Handle;
end;
{$IFEND}

{$IF DEFINED(CLR)}
function TWinControl.GetDeviceContext(var WindowHandle: THWndWrapper): HDC;
{$ELSE}
function TWinControl.GetDeviceContext(var WindowHandle: HWnd): HDC;
{$IFEND}
begin
  if csDesigning in ComponentState then
    Result := GetDCEx(Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS)
  else
    Result := GetDC(Handle);
  if Result = 0 then
    raise EOutOfResources.CreateRes({$IFNDEF CLR}@{$ENDIF}SWindowDCError);
  WindowHandle := FHandle;
end;

function TWinControl.GetParentHandle: HWND;
begin
  if Parent <> nil then
    Result := Parent.Handle
  else
    Result := ParentWindow;
end;

function TWinControl.GetTopParentHandle: HWND;
var
  C: TWinControl;
begin
  C := Self;
  while C.Parent <> nil do
    C := C.Parent;
  Result := C.ParentWindow;
  if Result = 0 then Result := C.Handle;
end;

procedure TWinControl.Invalidate;
begin
  Perform(CM_INVALIDATE, 0, 0);
end;

procedure TWinControl.CMInputLangChange(var Message: TMessage);
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TWinControl then
      with Message do
        TWinControl(Components[I]).Perform(CM_INPUTLANGCHANGE, WParam, LParam);
end;

procedure TWinControl.CMInvalidate(var Message: TMessage);
var
  I: Integer;
begin
  if HandleAllocated then
  begin
    if Parent <> nil then Parent.Perform(CM_INVALIDATE, 1, 0);
    if Message.WParam = 0 then
    begin
      InvalidateRect(WindowHandle, nil, not (csOpaque in ControlStyle));
      { Invalidate child windows which use the parentbackground when themed }
      if StyleServices.Enabled then
        for I := 0 to ControlCount - 1 do
          if csParentBackground in Controls[I].ControlStyle then
            Controls[I].Invalidate;
    end;
  end;
end;

procedure TWinControl.Update;
begin
  if HandleAllocated then UpdateWindow(WindowHandle);
end;

procedure TWinControl.Repaint;
begin
  Invalidate;
  Update;
end;

procedure TWinControl.InvalidateFrame;
var
  R: TRect;
begin
  R := BoundsRect;
  InflateRect(R, 1, 1);
  InvalidateRect(Parent.WindowHandle, R, True);
end;

function TWinControl.CanFocus: Boolean;
var
  Control: TWinControl;
  Form: TCustomForm;
begin
  Result := False;
  Form := GetParentForm(Self);
  if Form <> nil then
  begin
    Control := Self;
    while Control <> Form do
    begin
      if not (Control.FVisible and Control.Enabled) then Exit;
      Control := Control.Parent;
    end;
    Result := True;
  end;
end;

[UIPermission(SecurityAction.InheritanceDemand, Window=UIPermissionWindow.AllWindows),
UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TWinControl.SetFocus;
var
  Parent: TCustomForm;
begin
  Parent := GetParentForm(Self);
  if Parent <> nil then
    Parent.FocusControl(Self)
  else if ParentWindow <> 0 then
    Winapi.Windows.SetFocus(Handle)
  else
    ValidParentForm(Self);
end;

function TWinControl.Focused: Boolean;
begin
  Result := (WindowHandle <> 0) and (GetFocus = WindowHandle);
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TWinControl.HandleNeeded;
begin
  if WindowHandle = 0 then
  begin
    if Parent <> nil then Parent.HandleNeeded;
    CreateHandle;
  end;
end;

function TWinControl.GetHandle: HWND;
begin
  HandleNeeded;
  Result := WindowHandle;
end;

{$IF DEFINED(CLR)}
function TWinControl.GetWindowHandle: HWND;
begin
  if Assigned(FHandle) then
    Result := FHandle.Handle
  else
    Result := 0;
end;

procedure TWinControl.SetWindowHandle(Value: HWND);
begin
  if not Assigned(FHandle) then
    if Value = 0 then Exit
    else
      FHandle := THWndWrapper.Create;
  FHandle.Handle := Value;
end;
{$IFEND}

function TWinControl.GetControlExtents: TRect;
var
  I: Integer;
begin
  Result := System.Types.Rect(MaxInt, MaxInt, 0, 0);
  for I := 0 to ControlCount - 1 do
    with Controls[I] do
      if Visible or (csDesigning in ComponentState) and
        not (csNoDesignVisible in ControlStyle) then
      begin
        if Margins.ControlLeft < Result.Left then Result.Left := Margins.ControlLeft;
        if Margins.ControlTop < Result.Top then Result.Top := Margins.ControlTop;
        if Margins.ControlLeft + Margins.ControlWidth > Result.Right then
          Result.Right := Margins.ControlLeft + Margins.ControlWidth;
        if Margins.ControlTop + Margins.ControlHeight > Result.Bottom then
          Result.Bottom := Margins.ControlTop + Margins.ControlHeight;
      end;
end;

function TWinControl.GetClientOrigin: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
  Winapi.Windows.ClientToScreen(Handle, Result);
end;

function TWinControl.GetClientRect: TRect;
begin
  Winapi.Windows.GetClientRect(Handle, Result);
end;

procedure TWinControl.AdjustSize;
begin
  if not (csLoading in ComponentState) and HandleAllocated then
  begin
    SetWindowPos(Handle, 0, 0, 0, Width, Height, SWP_NOACTIVATE or SWP_NOMOVE or
      SWP_NOZORDER);
    RequestAlign;
  end;
end;

procedure TWinControl.SetBorderWidth(Value: TBorderWidth);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Perform(CM_BORDERCHANGED, 0, 0);
  end;
end;

procedure TWinControl.SetCtl3D(Value: Boolean);
begin
  if FCtl3D <> Value then
  begin
    FCtl3D := Value;
    FParentCtl3D := False;
    Perform(CM_CTL3DCHANGED, 0, 0);
  end;
end;

procedure TWinControl.InvokeHelp;
begin
  case HelpType of
    htKeyword:
      if HelpKeyword <> '' then
      begin
        Application.HelpKeyword(HelpKeyword);
        Exit;
      end;
    htContext:
      if HelpContext <> 0 then
      begin
        Application.HelpContext(HelpContext);
        Exit;
      end;
  end;
  if (Parent <> nil) then Parent.InvokeHelp;
end;

function TWinControl.IsCtl3DStored: Boolean;
begin
  Result := not ParentCtl3D;
end;

function TWinControl.IsDoubleBufferedStored: Boolean;
begin
  Result := not ParentDoubleBuffered;
end;

procedure TWinControl.SetParentCtl3D(Value: Boolean);
begin
  if FParentCtl3D <> Value then
  begin
    FParentCtl3D := Value;
    if (FParent <> nil) and not (csReading in ComponentState) then
      Perform(CM_PARENTCTL3DCHANGED, 0, 0);
  end;
end;

function TWinControl.GetTabOrder: TTabOrder;
begin
  if FParent <> nil then
    Result := FParent.FTabList.IndexOf(Self)
  else
    Result := -1;
end;

procedure TWinControl.UpdateTabOrder(Value: TTabOrder);
var
  CurIndex, Count: Integer;
begin
  CurIndex := GetTabOrder;
  if CurIndex >= 0 then
  begin
    Count := FParent.FTabList.Count;
    if Value < 0 then Value := 0;
    if Value >= Count then Value := Count - 1;
    if Value <> CurIndex then
    begin
      FParent.FTabList.Delete(CurIndex);
      FParent.FTabList.Insert(Value, Self);
    end;
  end;
end;

procedure TWinControl.SetTabOrder(Value: TTabOrder);
begin
  if csReadingState in ControlState then
    FTabOrder := Value else
    UpdateTabOrder(Value);
end;

procedure TWinControl.SetTabStop(Value: Boolean);
var
  Style: Longint;
begin
  if FTabStop <> Value then
  begin
    FTabStop := Value;
    if HandleAllocated then
    begin
      Style := GetWindowLong(WindowHandle, GWL_STYLE) and not WS_TABSTOP;
      if Value then Style := Style or WS_TABSTOP;
      SetWindowLong(WindowHandle, GWL_STYLE, Style);
    end;
    Perform(CM_TABSTOPCHANGED, 0, 0);
  end;
end;

procedure TWinControl.SetUseDockManager(Value: Boolean);
begin
  if FUseDockManager <> Value then
  begin
    FUseDockManager := Value;
    if not (csDesigning in ComponentState) and Value then
      FDockManager := CreateDockManager;
  end;
end;

function TWinControl.HandleAllocated: Boolean;
begin
  Result := WindowHandle <> 0;
end;

procedure TWinControl.UpdateBounds;
var
  ParentHandle: HWND;
  Rect: TRect;
  WindowPlacement: TWindowPlacement;
  LPoint: TPoint;
begin
  if IsIconic(WindowHandle) then
  begin
{$IF DEFINED(CLR)}
    WindowPlacement.Length := Marshal.SizeOf(TypeOf(WindowPlacement));
{$ELSE}
    WindowPlacement.Length := SizeOf(WindowPlacement);
{$IFEND}
    GetWindowPlacement(WindowHandle, WindowPlacement);
    Rect := WindowPlacement.rcNormalPosition;
  end else
    GetWindowRect(WindowHandle, Rect);
  if GetWindowLong(WindowHandle, GWL_STYLE) and WS_CHILD <> 0 then
  begin
    ParentHandle := GetWindowLong(WindowHandle, GWL_HWNDPARENT);
    if ParentHandle <> 0 then
    begin
      // Swap Left and Right when parent has WS_EX_LAYOUTRTL style
      if GetWindowLong(ParentHandle, GWL_EXSTYLE) and WS_EX_LAYOUTRTL <> 0 then
      begin
        LPoint.X := Rect.Left;
        Rect.Left := Rect.Right;
        Rect.Right := LPoint.X;
      end;
      LPoint := Rect.TopLeft;
      Winapi.Windows.ScreenToClient(ParentHandle, LPoint);
      Rect.TopLeft := LPoint;
      LPoint := Rect.BottomRight;
      Winapi.Windows.ScreenToClient(ParentHandle, LPoint);
      Rect.BottomRight := LPoint;
    end;
  end;
  FLeft := Rect.Left;
  FTop := Rect.Top;
  FWidth := Rect.Right - Rect.Left;
  FHeight := Rect.Bottom - Rect.Top;
  UpdateAnchorRules;
//  UpdateExplicitBounds;
end;

procedure TWinControl.GetTabControlList(List: TList);
var
  I: Integer;
  Control: TWinControl;
begin
  if FTabList <> nil then
    for I := 0 to FTabList.Count - 1 do
    begin
      Control := TWinControl(FTabList[I]);
      List.Add(Control);
    end;
end;

procedure TWinControl.GetTabOrderList(List: TList);
var
  I: Integer;
  Control: TWinControl;
begin
  if FTabList <> nil then
    for I := 0 to FTabList.Count - 1 do
    begin
      Control := TWinControl(FTabList[I]);
      List.Add(Control);
      Control.GetTabOrderList(List);
    end;
end;

function TWinControl.FindNextControl(CurControl: TWinControl;
  GoForward, CheckTabStop, CheckParent: Boolean): TWinControl;
var
  I, StartIndex: Integer;
  List: TList;
begin
  Result := nil;
  List := TList.Create;
  try
    GetTabOrderList(List);
    if List.Count > 0 then
    begin
      StartIndex := List.IndexOf(CurControl);
      if StartIndex = -1 then
        if GoForward then StartIndex := List.Count - 1 else StartIndex := 0;
      I := StartIndex;
      repeat
        if GoForward then
        begin
          Inc(I);
          if I = List.Count then I := 0;
        end else
        begin
          if I = 0 then I := List.Count;
          Dec(I);
        end;
        CurControl := TWinControl(List[I]);
        if CurControl.CanFocus and
          (not CheckTabStop or CurControl.TabStop) and
          (not CheckParent or (CurControl.Parent = Self)) then
          Result := CurControl;
      until (Result <> nil) or (I = StartIndex);
    end;
  finally
    List.Free;
  end;
end;

procedure TWinControl.SelectNext(CurControl: TWinControl;
  GoForward, CheckTabStop: Boolean);
begin
  CurControl := FindNextControl(CurControl, GoForward,
    CheckTabStop, not CheckTabStop);
  if CurControl <> nil then CurControl.SetFocus;
end;

procedure TWinControl.SelectFirst;
var
  Form: TCustomForm;
  Control: TWinControl;
begin
  Form := GetParentForm(Self);
  if Form <> nil then
  begin
    Control := FindNextControl(nil, True, True, False);
    if Control = nil then
      Control := FindNextControl(nil, True, False, False);
    if Control <> nil then
      Form.ActiveControl := Control;
  end;
end;

[UIPermission(SecurityAction.InheritanceDemand, Window=UIPermissionWindow.AllWindows),
UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TWinControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Control: TControl;
begin
  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    if Control.Owner = Root then Proc(Control);
  end;
end;

procedure TWinControl.SetChildOrder(Child: TComponent; Order: Integer);
begin
  if Child is TWinControl then
    TWinControl(Child).SetZOrderPosition(Order)
  else if Child is TControl then
    TControl(Child).SetZOrderPosition(Order);
end;


function TWinControl.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := inherited CanResize(NewWidth, NewHeight);
end;

procedure TWinControl.CalcConstraints(var MinWidth, MinHeight, MaxWidth,
  MaxHeight: Integer);
type
  TScale = (sNone, sReg, sStretch, sOther);
var
  AdjustMinWidth, AdjustMinHeight, AdjustMaxWidth, AdjustMaxHeight: Integer;
  I, TotalMinWidth, TotalMaxWidth, TotalMinHeight, TotalMaxHeight: Integer;
  TotalMinWidth2, TotalMaxWidth2, TotalMinHeight2, TotalMaxHeight2: Integer;
  ControlMinWidth, ControlMaxWidth, ControlMinHeight, ControlMaxHeight: Integer;
  Control: TControl;
  R: TRect;

  WidthScale: TScale;
  HeightScale: TScale;

  procedure DoCalcConstraints(Control: TControl; var MinWidth, MinHeight,
    MaxWidth, MaxHeight: Integer);
  var
    Margin: Integer;
  begin
    with Control do
    begin
      if Constraints.MinWidth > 0 then
        MinWidth := Constraints.MinWidth
      else
        MinWidth := 0;
      if Constraints.MinHeight > 0 then
        MinHeight := Constraints.MinHeight
      else
        MinHeight := 0;
      if Constraints.MaxWidth > 0 then
        MaxWidth := Constraints.MaxWidth
      else
        MaxWidth := 0;
      if Constraints.MaxHeight > 0 then
        MaxHeight := Constraints.MaxHeight
      else
        MaxHeight := 0;
      if csAlignWithMargins in ControlStyle then
      begin
        Margin := Margins.Left + Margins.Right;
        if MinWidth > 0 then
          Inc(MinWidth, Margin);
        if MaxWidth >= Margin then
          Dec(MaxWidth, Margin);
        Margin := Margins.Top + Margins.Bottom;
        if MinHeight > 0 then
          Inc(MinHeight, Margin);
        if MaxHeight >= Margin then
          Dec(MaxHeight, Margin);
      end;
      { Allow override of constraints }
      ConstrainedResize(MinWidth, MinHeight, MaxWidth, MaxHeight);
    end;
  end;

begin
  if not HandleAllocated or (ControlCount = 0) then Exit;
  { Adjust min/max size to compensate for non-client area }
  R := GetClientRect;
  AdjustClientRect(R);
  if IsRectEmpty(R) then Exit; // Coming from an icon view, don't do constraints
  AdjustMinWidth := Margins.ControlWidth - (R.Right - R.Left);
  AdjustMinHeight := Margins.ControlHeight - (R.Bottom - R.Top);
  AdjustMaxWidth := Margins.ControlWidth - (R.Right - R.Left);
  AdjustMaxHeight := Margins.ControlHeight - (R.Bottom - R.Top);
  if MinWidth > 0 then Dec(MinWidth, AdjustMinWidth);
  if MinHeight > 0 then Dec(MinHeight, AdjustMinHeight);
  if MaxWidth > 0 then Dec(MaxWidth, AdjustMaxWidth);
  if MaxHeight > 0 then Dec(MaxHeight, AdjustMaxHeight);
  { Compare incoming min/max constraints with those that we calculate }
  try
    TotalMinWidth := 0;
    TotalMinWidth2 := 0;
    TotalMaxWidth := 0;
    TotalMaxWidth2 := 0;
    TotalMinHeight := 0;
    TotalMinHeight2 := 0;
    TotalMaxHeight := 0;
    TotalMaxHeight2 := 0;
    for I := 0 to ControlCount - 1 do
    begin
      Control := Controls[I];
      with Control do
        if Visible or (csDesigning in ComponentState) and
          not (csNoDesignVisible in ControlStyle) then
        begin
          DoCalcConstraints(Control, ControlMinWidth, ControlMinHeight,
            ControlMaxWidth, ControlMaxHeight);

          case Align of
            alTop, alBottom: WidthScale := sReg;
            alClient: WidthScale := sStretch;
            alNone:
              if Anchors * [akLeft, akRight] = [akLeft, akRight] then
              begin
                WidthScale := sReg;
                { Adjust Anchored controls }
                if ControlMinWidth > 0 then
                  ControlMinWidth := (R.Right - R.Left) - Margins.ControlWidth - ControlMinWidth;
                if ControlMaxWidth > 0 then
                  ControlMaxWidth := (R.Right - R.Left) + ControlMaxWidth - Margins.ControlWidth;
              end
              else
                WidthScale := sNone;
          else
            WidthScale := sOther;
          end;

          case Align of
            alLeft, alRight: HeightScale := sReg;
            alClient: HeightScale := sStretch;
            alNone:
              if Anchors * [akTop, akBottom] = [akTop, akBottom] then
              begin
                HeightScale := sReg;
                { Adjust Anchored controls }
                if ControlMinHeight > 0 then
                  ControlMinHeight := (R.Bottom - R.Top) - Margins.ControlHeight - ControlMinHeight;
                if ControlMaxHeight > 0 then
                  ControlMaxHeight := (R.Bottom - R.Top) + ControlMaxHeight - Margins.ControlHeight;
              end
              else
                HeightScale := sNone;
          else
            HeightScale := sOther;
          end;

          { Calculate min/max width }
          case WidthScale of
            sReg, sStretch:
              begin
                if (ControlMinWidth > 0) and (ControlMinWidth > MinWidth) then
                begin
                  MinWidth := ControlMinWidth;
                  if MinWidth > TotalMinWidth then
                    TotalMinWidth := MinWidth;
                end;
                if (ControlMaxWidth > 0) and (ControlMaxWidth < MaxWidth) then
                begin
                  MaxWidth := ControlMaxWidth;
                  if MaxWidth > TotalMaxWidth then
                    TotalMaxWidth := MaxWidth;
                end;
              end;
            sOther:
              begin
                Inc(TotalMinWidth2, Margins.ControlWidth);
                Inc(TotalMaxWidth2, Margins.ControlWidth);
              end;
          end;

          { Calculate min/max height }
          case HeightScale of
            sReg, sStretch:
              begin
                if (ControlMinHeight > 0) and (ControlMinHeight > MinHeight) then
                begin
                  MinHeight := ControlMinHeight;
                  if MinHeight > TotalMinHeight then
                    TotalMinHeight := MinHeight;
                end;
                if (ControlMaxHeight > 0) and (ControlMaxHeight < MaxHeight) then
                begin
                  MaxHeight := ControlMaxHeight;
                  if MaxHeight > TotalMaxHeight then
                    TotalMaxHeight := MaxHeight;
                end;
              end;
            sOther:
              begin
                Inc(TotalMinHeight2, Margins.ControlHeight);
                Inc(TotalMaxHeight2, Margins.ControlHeight);
              end;
          end;
        end;
    end;
    if (TotalMinWidth > 0) and (TotalMinWidth+TotalMinWidth2 > MinWidth) then
      MinWidth := TotalMinWidth+TotalMinWidth2;
    if (TotalMaxWidth > 0) and ((MaxWidth = 0) or (TotalMaxWidth+TotalMaxWidth2 > MaxWidth)) then
      MaxWidth := TotalMaxWidth+TotalMaxWidth2;
    if (TotalMinHeight > 0) and (TotalMinHeight+TotalMinHeight2 > MinHeight) then
      MinHeight := TotalMinHeight+TotalMinHeight2;
    if (TotalMaxHeight > 0) and ((MaxHeight = 0) or (TotalMaxHeight+TotalMaxHeight2 > MaxHeight)) then
      MaxHeight := TotalMaxHeight+TotalMaxHeight2;
  finally
    if MinWidth > 0 then Inc(MinWidth, AdjustMinWidth);
    if MinHeight > 0 then Inc(MinHeight, AdjustMinHeight);
    if MaxWidth > 0 then Inc(MaxWidth, AdjustMaxWidth);
    if MaxHeight > 0 then Inc(MaxHeight, AdjustMaxHeight);
  end;
end;

procedure TWinControl.ConstrainedResize(var MinWidth, MinHeight, MaxWidth,
  MaxHeight: Integer);
begin
  CalcConstraints(MinWidth, MinHeight, MaxWidth, MaxHeight);
  inherited ConstrainedResize(MinWidth, MinHeight, MaxWidth, MaxHeight);
end;

{$IF DEFINED(CLR)}
function TWinControl.GetTextPiece(Size: Integer): TCaption;
var
  S: string;
begin
  if WindowHandle <> 0 then
  begin
    Perform(WM_GETTEXT, Size + 1, S, Size + 1, True);
    Result := S;
  end else
    Result := inherited GetTextPiece(Size);
end;

function TWinControl.GetTextLen: Integer;
begin
  if WindowHandle <> 0 then
    Result := Perform(WM_GETTEXTLENGTH, 0, 0)
  else
    Result := Length(FText);
end;

procedure TWinControl.SetText(const Value: TCaption);
begin
  if GetText <> Value then
  begin
    if WindowHandle <> 0 then
      Perform(WM_SETTEXT, 0, string(Value))
    else
      FText := Value;
    Perform(CM_TEXTCHANGED, 0, 0);
  end;
end;

procedure TWinControl.ControlChange(Inserting: Boolean; Child: TControl);
begin
end;
{$IFEND}

procedure TWinControl.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    if not CheckDefaults or (Self.HelpContext = 0) then
      Self.HelpContext := HelpContext;
end;

function TWinControl.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TWinControlActionLink;
end;


procedure TWinControl.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TCustomAction then TCustomAction(Dest).HelpContext := HelpContext;
end;

function TWinControl.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  I, LeftOffset, TopOffset: Integer;
  Extents, R: TRect;
begin
  Result := True;
  { Restrict size to visible extents of children }
  if HandleAllocated and (Align <> alClient) and
    (not (csDesigning in ComponentState) or (ControlCount > 0)) then
  begin
    Extents := GetControlExtents;
    { Here's where HandleAllocated is needed }
    R := GetClientRect;
    AdjustClientRect(R);
    DisableAlign;
    try
      for I := 0 to ControlCount - 1 do
        with Controls[I] do
          if Visible or (csDesigning in ComponentState) and
            not (csNoDesignVisible in ControlStyle) then
          begin
            if Self.Align in [alNone, alLeft, alRight] then
              LeftOffset := Extents.Left - R.Left else
              LeftOffset := 0;
            if Self.Align in [alNone, alTop, alBottom] then
              TopOffset := Extents.Top - R.Top else
              TopOffset := 0;
            SetBounds(Left - LeftOffset, Top - TopOffset, Width, Height);
          end;
    finally
      Exclude(FControlState, csAlignmentNeeded);
      EnableAlign;
    end;
    if Align in [alNone, alLeft, alRight] then
      if Extents.Right - Extents.Left > 0 then
      begin
        NewWidth := Extents.Right - Extents.Left + Width - (R.Right - R.Left);
        if Align = alRight then RequestAlign;
      end
      else
        NewWidth := 0;
    if Align in [alNone, alTop, alBottom] then
      if Extents.Bottom - Extents.Top > 0 then
      begin
        NewHeight := Extents.Bottom - Extents.Top + Height - (R.Bottom - R.Top);
        if Align = alBottom then RequestAlign;
      end
      else
        NewHeight := 0;
  end;
end;

procedure TWinControl.SetBevelCut(Index: Integer; const Value: TBevelCut);
begin
  case Index of
    0: { BevelInner }
      if Value <> FBevelInner then
      begin
        FBevelInner := Value;
        Perform(CM_BORDERCHANGED, 0, 0);
      end;
    1: { BevelOuter }
      if Value <> FBevelOuter then
      begin
        FBevelOuter := Value;
        Perform(CM_BORDERCHANGED, 0, 0);
      end;
  end;
end;

procedure TWinControl.SetBevelEdges(const Value: TBevelEdges);
begin
  if Value <> FBevelEdges then
  begin
    FBevelEdges := Value;
    Perform(CM_BORDERCHANGED, 0, 0);
  end;
end;

procedure TWinControl.SetBevelKind(const Value: TBevelKind);
begin
  if Value <> FBevelKind then
  begin
    FBevelKind := Value;
    Perform(CM_BORDERCHANGED, 0, 0);
  end;
end;

procedure TWinControl.SetBevelWidth(const Value: TBevelWidth);
begin
  if Value <> FBevelWidth then
  begin
    FBevelWidth := Value;
    Perform(CM_BORDERCHANGED, 0, 0);
  end;
end;

procedure TWinControl.WMNCCalcSize(var Message: TWMNCCalcSize);
var
  EdgeSize: Integer;
{$IF DEFINED(CLR)}
  Params: TNCCalcSizeParams;
{$ELSE}
  Params: PNCCalcSizeParams;
{$IFEND}
begin
  inherited;
  Params := Message.CalcSize_Params;
  with Params{$IFNDEF CLR}^{$ENDIF} do
  begin
    InflateRect(rgrc0, -Integer(BorderWidth), -Integer(BorderWidth));
    if BevelKind <> bkNone then
    begin
      EdgeSize := 0;
      if BevelInner <> bvNone then Inc(EdgeSize, BevelWidth);
      if BevelOuter <> bvNone then Inc(EdgeSize, BevelWidth);
      with rgrc0 do
      begin
        if beLeft in BevelEdges then Inc(Left, EdgeSize);
        if beTop in BevelEdges then Inc(Top, EdgeSize);
        if beRight in BevelEdges then Dec(Right, EdgeSize);
        if beBottom in BevelEdges then Dec(Bottom, EdgeSize);
      end;
    end;
  end;
{$IF DEFINED(CLR)}
  Message.CalcSize_Params := Params;
{$IFEND}
end;

procedure TWinControl.WMNCPaint(var Message: TWMNCPaint);
const
  InnerStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENINNER, BDR_RAISEDINNER, 0);
  OuterStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENOUTER, BDR_RAISEDOUTER, 0);
  EdgeStyles: array[TBevelKind] of Integer = (0, 0, BF_SOFT, BF_FLAT);
  Ctl3DStyles: array[Boolean] of Integer = (BF_MONO, 0);
var
  DC: HDC;
  RC, RW, SaveRW: TRect;
  EdgeSize: Integer;
  WinStyle: Longint;
begin
  { Get window DC that is clipped to the non-client area }
  if (BevelKind <> bkNone) or (BorderWidth > 0) then
  begin
    DC := GetWindowDC(Handle);
    try
      Winapi.Windows.GetClientRect(Handle, RC);
      GetWindowRect(Handle, RW);
      MapWindowPoints(0, Handle, RW, 2);
      OffsetRect(RC, -RW.Left, -RW.Top);
      ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
      { Draw borders in non-client area }
      SaveRW := RW;
      InflateRect(RC, BorderWidth, BorderWidth);
      RW := RC;
      with RW do
      begin
        WinStyle := GetWindowLong(Handle, GWL_STYLE);
        if (WinStyle and WS_VSCROLL) <> 0 then
          if UseRightToLeftScrollBar then
            Dec(Left, GetSystemMetrics(SM_CYVSCROLL))
          else
            Inc(Right, GetSystemMetrics(SM_CYVSCROLL));
        if (WinStyle and WS_HSCROLL) <> 0 then
          Inc(Bottom, GetSystemMetrics(SM_CXHSCROLL));
      end;
      if BevelKind <> bkNone then
      begin
        EdgeSize := 0;
        if BevelInner <> bvNone then Inc(EdgeSize, BevelWidth);
        if BevelOuter <> bvNone then Inc(EdgeSize, BevelWidth);
        with RW do
        begin
          if beLeft in BevelEdges then Dec(Left, EdgeSize);
          if beTop in BevelEdges then Dec(Top, EdgeSize);
          if beRight in BevelEdges then Inc(Right, EdgeSize);
          if beBottom in BevelEdges then Inc(Bottom, EdgeSize);
        end;

      if TStyleManager.IsCustomStyleActive then
        DrawStyleEdge(DC, RW,
          TStyleElementEdges(InnerStyles[BevelInner] or OuterStyles[BevelOuter]),
          TStyleElementEdgeFlags(Byte(BevelEdges) or EdgeStyles[BevelKind] or Ctl3DStyles[Ctl3D] or BF_ADJUST))
      else
        DrawEdge(DC, RW, InnerStyles[BevelInner] or OuterStyles[BevelOuter],
          Byte(BevelEdges) or EdgeStyles[BevelKind] or Ctl3DStyles[Ctl3D] or BF_ADJUST);
      end;
      IntersectClipRect(DC, RW.Left, RW.Top, RW.Right, RW.Bottom);
      RW := SaveRW;
      { Erase parts not drawn }
      if Message.RGN = 1 then // Redraw entire NC area
        OffsetRect(RW, -RW.Left, -RW.Top)
      else
      begin
        GetRgnBox(Message.RGN, RC);
        MapWindowPoints(0, Handle, RC, 2);
        IntersectRect(RW, RW, RC);
        OffsetRect(RW, -SaveRW.Left, -SaveRW.Top);
      end;
      Winapi.Windows.FillRect(DC, RW, Brush.Handle);
    finally
      ReleaseDC(Handle, DC);
    end;
  end;

  inherited;

  if ThemeControl(Self) and (csNeedsBorderPaint in ControlStyle) then
    StyleServices.PaintBorder(Self, False);
end;

function TWinControl.FindChildControl(const ControlName: string): TControl;
var
  I: Integer;
begin
  Result := nil;
  if FWinControls <> nil then
    for I := 0 to FWinControls.Count - 1 do
      if CompareText(TWinControl(FWinControls[I]).Name, ControlName) = 0 then
      begin
        Result := TControl(FWinControls[I]);
        Exit;
      end;
end;

procedure TWinControl.WMContextMenu(var Message: TWMContextMenu);
var
  Ctrl: TControl;
begin
  if Message.Result <> 0 then Exit;
  Ctrl := ControlAtPos(ScreenToClient(SmallPointToPoint(Message.Pos)), False);
  if Ctrl <> nil then
    Message.Result := Ctrl.Perform(WM_CONTEXTMENU, 0, PointToLParam(SmallPointToPoint(Message.Pos)));

  if Message.Result = 0 then
    inherited;
end;

procedure TWinControl.UpdateUIState(CharCode: Word);
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Assigned(Form) then
    case CharCode of
      VK_LEFT..VK_DOWN, VK_TAB:
        Form.Perform(WM_CHANGEUISTATE, MakeLong(UIS_CLEAR, UISF_HIDEFOCUS), 0);
      VK_MENU:
        Form.Perform(WM_CHANGEUISTATE, MakeLong(UIS_CLEAR, UISF_HIDEACCEL), 0);
    end;
end;

procedure TWinControl.WMPrintClient(var Message: TWMPrintClient);
var
  SaveIndex: Integer;
{$IF DEFINED(CLR)}
  PaintMsg: TWMPaint;
{$IFEND}
  WasPrintClient: Boolean;
begin
  WasPrintClient := csPrintClient in ControlState;
  Include(FControlState, csPrintClient);
  try
    with Message do
      if Result <> 1 then
        if ((Flags and PRF_CHECKVISIBLE) = 0) or Visible then
        begin
          SaveIndex := SaveDC(DC);
          try
{$IF DEFINED(CLR)}
            PaintMsg := TWMPaint.Create(Message.OriginalMessage);
            PaintHandler(PaintMsg);
{$ELSE}
            PaintHandler(TWMPaint(Message));
{$IFEND}
          finally
            RestoreDC(DC, SaveIndex);
          end;
        end
        else
          inherited
      else
        inherited;
  finally
    if not WasPrintClient then
      Exclude(FControlState, csPrintClient);
  end;
end;

procedure TWinControl.WMGesture(var Message: TMessage);
{$IF NOT DEFINED(CLR)}
const
  GestureMap: array[0..4] of TInteractiveGesture = (igZoom, igPan, igRotate,
    igTwoFingerTap, igPressAndTap);
var
  LPoint: TPoint;
  LControl: TControl;
  LGestureInfo: GestureInfo;
  EventInfo: TGestureEventInfo;
{$IFEND}
begin
  if FTouchControl = nil then
  begin
    // If FTouchControl is nil a control requested a gesture but didn't
    // handle it. Call DefWindowProc to allow Windows to handle the gesture
    Message.Result := DefWindowProc(Handle, Message.Msg, Message.WParam, Message.LParam);
    Exit;
  end;

{$IF NOT DEFINED(CLR)}
  ZeroMemory(@LGestureInfo, SizeOf(LGestureInfo));
  LGestureInfo.cbSize := Sizeof(LGestureInfo);
  if GetGestureInfo(Message.LParam, LGestureInfo) then
  try
    ZeroMemory(@EventInfo, SizeOf(EventInfo));
    EventInfo.GestureID := LGestureInfo.dwID + igiFirst;

    // Don't pass GID_BEGIN and GID_END to the control
    if (EventInfo.GestureID <> igiBegin) and (EventInfo.GestureID <> igiEnd) then
    begin
      // Find the control to send the event to
      LControl := FTouchControl;
      while (LControl.Parent <> nil) and
            (igoParentPassthrough in LControl.Touch.InteractiveGestureOptions) and
            not (GestureMap[EventInfo.GestureID - igiZoom] in LControl.Touch.InteractiveGestures) do
        LControl := LControl.Parent;

      // Set EventInfo fields from GestureInfo
      LPoint := Point(LGestureInfo.ptsLocation.X, LGestureInfo.ptsLocation.Y);
      PhysicalToLogicalPoint(FHandle, LPoint);
      EventInfo.Location := LControl.ScreenToClient(LPoint);
      EventInfo.Flags := [];
      if LGestureInfo.dwFlags and GF_BEGIN = GF_BEGIN then
        Include(EventInfo.Flags, gfBegin);
      if LGestureInfo.dwFlags and GF_INERTIA = GF_INERTIA then
        Include(EventInfo.Flags, gfInertia);
      if LGestureInfo.dwFlags and GF_END = GF_END then
        Include(EventInfo.Flags, gfEnd);

      case EventInfo.GestureID of
        igiPan:
          begin
            EventInfo.Distance := Cardinal(LGestureInfo.ullArguments);
            EventInfo.InertiaVector := InertiaVectorFromArgument(LGestureInfo.ullArguments);
          end;
        igiZoom, igiTwoFingerTap:
          EventInfo.Distance := Cardinal(LGestureInfo.ullArguments);
        igiPressAndTap:
          begin
            // ullArguments is distance/offset. Add to Location to make TapLocation
            LPoint := SmallPointToPoint(TSmallPoint(Cardinal(LGestureInfo.ullArguments)));
            Inc(LPoint.X, LGestureInfo.ptsLocation.X);
            Inc(LPoint.Y, LGestureInfo.ptsLocation.Y);
            if LControl is TWinControl then
              PhysicalToLogicalPoint(TWinControl(LControl).Handle, LPoint)
            else
              PhysicalToLogicalPoint(LControl.Parent.Handle, LPoint);
            EventInfo.TapLocation := PointToSmallPoint(LControl.ScreenToClient(LPoint));
          end;
        igiRotate:
          EventInfo.Angle := RotateAngleFromArgument(LGestureInfo.ullArguments);
      end;

      // Send the event to the control, if not handled pass to Windows
      Message.Result := LControl.Perform(CM_GESTURE, 0, @EventInfo);
      if Message.Result <> 1 then
        Message.Result := DefWindowProc(FHandle, Message.Msg, Message.WParam, Message.LParam);
    end
    else
      // Let Windows handle GID_BEGIN and GID_END for default behaviour (if any)
      Message.Result := DefWindowProc(FHandle, Message.Msg, Message.WParam, Message.LParam);
  finally
    CloseGestureInfoHandle(Message.LParam);
    if EventInfo.GestureID = igiEnd then
      FTouchControl := nil;
  end;
{$IFEND}
end;

procedure TWinControl.WMGestureNotify(var Message: TWMGestureNotify);
{$IF NOT DEFINED(CLR)}
const
  // All pan gesture options
  CPanOoptions: TInteractiveGestureOptions = [igoPanSingleFingerHorizontal,
    igoPanSingleFingerVertical, igoPanInertia, igoPanGutter];
  // Gestures
  CPan: array[Boolean] of Cardinal = (0, GC_PAN);
  CZoom: array[Boolean] of Cardinal = (0, GC_ZOOM);
  CRotate: array[Boolean] of Cardinal = (0, GC_ROTATE);
  CPressAndTap: array[Boolean] of Cardinal = (0, GC_PRESSANDTAP);
  CTwoFingerTap: array[Boolean] of Cardinal = (0, GC_TWOFINGERTAP);
  // Options
  CPanSingleFingerVertical: array[Boolean] of Cardinal = (0, GC_PAN_WITH_SINGLE_FINGER_VERTICALLY);
  CPanSingleFingerHorizontal: array[Boolean] of Cardinal = (0, GC_PAN_WITH_SINGLE_FINGER_HORIZONTALLY);
  CPanWithGutter: array[Boolean] of Cardinal = (0, GC_PAN_WITH_GUTTER);
  CPanWithInertia: array[Boolean] of Cardinal = (0, GC_PAN_WITH_INERTIA);
var
  LPoint: TPoint;
  LControl: TControl;
  Configs: array of TGestureConfig;
  LGestures, ControlGestures: TInteractiveGestures;
  LGestureOptions, ControlGestureOptions: TInteractiveGestureOptions;
{$IFEND}
begin
{$IF NOT DEFINED(CLR)}
  // Convert incoming point to logical client coordinates
  LPoint := SmallPointToPoint(Message.NotifyStruct^.ptsLocation);
  PhysicalToLogicalPoint(FHandle, LPoint);
  LPoint := ScreenToClient(LPoint);

  // Use Windows default behaviour for the non client area
  if not PtInRect(ClientRect, LPoint) then
  begin
    SetLength(Configs, 1);
    Configs[0].dwID := 0;
    Configs[0].dwWant := 0;
    Configs[0].dwBlock := GC_ALLGESTURES;
    SetGestureConfig(Handle, 0, 1, @Configs[0], SizeOf(TGestureConfig));
    Message.Result := 1;
    Exit;
  end;

  // Find control to gesture will be sent to
  FTouchControl := ControlAtPos(LPoint, True);
  if FTouchControl = nil then
    FTouchControl := Self;

  // Build complete list of gestures
  LControl := FTouchControl;
  LControl.DoGetGestureOptions(LGestures, LGestureOptions);
  while (LControl.Parent <> nil) and
        (igoParentPassthrough in LControl.Touch.InteractiveGestureOptions) do
  begin
    LControl := LControl.Parent;
    LControl.DoGetGestureOptions(ControlGestures, ControlGestureOptions);
    // Include Pan options if igPan isn't in LGestures
    if (igPan in LGestures) then
      LGestureOptions := LGestureOptions + (ControlGestureOptions - CPanOoptions)
    else
      LGestureOptions := LGestureOptions + ControlGestureOptions;
    LGestures := LGestures + ControlGestures;
  end;

  // Call SetGestureConfig with the gestures the control wants
  if LGestures = [] then
  begin
    SetLength(Configs, 1);
    Configs[0].dwID := 0;
    Configs[0].dwWant := 0;
    Configs[0].dwBlock := GC_ALLGESTURES;
  end
  else
  begin
    SetLength(Configs, 5);
    ZeroMemory(@Configs[0], SizeOf(GestureConfig) * 5);

    // Pan gesture & options
    Configs[0].dwID := GID_PAN;
    Configs[0].dwWant := CPan[igPan in LGestures] or
      CPanSingleFingerVertical[igoPanSingleFingerVertical in LGestureOptions] or
      CPanSingleFingerHorizontal[igoPanSingleFingerHorizontal in LGestureOptions] or
      CPanWithGutter[igoPanGutter in LGestureOptions] or
      CPanWithInertia[igoPanInertia in LGestureOptions];
    Configs[0].dwBlock := CPan[not (igPan in LGestures)] or
      CPanSingleFingerVertical[not (igoPanSingleFingerVertical in LGestureOptions)] or
      CPanSingleFingerHorizontal[not (igoPanSingleFingerHorizontal in LGestureOptions)] or
      CPanWithGutter[not (igoPanGutter in LGestureOptions)] or
      CPanWithInertia[not (igoPanInertia in LGestureOptions)];

    // Zoom gesture
    Configs[1].dwID := GID_ZOOM;
    Configs[1].dwWant := CZoom[igZoom in LGestures];
    Configs[1].dwBlock := CZoom[not (igZoom in LGestures)];

    // Rotate gesture
    Configs[2].dwID := GID_ROTATE;
    Configs[2].dwWant := CRotate[igRotate in LGestures];
    Configs[2].dwBlock := CRotate[not (igRotate in LGestures)];

    // TwoFingerTap gesture
    Configs[3].dwID := GID_TWOFINGERTAP;
    Configs[3].dwWant := CTwoFingerTap[igTwoFingerTap in LGestures];
    Configs[3].dwBlock := CTwoFingerTap[not (igTwoFingerTap in LGestures)];

    // PressAnTap gesture
    Configs[4].dwID := GID_PRESSANDTAP;
    Configs[4].dwWant := CPressAndTap[igPressAndTap in LGestures];
    Configs[4].dwBlock := CPressAndTap[not (igPressAndTap in LGestures)];
  end;

  SetGestureConfig(Handle, 0, Length(Configs), @Configs[0], SizeOf(TGestureConfig));
  Message.Result := 1;
{$IFEND}
end;

procedure TWinControl.WMTabletQuerySystemGestureStatus(var Message: TMessage);
{$IF NOT DEFINED(CLR)}
const
  Options: array[TTabletOption] of Cardinal = (TABLET_DISABLE_PRESSANDHOLD,
    TABLET_DISABLE_PENTAPFEEDBACK, TABLET_DISABLE_PENBARRELFEEDBACK,
    TABLET_DISABLE_TOUCHUIFORCEON, TABLET_DISABLE_TOUCHUIFORCEOFF,
    TABLET_DISABLE_TOUCHSWITCH, TABLET_DISABLE_FLICKS,
    TABLET_DISABLE_SMOOTHSCROLLING, TABLET_DISABLE_FLICKFALLBACKKEYS);
var
  LPoint: TPoint;
  LControl: TControl;
  LOption: TTabletOption;
{$IFEND}
begin
{$IF NOT DEFINED(CLR)}
  // Convert incoming point to logical client coordinates and find control
  LPoint := Point(Message.LParamLo, Message.LParamHi);
  PhysicalToLogicalPoint(FHandle, LPoint);
  LPoint := ScreenToClient(LPoint);
  LControl := ControlAtPos(LPoint, True);
  if LControl = nil then
    LControl := Self;

  Message.Result := TABLET_DISABLE_PRESSANDHOLD or
    TABLET_DISABLE_PENTAPFEEDBACK or TABLET_DISABLE_PENBARRELFEEDBACK or
    TABLET_DISABLE_TOUCHUIFORCEON or TABLET_DISABLE_TOUCHUIFORCEOFF or
    TABLET_DISABLE_TOUCHSWITCH or TABLET_DISABLE_FLICKS or
    TABLET_DISABLE_SMOOTHSCROLLING or TABLET_DISABLE_FLICKFALLBACKKEYS;

  for LOption := Low(TTabletOption) to High(TTabletOption) do
    if LOption in LControl.Touch.TabletOptions then
      Message.Result := Message.Result and not Options[LOption];
{$IFEND}
end;

function TWinControl.GetParentBackground: Boolean;
begin
  Result := csParentBackground in ControlStyle;
end;

procedure TWinControl.SetParentBackground(Value: Boolean);
begin
  if ParentBackground <> Value then
  begin
    if Value then
      ControlStyle := ControlStyle + [csParentBackground]
    else
      ControlStyle := ControlStyle - [csParentBackground];
    Invalidate;
  end;
end;

procedure TWinControl.CMTextChanged(var Message: TMessage);
begin
  inherited;
  InvalidateDockHostSite(False);
end;

{$IF DEFINED(CLR)}
procedure TWinControl.InvalidateDockHostSite(FocusLost: Boolean);
{$ELSE}
procedure TWinControl.InvalidateDockHostSite;
{$IFEND}
var
  ChildDockSite: TControl;
  ParentWalk: TWinControl;
{$IF DEFINED(CLR)}
  LGCHandle: GCHandle;
{$IFEND}
begin
  { Invalidate the first dock site we come across; its ui may
    need updating to reflect which control has focus. }
  ChildDockSite := Self;
  ParentWalk := Parent;
  while (ChildDockSite.HostDockSite = nil) and (ParentWalk <> nil) do
  begin
    ChildDockSite := ParentWalk;
    ParentWalk := ParentWalk.Parent;
  end;
  if ChildDockSite <> nil then
{$IF DEFINED(CLR)}
    {Note: I changed the WParam from an object pointer to an HWND
     because that is less expensive than a GCHandle }
  begin
    LGCHandle := GCHandle.Alloc(Self);
    try
      ChildDockSite.SendDockNotification(CM_INVALIDATEDOCKHOST,
        THandle(IntPtr(LGCHandle)), Integer(FocusLost));
    finally
      LGCHandle.Free;
    end;
  end;
{$ELSE}
    ChildDockSite.SendDockNotification(CM_INVALIDATEDOCKHOST,
      Winapi.Windows.WPARAM(Self), Winapi.Windows.LPARAM(FocusLost));
{$IFEND}
end;

function TWinControl.DockReplaceDockClient(Client: TControl;
  NewDockSite: TWinControl; DropControl: TControl;
  ControlSide: TAlign; ReplacementClient: TControl): Boolean;
begin
  { Do nothing. Descendents may want to do some specific operations,
    such as manually replacing Client with ReplacementClient and
    manually docking Client to NewDockSite, DropControl, ControlSide. }
  Result := False;
end;

[UIPermission(SecurityAction.InheritanceDemand, Window=UIPermissionWindow.AllWindows),
UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
function TWinControl.PreProcessMessage(var Msg: TMsg): Boolean;
begin
  Result := False; { Not handled }
end;

{$IF NOT DEFINED(CLR)}
procedure TWinControl.RemoveWindowProps;
begin
  RemoveProp(FHandle, MakeIntAtom(ControlAtom));
  RemoveProp(FHandle, MakeIntAtom(WindowAtom));
end;
{$IFEND}

function TWinControl.IsQualifyingSite(const Client: TControl): Boolean;
begin
  Result := (Client.HostDockSite <> Self) or (VisibleDockClientCount > 1);
end;

procedure TWinControl.DoPaddingChange(Sender: TObject);
begin
  Realign;
end;

procedure TWinControl.SetPadding(const Value: TPadding);
begin
  FPadding.Assign(Value);
end;

procedure TWinControl.UpdateRecreatingFlag(Recreating: Boolean);
var
  I: Integer;
begin
  if Recreating then
    Include(FControlState, csRecreating)
  else
    Exclude(FControlState, csRecreating);
  if FWinControls <> nil then
    for I := 0 to FWinControls.Count - 1 do
      TWinControl(FWinControls[I]).UpdateRecreatingFlag(Recreating);
end;

procedure TWinControl.UpdateControlOriginalParentSize(AControl: TControl; var AOriginalParentSize: TPoint);
begin
  if csReading in ComponentState then
  begin
    if not (csDesigning in AControl.ComponentState) then
      AOriginalParentSize := FDesignSize
  end
  else if HandleAllocated then
    AOriginalParentSize := ClientRect.BottomRight
  else
  begin
    AOriginalParentSize.X := Width;
    AOriginalParentSize.Y := Height;
  end;
  Dec(AOriginalParentSize.X, Padding.Left + Padding.Right);
  Dec(AOriginalParentSize.Y, Padding.Top + Padding.Bottom);
end;

procedure TWinControl.SetParent(AParent: TWinControl);
var
  LRecreate: Boolean;
begin
  LRecreate := HandleAllocated and not (csRecreating in ControlState);
  if LRecreate then
    UpdateRecreatingFlag(True);
  try
    inherited;
  finally
    if LRecreate then
      UpdateRecreatingFlag(False);
  end;
end;

{ TGraphicControl }

constructor TGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TGraphicControl.Destroy;
begin
  if CaptureControl = Self then SetCaptureControl(nil);
  FCanvas.Free;
  inherited Destroy;
end;

{$IF DEFINED(CLR)}
[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.SafeSubWindows)]
function TGraphicControl.get_Canvas: TCanvas;
begin
  Result := FCanvas;
end;
{$IFEND}

procedure TGraphicControl.WMPaint(var Message: TWMPaint);
begin
  if (Message.DC <> 0) and not (csDestroying in ComponentState) then
  begin
    Canvas.Lock;
    try
      Canvas.Handle := Message.DC;
      try
        Paint;
      finally
        Canvas.Handle := 0;
      end;
    finally
      Canvas.Unlock;
    end;
  end;
end;

procedure TGraphicControl.Paint;
begin
end;

{ THintWindow }

constructor THintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := $80FFFF;
  Canvas.Font := Screen.HintFont;
  Canvas.Brush.Style := bsClear;
end;

procedure THintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_BORDER;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    // CS_DROPSHADOW requires Windows XP or above
    if CheckWin32Version(5, 1) then
      WindowClass.Style := WindowClass.style or CS_DROPSHADOW;
    if NewStyleControls then
      ExStyle := WS_EX_TOOLWINDOW;
    AddBiDiModeExStyle(ExStyle);
  end;
end;

procedure THintWindow.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := HTTRANSPARENT;
end;

procedure THintWindow.WMNCPaint(var Message: TWMNCPaint);
var
  DC: HDC;
begin
  DC := GetWindowDC(Handle);
  try
    NCPaint(DC);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure THintWindow.Paint;
var
  R, ClipRect: TRect;
  LColor: TColor;
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  LGradientStart, LGradientEnd, LTextColor: TColor;
begin
  R := ClientRect;
  LStyle := StyleServices;
  LTextColor := Screen.HintFont.Color;
  if LStyle.Enabled then
  begin
    ClipRect := R;
    InflateRect(R, 4, 4);
    if TOSVersion.Check(6) and LStyle.IsSystemStyle then
    begin
      // Paint Windows gradient background
      LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(tttStandardNormal), R, ClipRect);
    end
    else
    begin
      LDetails := LStyle.GetElementDetails(thHintNormal);
      if LStyle.GetElementColor(LDetails, ecGradientColor1, LColor) and (LColor <> clNone) then
        LGradientStart := LColor
      else
        LGradientStart := clInfoBk;
      if LStyle.GetElementColor(LDetails, ecGradientColor2, LColor) and (LColor <> clNone) then
        LGradientEnd := LColor
      else
        LGradientEnd := clInfoBk;
      if LStyle.GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
        LTextColor := LColor
      else
        LTextColor := Screen.HintFont.Color;
      GradientFillCanvas(Canvas, LGradientStart, LGradientEnd, R, gdVertical);
    end;
    R := ClipRect;
  end;
  Inc(R.Left, 2);
  Inc(R.Top, 2);
  Canvas.Font.Color := LTextColor;
  DrawText(Canvas.Handle, Caption, -1, R, DT_LEFT or DT_NOPREFIX or
    DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
end;

function THintWindow.IsHintMsg(var Msg: TMsg): Boolean;
begin
  with Msg do
    Result := ((Message >= WM_KEYFIRST) and (Message <= WM_KEYLAST)) or
      ((Message = CM_ACTIVATE) or (Message = CM_DEACTIVATE)) or
      (Message = CM_APPKEYDOWN) or (Message = CM_APPSYSCOMMAND) or
      (Message = WM_COMMAND) or ((Message > WM_MOUSEMOVE) and
      (Message <= WM_MOUSELAST)) or (Message = WM_NCMOUSEMOVE);
end;

procedure THintWindow.ReleaseHandle;
begin
  DestroyHandle;
end;

procedure THintWindow.CMTextChanged(var Message: TMessage);
begin
  inherited;
  { Avoid flicker when calling ActivateHint }
  if FActivating then Exit;
  Width := Canvas.TextWidth(Caption) + 6;
  Height := Canvas.TextHeight(Caption) + 4;
end;

procedure THintWindow.ActivateHint(Rect: TRect; const AHint: string);
type
  TAnimationStyle = (atSlideNeg, atSlidePos, atBlend);
const
  AnimationStyle: array[TAnimationStyle] of Integer = (AW_VER_NEGATIVE,
    AW_VER_POSITIVE, AW_BLEND);
var
  Animate: BOOL;
  Style: TAnimationStyle;
  Monitor: TMonitor;
begin
  FActivating := True;
  try
    Caption := AHint;
    Inc(Rect.Bottom, 4);
    UpdateBoundsRect(Rect);
    Monitor := Screen.MonitorFromPoint(Point(Rect.Left, Rect.Top));
    if Width > Monitor.Width then
      Width := Monitor.Width;
    if Height > Monitor.Height then
      Height := Monitor.Height;
    if Rect.Top + Height > Monitor.Top + Monitor.Height then
      Rect.Top := (Monitor.Top + Monitor.Height) - Height;
    if Rect.Left + Width > Monitor.Left + Monitor.Width then
      Rect.Left := (Monitor.Left + Monitor.Width) - Width;
    if Rect.Left < Monitor.Left then Rect.Left := Monitor.Left;
    if Rect.Bottom < Monitor.Top then Rect.Top := Monitor.Top;

    ParentWindow := Application.Handle;
    SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, Width, Height,
      SWP_NOACTIVATE);
    if (GetTickCount - FLastActive > 250) and (Length(AHint) < 100) and
       Assigned(AnimateWindowProc) then
    begin
      SystemParametersInfo(SPI_GETTOOLTIPANIMATION, 0, {$IFNDEF CLR}@{$ENDIF}Animate, 0);
      if Animate then
      begin
        SystemParametersInfo(SPI_GETTOOLTIPFADE, 0, {$IFNDEF CLR}@{$ENDIF}Animate, 0);
        if Animate then
          Style := atBlend
        else
          if Mouse.GetCursorPos.Y > Rect.Top then
            Style := atSlideNeg
          else
            Style := atSlidePos;
        AnimateWindowProc(Handle, 100, AnimationStyle[Style] or AW_SLIDE);
      end;
    end;
    ShowWindow(Handle, SW_SHOWNOACTIVATE);
    Invalidate;
  finally
    FLastActive := GetTickCount;
    FActivating := False;
  end;
end;

procedure THintWindow.ActivateHintData(Rect: TRect; const AHint: string; AData: TCustomData);
begin
  ActivateHint(Rect, AHint);
end;

function THintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: TCustomData): TRect;
begin
  Result := System.Types.Rect(0, 0, MaxWidth, 0);
  DrawText(Canvas.Handle, AHint, -1, Result, DT_CALCRECT or DT_LEFT or
    DT_WORDBREAK or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly);
  Inc(Result.Right, 6);
  Inc(Result.Bottom, 2);
end;

procedure THintWindow.NCPaint(DC: HDC);
var
  R: TRect;
  Details: TThemedElementDetails;
begin
  R := System.Types.Rect(0, 0, Width, Height);
  if not StyleServices.Enabled then
    Winapi.Windows.DrawEdge(DC, R, BDR_RAISEDOUTER, BF_RECT)
  else
  begin
    Details := StyleServices.GetElementDetails(twWindowRoot);
    StyleServices.DrawEdge(DC, Details, R, [eeRaisedOuter], [efRect]);
  end;
end;

procedure THintWindow.WMPrint(var Message: TMessage);
begin
  PaintTo(Message.WParam, 0, 0);
  NCPaint(Message.WParam);
end;

function THintWindow.ShouldHideHint: Boolean;
begin
  Result := True;
end;

{ TDragImageList }

function ClientToWindow(Handle: HWND; X, Y: Integer): TPoint;
var
  Rect: TRect;
  Point: TPoint;
begin
  Point.X := X;
  Point.Y := Y;
  ClientToScreen(Handle, Point);
  GetWindowRect(Handle, Rect);
  Result.X := Point.X - Rect.Left;
  Result.Y := Point.Y - Rect.Top;
end;

procedure TDragImageList.Initialize;
begin
  inherited Initialize;
  DragCursor := crNone;
end;

function TDragImageList.SetDragImage(Index, HotSpotX, HotSpotY: Integer): Boolean;
begin
  if HandleAllocated then
  begin
    FDragIndex := Index;
    FDragHotspot.x := HotSpotX;
    FDragHotspot.y := HotSpotY;
    ImageList_BeginDrag(Handle, Index, HotSpotX, HotSpotY);
    Result := True;
    FDragging := Result;
  end
  else Result := False;
end;

procedure TDragImageList.SetDragCursor(Value: TCursor);
begin
  if Value <> DragCursor then
  begin
    FDragCursor := Value;
    if Dragging then
      Screen.Cursor := DragCursor;
  end;
end;

function TDragImageList.GetHotSpot: TPoint;
begin
  Result := inherited GetHotSpot;
  if HandleAllocated and Dragging then
    ImageList_GetDragImage(nil, Result);
end;

function TDragImageList.BeginDrag(Window: HWND; X, Y: Integer): Boolean;
begin
  Result := False;
  if HandleAllocated then
  begin
    if not Dragging then SetDragImage(FDragIndex, FDragHotspot.x, FDragHotspot.y);
    Result := DragLock(Window, X, Y);
    if Result then
    begin
      FOldCursor := Screen.Cursor;
      Screen.Cursor := DragCursor;
    end;
  end;
end;

function TDragImageList.DragLock(Window: HWND; XPos, YPos: Integer): Boolean;
begin
  Result := False;
  if HandleAllocated and (Window <> FDragHandle) then
  begin
    DragUnlock;
    FDragHandle := Window;
    with ClientToWindow(FDragHandle, XPos, YPos) do
      Result := ImageList_DragEnter(FDragHandle, X, Y);
  end;
end;

procedure TDragImageList.DragUnlock;
begin
  if HandleAllocated and (FDragHandle <> 0) then
  begin
    ImageList_DragLeave(FDragHandle);
    FDragHandle := 0;
  end;
end;

function TDragImageList.DragMove(X, Y: Integer): Boolean;
begin
  if HandleAllocated then
    with ClientToWindow(FDragHandle, X, Y) do
      Result := ImageList_DragMove(X, Y)
  else
    Result := False;
end;

procedure TDragImageList.ShowDragImage;
begin
  if HandleAllocated then ImageList_DragShowNoLock(True);
end;

procedure TDragImageList.HideDragImage;
begin
  if HandleAllocated then ImageList_DragShowNoLock(False);
end;

function TDragImageList.EndDrag: Boolean;
begin
  if HandleAllocated and Dragging then
  begin
    DragUnlock;
    Result := ImageList_EndDrag;
    FDragging := False;
    DragCursor := crNone;
    Screen.Cursor := FOldCursor;
  end
  else Result := False;
end;

{ TCustomControl }

constructor TCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TCustomControl.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

{$IF DEFINED(CLR)}
[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.SafeSubWindows)]
function TCustomControl.get_Canvas: TCanvas;
begin
  Result := FCanvas;
end;
{$IFEND}

procedure TCustomControl.WMPaint(var Message: TWMPaint);
begin
  Include(FControlState, csCustomPaint);
  inherited;
  Exclude(FControlState, csCustomPaint);
end;

procedure TCustomControl.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      TControlCanvas(FCanvas).UpdateTextFlags;
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

procedure TCustomControl.Paint;
begin
end;

{ TDockZone }

function NextVisibleZone(StartZone: TDockZone): TDockZone;
begin
  Result := StartZone;
  while Assigned(Result) and not Result.Visible do
    Result := Result.FNextSibling;
end;

function IsOrientationSet(Zone: TDockZone): Boolean;
begin
  Result := (Assigned(Zone.FParentZone) and
             (Zone.FParentZone.FOrientation <> doNoOrient)) or
            ((Zone.FTree.FTopZone = Zone) and (Zone.FOrientation <> doNoOrient));
end;

constructor TDockZone.Create(Tree: TDockTree);
begin
  inherited Create;
  FTree := Tree;
end;

function TDockZone.GetChildCount: Integer;
var
  Zone: TDockZone;
begin
  Result := 0;
  Zone := FChildZones;
  while Zone <> nil do
  begin
    Zone := Zone.FNextSibling;
    Inc(Result);
  end;
end;

function TDockZone.GetVisibleChildCount: Integer;
var
  Zone: TDockZone;
begin
  Result := 0;
  Zone := FirstVisibleChild;
  while Zone <> nil do
  begin
    Zone := Zone.NextVisible;
    Inc(Result);
  end;
end;

function TDockZone.GetVisible: Boolean;
var
  NextChild: TDockZone;
begin
  if Assigned(FChildControl) then
    Result := FChildControl.Visible
  else
  begin
    Result := True;
    NextChild := FirstVisibleChild;
    while Assigned(NextChild) do
    begin
      if NextChild.Visible then Exit;
      NextChild := NextChild.FNextSibling;
    end;
    Result := False;
  end;
end;

function TDockZone.GetLimitBegin: Integer;
var
  CheckZone: TDockZone;
begin
  if FTree.FTopZone = Self then
    CheckZone := Self
  else
    CheckZone := FParentZone;

  if CheckZone.FOrientation = doHorizontal then
    Result := Top
  else if CheckZone.FOrientation = doVertical then
    Result := Left
  else
    raise Exception.Create('');
end;

function TDockZone.GetLimitSize: Integer;
var
  CheckZone: TDockZone;
begin
  if FTree.FTopZone = Self then
    CheckZone := Self
  else
    CheckZone := FParentZone;

  if CheckZone.FOrientation = doHorizontal then
    Result := Height
  else if CheckZone.FOrientation = doVertical then
    Result := Width
  else
    raise Exception.Create('');
end;

function TDockZone.GetTopLeft(Orient: Integer{TDockOrientation}): Integer;
var
  Zone: TDockZone;
  R: TRect;
begin
  Zone := Self;
  while Zone <> FTree.FTopZone do
  begin
    if (Zone.FParentZone.FOrientation = TDockOrientation(Orient)) and
      (Zone.FPrevSibling <> nil) then
    begin
      Result := Zone.FPrevSibling.ZoneLimit;
      Exit;
    end
    else
      Zone := Zone.FParentZone;
  end;
  R := FTree.FDockSite.ClientRect;
  FTree.FDockSite.AdjustClientRect(R);
  case TDockOrientation(Orient) of
    doVertical: Result := R.Left;
    doHorizontal: Result := R.Top;
  else
    Result := 0;
  end;
end;

function TDockZone.GetHeightWidth(Orient: Integer{TDockOrientation}): Integer;
var
  Zone: TDockZone;
  R: TRect;
begin
  if (Self = FTree.FTopZone) or ((FParentZone = FTree.FTopZone) and
    (FChildControl <> nil) and (FTree.FTopZone.VisibleChildCount = 1)) then
  begin
    R := FTree.FDockSite.ClientRect;
    FTree.FDockSite.AdjustClientRect(R);
    if TDockOrientation(Orient) = doHorizontal then
      Result := R.Bottom - R.Top
    else
      Result := R.Right - R.Left;
  end
  else
  begin
    Zone := Self;
    while Zone <> FTree.FTopZone do
    begin
      if Zone.FParentZone.FOrientation = TDockOrientation(Orient) then
      begin
        Result := Zone.ZoneLimit - Zone.LimitBegin;
        Exit;
      end
      else
        Zone := Zone.FParentZone;
    end;
    if FTree.FTopZone.FOrientation = TDockOrientation(Orient) then
      Result := FTree.FTopXYLimit
    else
      Result := FTree.FTopZone.ZoneLimit;
  end;
end;

procedure TDockZone.ResetChildren;
var
  MaxLimit: Integer;
  NewLimit: Integer;
  ChildNode: TDockZone;
begin
  if (VisibleChildCount = 0) or (FOrientation = doNoOrient) then Exit;
  ChildNode := FirstVisibleChild;
  case FOrientation of
    doHorizontal: MaxLimit := Height;
    doVertical: MaxLimit := Width;
  else
    MaxLimit := 0;
  end;
  NewLimit := MaxLimit div VisibleChildCount;
  while ChildNode <> nil do
  begin
    if ChildNode.FNextSibling = nil then
      ChildNode.ZoneLimit := MaxLimit
    else
      ChildNode.ZoneLimit := ChildNode.LimitBegin + NewLimit;
    ChildNode.Update;
    ChildNode := ChildNode.NextVisible;
  end;
end;

function TDockZone.GetControlName: string;
begin
  Result := '';
  if FChildControl <> nil then
  begin
    if FChildControl.Name = '' then
      raise Exception.CreateRes({$IFNDEF CLR}@{$ENDIF}SDockedCtlNeedsName);
    Result := FChildControl.Name;
  end;
end;

function TDockZone.SetControlName(const Value: string): Boolean;
var
  Client: TControl;
begin
  Client := nil;
  with FTree do
  begin
    FDockSite.ReloadDockedControl(Value, Client);
    Result := Client <> nil;
    if Result then
    begin
      FReplacementZone := Self;
      try
        Client.ManualDock(FDockSite, nil, alNone);
      finally
        FReplacementZone := nil;
      end;
    end;
  end;
end;

procedure TDockZone.Update;

  function ParentNotLast: Boolean;
  var
    Parent: TDockZone;
  begin
    Result := False;
    Parent := FParentZone;
    while Parent <> nil do
    begin
      if Parent.NextVisible <> nil then
      begin
        Result := True;
        Exit;
      end;
      Parent := Parent.FParentZone;
    end;
  end;

var
  NewWidth, NewHeight: Integer;
  R: TRect;
begin
  if (FChildControl <> nil) and FChildControl.Visible and (FTree.FUpdateCount = 0) then
  begin
    FChildControl.DockOrientation := FParentZone.FOrientation;
    NewWidth := Width;
    NewHeight := Height;
    if ParentNotLast then
    begin
      if FParentZone.FOrientation = doHorizontal then
        Dec(NewWidth, FTree.FBorderWidth)
      else
        Dec(NewHeight, FTree.FBorderWidth);
    end;
    if (NextVisible <> nil) or ((FParentZone <> FTree.FTopZone) and
      ((FParentZone.FOrientation = FTree.FTopZone.FOrientation) and
      (ZoneLimit < FTree.FTopXYLimit)) or
      ((FParentZone.FOrientation <> FTree.FTopZone.FOrientation) and
      (ZoneLimit < FTree.FTopZone.ZoneLimit))) then
    begin
      if FParentZone.FOrientation = doHorizontal then
        Dec(NewHeight, FTree.FBorderWidth)
      else
        Dec(NewWidth, FTree.FBorderWidth);
    end;
    R := Bounds(Left, Top, NewWidth, NewHeight);
    FTree.AdjustDockRect(FChildControl, R);
    FChildControl.BoundsRect := R;
  end;
end;

function TDockZone.GetZoneLimit: Integer;
begin
  if not Visible and IsOrientationSet(Self) then
    // LimitSize will be zero and zone will take up no space
    Result := GetLimitBegin
  else
    Result := FZoneLimit;
end;

procedure TDockZone.SetZoneLimit(const Value: Integer);
begin
  FZoneLimit := Value;
end;

procedure TDockZone.ExpandZoneLimit(NewLimit: Integer);

  function GetLastChildZone(Zone: TDockZone): TDockZone;
  begin
    { Assumes Zone has at least one child }
    Result := Zone.FChildZones;
    while Result.FNextSibling <> nil do
      Result := Result.FNextSibling;
  end;

var
  LastChild, ChildZone: TDockZone;
begin
  ZoneLimit := NewLimit;
  ChildZone := FChildZones;
  while Assigned(ChildZone) do
  begin
    if ChildZone.ChildCount > 0 then
    begin
      LastChild := GetLastChildZone(ChildZone);
      LastChild.ExpandZoneLimit(NewLimit);
    end;
    ChildZone := ChildZone.FNextSibling;
  end;
end;

procedure TDockZone.ResetZoneLimits;
var
  ChildZone: TDockZone;
begin
  ChildZone := FChildZones;
  while Assigned(ChildZone) do
  begin
    { If the ZoneLimit is too big or too small then just reset all child zones }
    if (ChildZone.ZoneLimit < ChildZone.LimitBegin) or
       (ChildZone.ZoneLimit > LimitSize) then
    begin
      ResetChildren;
      FTree.ForEachAt(Self, FTree.UpdateZone);
    end;
    ChildZone.ResetZoneLimits;
    ChildZone := ChildZone.FNextSibling;
  end;
end;

function TDockZone.NextVisible: TDockZone;
begin
  Result := NextVisibleZone(FNextSibling);
end;

function TDockZone.PrevVisible: TDockZone;
begin
  Result := FPrevSibling;
  while Assigned(Result) and not Result.Visible do
    Result := Result.FPrevSibling;
end;

function TDockZone.FirstVisibleChild: TDockZone;
begin
  Result := NextVisibleZone(FChildZones)
end;

{ TDockTree }

const
  GrabberSize = 12;
  cPriorVersion = $00040000;

constructor TDockTree.Create(DockSite: TWinControl);
var
  I: Integer;
begin
  inherited Create;
  FBorderWidth := 4;
  FDockSite := DockSite;
  FVersion := $00080000;
  FGrabberSize := GrabberSize;
  FGrabbersOnTop := (DockSite.Align <> alTop) and (DockSite.Align <> alBottom);
  FTopZone := TDockZone.Create(Self);
  FBrush := TBrush.Create;
  FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
  // insert existing controls into tree
  BeginUpdate;
  try
    for I := 0 to DockSite.ControlCount - 1 do
      InsertControl(DockSite.Controls[I], alLeft, nil);
    FTopZone.ResetChildren;
  finally
    EndUpdate;
  end;
  if not (csDesigning in DockSite.ComponentState) then
  begin
    FOldWndProc := FDockSite.WindowProc;
    FDockSite.WindowProc := WindowProc;
  end;
end;

destructor TDockTree.Destroy;
begin
  if Assigned(FOldWndProc) then
  begin
    FDockSite.WindowProc := FOldWndProc;
    FOldWndProc := nil;
  end;
  PruneZone(FTopZone);
  FBrush.Free;
  inherited Destroy;
end;

procedure TDockTree.AdjustDockRect(Control: TControl; var ARect: TRect);
begin
  { Allocate room for the caption on the left if docksite is horizontally
    oriented, otherwise allocate room for the caption on the top. }
  if FDockSite.Align in [alTop, alBottom] then
    Inc(ARect.Left, GrabberSize) else
    Inc(ARect.Top, GrabberSize);
end;

procedure TDockTree.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TDockTree.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount <= 0 then
  begin
    FUpdateCount := 0;
    UpdateAll;
  end;
end;

function TDockTree.FindControlZone(Control: TControl): TDockZone;
var
  CtlZone: TDockZone;

  procedure DoFindControlZone(StartZone: TDockZone);
  begin
    if StartZone.FChildControl = Control then
      CtlZone := StartZone
    else begin
      // Recurse sibling
      if (CtlZone = nil) and (StartZone.FNextSibling <> nil) then
        DoFindControlZone(StartZone.FNextSibling);
      // Recurse child
      if (CtlZone = nil) and (StartZone.FChildZones <> nil) then
        DoFindControlZone(StartZone.FChildZones);
    end;
  end;

begin
  CtlZone := nil;
  if (Control <> nil) and (FTopZone <> nil) then DoFindControlZone(FTopZone);
  Result := CtlZone;
end;

procedure TDockTree.ForEachAt(Zone: TDockZone; Proc: TForEachZoneProc);

  procedure DoForEach(Zone: TDockZone);
  begin
    Proc(Zone);
    // Recurse sibling
    if Zone.FNextSibling <> nil then DoForEach(Zone.FNextSibling);
    // Recurse child
    if Zone.FChildZones <> nil then DoForEach(Zone.FChildZones);
  end;

begin
  if Zone = nil then Zone := FTopZone;
  DoForEach(Zone);
end;

procedure TDockTree.GetControlBounds(Control: TControl; out CtlBounds: TRect);
var
  Z: TDockZone;
begin
  Z := FindControlZone(Control);
  if Z = nil then
  begin
    CtlBounds.Left := 0;
    CtlBounds.Right := 0;
    CtlBounds.Top := 0;
    CtlBounds.Bottom := 0;
  end
  else
    with Z do
      CtlBounds := Bounds(Left, Top, Width, Height);
end;

function TDockTree.HitTest(const MousePos: TPoint; out HTFlag: Integer): TControl;
var
  Zone: TDockZone;
begin
  Zone := InternalHitTest(MousePos, HTFlag);
  if Zone <> nil then Result := Zone.FChildControl
  else Result := nil;
end;

procedure TDockTree.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
const
  OrientArray: array[TAlign] of TDockOrientation = (doNoOrient, doHorizontal,
    doHorizontal, doVertical, doVertical, doNoOrient, doNoOrient); { alCustom }
  MakeLast: array[TAlign] of Boolean = (False, False, True, False, True, False, False);  { alCustom }
var
  Sibling, Me: TDockZone;
  InsertOrientation, CurrentOrientation: TDockOrientation;
  NewWidth, NewHeight: Integer;
  R: TRect;
begin
  if not Control.Visible then Exit;
  if FReplacementZone <> nil then
  begin
    FReplacementZone.FChildControl := Control;
    FReplacementZone.Update;
  end
  else if FTopZone.FChildZones = nil then
  begin
    // Tree is empty, so add first child
    R := FDockSite.ClientRect;
    FDockSite.AdjustClientRect(R);
    NewWidth := R.Right - R.Left;
    NewHeight := R.Bottom - R.Top;
    if FDockSite.AutoSize then
    begin
      if NewWidth = 0 then NewWidth := Control.UndockWidth;
      if NewHeight = 0 then NewHeight := Control.UndockHeight;
    end;
    R := Bounds(R.Left, R.Top, NewWidth, NewHeight);
    AdjustDockRect(Control, R);
    Control.BoundsRect := R;
    Me := TDockZone.Create(Self);
    FTopZone.FChildZones := Me;
    Me.FParentZone := FTopZone;
    Me.FChildControl := Control;
  end
  else
  begin
    // Default to right-side docking
    if InsertAt in [alClient, alNone] then InsertAt := alRight;
    Me := FindControlZone(Control);
    if Me <> nil then RemoveZone(Me);
    Sibling := FindControlZone(DropCtl);
    InsertOrientation := OrientArray[InsertAt];
    if FTopZone.ChildCount = 1 then
    begin
      // Tree only has one child, and a second is being added, so orientation and
      // limits must be set up
      FTopZone.FOrientation := InsertOrientation;
      case InsertOrientation of
        doHorizontal:
          begin
            FTopZone.ZoneLimit := FTopZone.FChildZones.Width;
            FTopXYLimit := FTopZone.FChildZones.Height;
          end;
        doVertical:
          begin
            FTopZone.ZoneLimit := FTopZone.FChildZones.Height;
            FTopXYLimit := FTopZone.FChildZones.Width;
          end;
      end;
    end;
    Me := TDockZone.Create(Self);
    Me.FChildControl := Control;
    if Sibling <> nil then
      CurrentOrientation := Sibling.FParentZone.FOrientation
    else
      CurrentOrientation := FTopZone.FOrientation;
    if InsertOrientation = doNoOrient then
      InsertOrientation := CurrentOrientation;
    // Control is being dropped into a zone with the same orientation we
    // are requesting, so we just need to add ourselves to the sibling last
    if InsertOrientation = CurrentOrientation then InsertSibling(Me, Sibling,
      MakeLast[InsertAt])
    // Control is being dropped into a zone with a different orientation than
    // we are requesting
    else InsertNewParent(Me, Sibling, InsertOrientation, MakeLast[InsertAt]);
  end;
  { Redraw client dock frames }
  FDockSite.Invalidate;
end;

procedure TDockTree.InsertNewParent(NewZone, SiblingZone: TDockZone;
  ParentOrientation: TDockOrientation; InsertLast: Boolean);
var
  NewParent: TDockZone;
begin
  NewParent := TDockZone.Create(Self);
  NewParent.FOrientation := ParentOrientation;
  if SiblingZone = nil then
  begin
    // if SiblingZone is nil, then we need to insert zone as a child of the top
    NewParent.ZoneLimit := FTopXYLimit;
    FTopXYLimit := FTopZone.ZoneLimit;
    FShiftScaleOrient := ParentOrientation;
    FScaleBy := 0.5;
    if InsertLast then
    begin
      NewParent.FChildZones := FTopZone;
      FTopZone.FParentZone := NewParent;
      FTopZone.FNextSibling := NewZone;
      NewZone.FPrevSibling := FTopZone;
      NewZone.FParentZone := NewParent;
      FTopZone := NewParent;
      ForEachAt(NewParent.FChildZones, ScaleZone);
    end
    else begin
      NewParent.FChildZones := NewZone;
      FTopZone.FParentZone := NewParent;
      FTopZone.FPrevSibling := NewZone;
      NewZone.FNextSibling := FTopZone;
      NewZone.FParentZone := NewParent;
      FTopZone := NewParent;
      ForEachAt(NewParent.FChildZones, ScaleZone);
      FShiftBy := FTopZone.ZoneLimit div 2;
      ForEachAt(NewParent.FChildZones, ShiftZone);
      NewZone.ZoneLimit := FTopZone.ZoneLimit div 2;
    end;
    ForEachAt(nil, UpdateZone);
  end
  else begin
    // if SiblingZone is not nil, we need to insert a new parent zone for me
    // and my SiblingZone
    NewParent.ZoneLimit := SiblingZone.ZoneLimit;
    NewParent.FParentZone := SiblingZone.FParentZone;
    NewParent.FPrevSibling := SiblingZone.FPrevSibling;
    if NewParent.FPrevSibling <> nil then
      NewParent.FPrevSibling.FNextSibling := NewParent;
    NewParent.FNextSibling := SiblingZone.FNextSibling;
    if NewParent.FNextSibling <> nil then
      NewParent.FNextSibling.FPrevSibling := NewParent;
    if NewParent.FParentZone.FChildZones = SiblingZone then
      NewParent.FParentZone.FChildZones := NewParent;
    NewZone.FParentZone := NewParent;
    SiblingZone.FParentZone := NewParent;
    if InsertLast then
    begin
      // insert after SiblingZone
      NewParent.FChildZones := SiblingZone;
      SiblingZone.FPrevSibling := nil;
      SiblingZone.FNextSibling := NewZone;
      NewZone.FPrevSibling := SiblingZone;
    end
    else begin
      // insert before SiblingZone
      NewParent.FChildZones := NewZone;
      SiblingZone.FPrevSibling := NewZone;
      SiblingZone.FNextSibling := nil;
      NewZone.FNextSibling := SiblingZone;
    end;
    // Set bounds of new children
  end;
  NewParent.ResetChildren;
  NewParent.ResetZoneLimits;
  ForEachAt(nil, UpdateZone);
end;

procedure TDockTree.InsertSibling(NewZone, SiblingZone: TDockZone;
  InsertLast: Boolean);
begin
  if SiblingZone = nil then
  begin
    // If sibling is nil then make me the a child of the top
    SiblingZone := FTopZone.FChildZones;
    if InsertLast then
      while SiblingZone.FNextSibling <> nil do
        SiblingZone := SiblingZone.FNextSibling;
  end;
  if InsertLast then
  begin
    // Insert me after sibling
    NewZone.FParentZone := SiblingZone.FParentZone;
    NewZone.FPrevSibling := SiblingZone;
    NewZone.FNextSibling := SiblingZone.FNextSibling;
    if NewZone.FNextSibling <> nil then
      NewZone.FNextSibling.FPrevSibling := NewZone;
    SiblingZone.FNextSibling := NewZone;
  end
  else begin
    // insert before sibling
    NewZone.FNextSibling := SiblingZone;
    NewZone.FPrevSibling := SiblingZone.FPrevSibling;
    if NewZone.FPrevSibling <> nil then
      NewZone.FPrevSibling.FNextSibling := NewZone;
    SiblingZone.FPrevSibling := NewZone;
    NewZone.FParentZone := SiblingZone.FParentZone;
    if NewZone.FParentZone.FChildZones = SiblingZone then
      NewZone.FParentZone.FChildZones := NewZone;
  end;
  // Set up zone limits for all siblings
  SiblingZone.FParentZone.ResetChildren;
  SiblingZone.FParentZone.ResetZoneLimits;
end;

function TDockTree.ZoneCaptionHitTest(const Zone: TDockZone;
  const MousePos: TPoint; var HTFlag: Integer): Boolean;
var
  ZoneTop, ZoneLeft: Integer;
begin
  Result := False;
  ZoneTop := Zone.Top;
  ZoneLeft := Zone.Left;
  if FGrabbersOnTop then
  begin
    if (MousePos.Y >= ZoneTop) and (MousePos.Y <= ZoneTop + FGrabberSize) and
      (MousePos.X >= ZoneLeft) and (MousePos.X <= ZoneLeft + Zone.Width) then
    begin
      Result := True;
      with Zone.FChildControl do
        if MousePos.X > Left + Width - 15 then
          HTFlag := HTCLOSE
        else
          HTFlag := HTCAPTION;
    end;
  end
  else
  begin
    if (MousePos.X >= ZoneLeft) and (MousePos.X <= ZoneLeft + FGrabberSize) and
      (MousePos.Y >= ZoneTop) and (MousePos.Y <= ZoneTop + Zone.Height) then
    begin
      Result := True;
      if MousePos.Y < Zone.FChildControl.Top + 15 then
        HTFlag := HTCLOSE
      else
        HTFlag := HTCAPTION;
    end;
  end;
end;

function TDockTree.FindControlAtPos(const Pos: TPoint): TControl;
var
  I: Integer;
  P: TPoint;
begin
  for I := FDockSite.ControlCount - 1 downto 0 do
  begin
    Result := FDockSite.Controls[I];
    with Result do
    begin
      { Control must be Visible and Showing }
      if not Result.Visible or ((Result is TWinControl) and
           not TWinControl(Result).Showing) then
          Continue;
      P := Point(Pos.X - Left, Pos.Y - Top);
      if PtInRect(ClientRect, P) then
        Exit;
    end;
  end;
  Result := nil;
end;

function TDockTree.InternalHitTest(const MousePos: TPoint; out HTFlag: Integer): TDockZone;
var
  ResultZone: TDockZone;

  procedure DoFindZone(Zone: TDockZone);
  begin
    // Check for hit on bottom splitter...
    if (Zone.FParentZone.FOrientation = doHorizontal) and
      ((MousePos.Y <= Zone.ZoneLimit) and
      (MousePos.Y >= Zone.ZoneLimit - FBorderWidth)) then
    begin
      HTFlag := HTBORDER;
      ResultZone := Zone;
    end
    // Check for hit on left splitter...
    else if (Zone.FParentZone.FOrientation = doVertical) and
      ((MousePos.X <= Zone.ZoneLimit) and
      (MousePos.X >= Zone.ZoneLimit - FBorderWidth)) then
    begin
      HTFlag := HTBORDER;
      ResultZone := Zone;
    end
    // Check for hit on grabber...
    else if Zone.FChildControl <> nil then
    begin
      if ZoneCaptionHitTest(Zone, MousePos, HTFlag) then
          ResultZone := Zone;
    end;
    // Recurse to next zone...
    if (ResultZone = nil) and (Zone.NextVisible <> nil) then
      DoFindZone(Zone.NextVisible);
    if (ResultZone = nil) and (Zone.FirstVisibleChild <> nil) then
      DoFindZone(Zone.FirstVisibleChild);
  end;

var
  CtlAtPos: TControl;
begin
  ResultZone := nil;
  HTFlag := HTNOWHERE;
  CtlAtPos := FindControlAtPos(MousePos);
  if (CtlAtPos <> nil) and (CtlAtPos.HostDockSite = FDockSite) then
  begin
    ResultZone := FindControlZone(CtlAtPos);
    if ResultZone <> nil then
      HTFlag := HTCLIENT;
  end
  else if (FTopZone.FirstVisibleChild <> nil) and (CtlAtPos = nil) then
    DoFindZone(FTopZone.FirstVisibleChild);
  Result := ResultZone;
end;

var
  TreeStreamEndFlag: Integer = -1;

procedure TDockTree.LoadFromStream(Stream: TStream);

  procedure ReadControlName(var ControlName: string);
  var
    LSize: Integer;
    LBytes: TBytes;
  begin
    ControlName := '';
    Stream.ReadBuffer(LSize, SizeOf(LSize));
    if LSize > 0 then
    begin
      SetLength(LBytes, LSize);
      Stream.ReadBuffer(LBytes{$IFNDEF CLR}[0]{$ENDIF}, LSize);
      ControlName := TEncoding.UTF8.GetString(LBytes);
    end;
  end;

var
  CompName: string;
  Client: TControl;
  Level, LastLevel, I, InVisCount: Integer;
  Zone, LastZone, NextZone: TDockZone;
  OldRelativeSizes: Boolean;
  TopOrientation: TDockOrientation;
  TempZone: TDockZone;
{$IF DEFINED(CLR)}
  LByte: Byte;
{$IFEND}
begin
  PruneZone(FTopZone);
  OldRelativeSizes := FRelativeSizes;
  BeginUpdate;
  try
    // read stream version
    Stream.ReadBuffer(I, SizeOf(I));
    if I = cPriorVersion then
    begin
      { We can't have RelativeSizes at this time }
      FRelativeSizes := False;
      TopOrientation := doNoOrient;
    end
    else if I = FVersion then
    begin
      { Read in the relative size "option", and the orientation of
        the top zone }
{$IF DEFINED(CLR)}
      Stream.ReadBuffer(LByte, SizeOf(Byte));
      FRelativeSizes := LByte <> 0;
      Stream.ReadBuffer(LByte, SizeOf(Byte));
      TopOrientation := TDockOrientation(LByte);
{$ELSE}
      Stream.ReadBuffer(FRelativeSizes, SizeOf(FRelativeSizes));
      Stream.ReadBuffer(TopOrientation, SizeOf(TopOrientation));
{$IFEND}
    end
    else
      raise Exception.CreateFmt(SDockZoneVersionConflict, [FVersion, I]);
    { read invisible dock clients }
    Stream.ReadBuffer(InVisCount, SizeOf(InVisCount));
    for I := 0 to InVisCount - 1 do
    begin
      ReadControlName(CompName);
      if CompName <> '' then
      begin
        FDockSite.ReloadDockedControl(CompName, Client);
        if Client <> nil then
        begin
          Client.Visible := False;
          Client.ManualDock(FDockSite);
        end;
      end;
    end;
    // read top zone data
    Stream.ReadBuffer(FTopXYLimit, SizeOf(FTopXYLimit));
    { Convert to a real size from the relative size, if needed }
    if FRelativeSizes then
      FTopXYLimit := ActualSize(FTopXYLimit, ReferenceFromOrient(TopOrientation));

    LastLevel := 0;
    LastZone := nil;
    // read dock zone tree
    while True do
    begin
      with Stream do
      begin
        ReadBuffer(Level, SizeOf(Level));
        if Level = TreeStreamEndFlag then
          Break;
        Zone := TDockZone.Create(Self);
{$IF DEFINED(CLR)}
        ReadBuffer(LByte, SizeOf(Byte));
        Zone.FOrientation := TDockOrientation(LByte);
{$ELSE}
        ReadBuffer(Zone.FOrientation, SizeOf(Zone.FOrientation));
{$IFEND}
        ReadBuffer(Zone.FZoneLimit, SizeOf(Zone.FZoneLimit));
        if FRelativeSizes then
          Zone.FZoneLimit := ActualSize(Zone.FZoneLimit, ReferenceFromOrient(Zone.FOrientation));
        ReadControlName(CompName);
        if CompName <> '' then
          if not Zone.SetControlName(CompName) then
          begin
            {Remove dock zone if control cannot be found}
            Zone.Free;
            Continue;
          end;
      end;
      if Level = 0 then
        FTopZone := Zone
      else if Level = LastLevel then
      begin
        { Fix up badly saved desktop speed settings or .dsk's }
        { If a zone is the same level as the previous zone, and
          the previous zone has a previous zone (ie: It is in the middle),
          but it doesn't have a child control, then it shouldn't be there.
          So, remove it. }
        if (LastZone.FPrevSibling <> nil) and (LastZone.FChildControl = nil) then
        begin
          TempZone := LastZone;
          LastZone := LastZone.FPrevSibling;
          TempZone.Free;
          { At this point, LastZone.FNextSibling is set to TempZone, which
            we just freed. However, the next line below will solve the
            problem. }
        end;
        LastZone.FNextSibling := Zone;
        Zone.FPrevSibling := LastZone;
        Zone.FParentZone := LastZone.FParentZone;
      end
      else if Level > LastLevel then
      begin
        LastZone.FChildZones := Zone;
        Zone.FParentZone := LastZone;
      end
      else if Level < LastLevel then
      begin
        NextZone := LastZone;
        for I := 1 to LastLevel - Level do NextZone := NextZone.FParentZone;
        NextZone.FNextSibling := Zone;
        Zone.FPrevSibling := NextZone;
        Zone.FParentZone := NextZone.FParentZone;
      end;
      LastLevel := Level;
      LastZone := Zone;
    end;
  finally
    FRelativeSizes := OldRelativeSizes;
    { If loading failed for some reason, be sure we have an FTopZone still.
      Otherwise, it could AV. }
    if FTopZone = nil then
      FTopZone := TDockZone.Create(Self);
    EndUpdate;
  end;
end;

procedure TDockTree.PaintDockFrame(Canvas: TCanvas; Control: TControl;
  const ARect: TRect);

  procedure DrawCloseButton(Left, Top: Integer);
  var
    Details: TThemedElementDetails;
    DrawRect: TRect;
  begin
    DrawRect := System.Types.Rect(Left, Top, Left+FGrabberSize-2,
      Top+FGrabberSize-2);
    if StyleServices.Enabled then
    begin
      Details := StyleServices.GetElementDetails(twCloseButtonNormal);
      StyleServices.DrawElement(Canvas.Handle, Details, DrawRect, nil);
    end
    else
      DrawFrameControl(Canvas.Handle, DrawRect, DFC_CAPTION, DFCS_CAPTIONCLOSE);
  end;

  procedure DrawGrabberLine(Left, Top, Right, Bottom: Integer);
  begin
    with Canvas do
    begin
      Pen.Color := clBtnHighlight;
      MoveTo(Right, Top);
      LineTo(Left, Top);
      LineTo(Left, Bottom);
      Pen.Color := clBtnShadow;
      LineTo(Right, Bottom);
      LineTo(Right, Top-1);
    end;
  end;

  procedure DrawThemedGrabber(const GripperType: TThemedReBar;
    const Left, Top, Right, Bottom: Integer);
  var
    Details: TThemedElementDetails;
    DrawRect: TRect;
  begin
    DrawRect := System.Types.Rect(Left, Top, Right, Bottom);
    Details := StyleServices.GetElementDetails(GripperType);
    StyleServices.DrawElement(Canvas.Handle, Details, DrawRect);
  end;

begin
  with ARect do
    if FDockSite.Align in [alTop, alBottom] then
    begin
      DrawCloseButton(Left+1, Top+1);
      if StyleServices.Enabled then
      begin
        DrawThemedGrabber(trGripperVert, Left+1, Top+FGrabberSize+1,
          Left+10, Bottom-2);
      end
      else
      begin
        DrawGrabberLine(Left+3, Top+FGrabberSize+1, Left+5, Bottom-2);
        DrawGrabberLine(Left+6, Top+FGrabberSize+1, Left+8, Bottom-2);
      end;
    end
    else
    begin
      DrawCloseButton(Right-FGrabberSize+1, Top+1);
      if StyleServices.Enabled then
      begin
        DrawThemedGrabber(trGripper, Left+2, Top+1,
          Right-FGrabberSize-2, Top+10)
      end
      else
      begin
        DrawGrabberLine(Left+2, Top+3, Right-FGrabberSize-2, Top+5);
        DrawGrabberLine(Left+2, Top+6, Right-FGrabberSize-2, Top+8);
      end;
    end;
end;

procedure TDockTree.PaintSite(DC: HDC);
var
  Canvas: TControlCanvas;
  Control: TControl;
  I: Integer;
  R: TRect;
begin
  Canvas := TControlCanvas.Create;
  try
    Canvas.Control := FDockSite;
    Canvas.Lock;
    try
      Canvas.Handle := DC;
      try
        for I := 0 to FDockSite.ControlCount - 1 do
        begin
          Control := FDockSite.Controls[I];
          if Control.Visible and (Control.HostDockSite = FDockSite) then
          begin
            R := Control.BoundsRect;
            AdjustDockRect(Control, R);
            AdjustFrameRect(Control, R);
            PaintDockFrame(Canvas, Control, R);
          end;
        end;
      finally
        Canvas.Handle := 0;
      end;
    finally
      Canvas.Unlock;
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TDockTree.PositionDockRect(Client, DropCtl: TControl;
  DropAlign: TAlign; var DockRect: TRect);
var
  VisibleClients,
  NewX, NewY, NewWidth, NewHeight: Integer;
begin
  VisibleClients := FDockSite.VisibleDockClientCount;
  { When docksite has no controls in it, or 1 or less clients then the
    dockrect should only be based on the client area of the docksite }
  if (DropCtl = nil) or (DropCtl.DockOrientation = doNoOrient) or
     {(DropCtl = Client) or }(VisibleClients < 2) then
  begin
    DockRect := System.Types.Rect(0, 0, FDockSite.ClientWidth,
      FDockSite.ClientHeight);
    { When there is exactly 1 client we divide the docksite client area in half}
    if VisibleClients > 0 then
    with DockRect do
      case DropAlign of
        alLeft: Right := Right div 2;
        alRight: Left := Right div 2;
        alTop: Bottom := Bottom div 2;
        alBottom: Top := Bottom div 2;
      end;
  end
  else begin
  { Otherwise, if the docksite contains more than 1 client, set the coordinates
    for the dockrect based on the control under the mouse }
    NewX := DropCtl.Left;
    NewY := DropCtl.Top;
    NewWidth := DropCtl.Width;
    NewHeight := DropCtl.Height;
    if DropAlign in [alLeft, alRight] then
      NewWidth := DropCtl.Width div 2
    else if DropAlign in [alTop, alBottom] then
      NewHeight := DropCtl.Height div 2;
    case DropAlign of
      alRight: Inc(NewX, NewWidth);
      alBottom: Inc(NewY, NewHeight);
    end;
    DockRect := Bounds(NewX, NewY, NewWidth, NewHeight);
  end;
  MapWindowPoints(FDockSite.Handle, 0, DockRect, 2);
end;

procedure TDockTree.PruneZone(Zone: TDockZone);

  procedure DoPrune(Zone: TDockZone);
  begin
    // Recurse sibling
    if Zone.FNextSibling <> nil then
      DoPrune(Zone.FNextSibling);
    // Recurse child
    if Zone.FChildZones <> nil then
      DoPrune(Zone.FChildZones);
    // Free zone
    Zone.Free;
  end;

begin
  if Zone = nil then Exit;
  // Delete children recursively
  if Zone.FChildZones <> nil then DoPrune(Zone.FChildZones);
  // Fixup all pointers to this zone
  if Zone.FPrevSibling <> nil then
    Zone.FPrevSibling.FNextSibling := Zone.FNextSibling
  else if Zone.FParentZone <> nil then
    Zone.FParentZone.FChildZones := Zone.FNextSibling;
  if Zone.FNextSibling <> nil then
    Zone.FNextSibling.FPrevSibling := Zone.FPrevSibling;
  // Free this zone
  if Zone = FTopZone then FTopZone := nil;
  Zone.Free;
end;

procedure TDockTree.RemoveControl(Control: TControl);
var
  Z: TDockZone;
begin
  Z := FindControlZone(Control);
  if (Z <> nil) then
  begin
    if Z = FReplacementZone then
      Z.FChildControl := nil
    else
     RemoveZone(Z);
    Control.DockOrientation := doNoOrient;
    { Redraw client dock frames }
    FDockSite.Invalidate;
  end;
end;

procedure TDockTree.RemoveZone(Zone: TDockZone);
var
  Sibling, LastChild: TDockZone;
  ZoneChildCount: Integer;
begin
  if Zone = nil then
    raise Exception.Create(SDockTreeRemoveError + SDockZoneNotFound);
  if Zone.FChildControl = nil then
    raise Exception.Create(SDockTreeRemoveError + SDockZoneHasNoCtl);
  ZoneChildCount := Zone.FParentZone.ChildCount;
  if ZoneChildCount = 1 then
  begin
    FTopZone.FChildZones := nil;
    FTopZone.FOrientation := doNoOrient;
  end
  else if ZoneChildCount = 2 then
  begin
    // This zone has only one sibling zone
    if Zone.FPrevSibling = nil then
      Sibling := Zone.FNextSibling
    else
      Sibling := Zone.FPrevSibling;
    if Sibling.FChildControl <> nil then
    begin
      // Sibling is a zone with one control and no child zones
      if Zone.FParentZone = FTopZone then
      begin
        // If parent is top zone, then just remove the zone
        FTopZone.FChildZones := Sibling;
        Sibling.FPrevSibling := nil;
        Sibling.FNextSibling := nil;
        Sibling.ZoneLimit := FTopZone.LimitSize;
        Sibling.Update;
      end
      else begin
        // Otherwise, move sibling's control up into parent zone and dispose of sibling
        Zone.FParentZone.FOrientation := doNoOrient;
        Zone.FParentZone.FChildControl := Sibling.FChildControl;
        Zone.FParentZone.FChildZones := nil;
        Sibling.Free;
      end;
      ForEachAt(Zone.FParentZone, UpdateZone);
    end
    else
    begin
      // Sibling is a zone with child zones, so sibling must be made topmost
      // or collapsed into higher zone.
      if Zone.FParentZone = FTopZone then
      begin
        // Zone is a child of topmost zone, so sibling becomes topmost
        Sibling.ExpandZoneLimit(FTopXYLimit);
        FTopXYLimit := FTopZone.ZoneLimit;
        FTopZone.Free;
        FTopZone := Sibling;
        Sibling.FNextSibling := nil;
        Sibling.FPrevSibling := nil;
        Sibling.FParentZone := nil;
        UpdateAll;
      end
      else if Sibling.FChildZones <> nil then
      begin
        // Zone's parent is not the topmost zone, so child zones must be
        // collapsed into parent zone
        Sibling.FChildZones.FPrevSibling := Zone.FParentZone.FPrevSibling;
        if Sibling.FChildZones.FPrevSibling = nil then
          Zone.FParentZone.FParentZone.FChildZones := Sibling.FChildZones
        else
          Sibling.FChildZones.FPrevSibling.FNextSibling := Sibling.FChildZones;
        LastChild := Sibling.FChildZones;
        LastChild.FParentZone := Zone.FParentZone.FParentZone;
        if LastChild.FNextSibling <> nil then
        repeat
          LastChild := LastChild.FNextSibling;
          LastChild.FParentZone := Zone.FParentZone.FParentZone;
        until LastChild.FNextSibling = nil;
        LastChild.FNextSibling := Zone.FParentZone.FNextSibling;
        if LastChild.FNextSibling <> nil then
          LastChild.FNextSibling.FPrevSibling := LastChild;
        ForEachAt(LastChild.FParentZone, UpdateZone);
        Zone.FParentZone.Free;
        Sibling.Free;
      end
      else
      begin
        { The Sibling had no child zones, so it can be removed,
          and the parent now has no children, since Zone was its only
          sibling. }
        Sibling.FParentZone.FChildZones := nil;
        Sibling.Free;
      end;
    end;
  end
  else
  begin
    // This zone has multiple sibling zones
    if Zone.FPrevSibling = nil then
    begin
      // First zone in parent's child list, so make next one first and remove
      // from list
      Zone.FParentZone.FChildZones := Zone.FNextSibling;
      Zone.FNextSibling.FPrevSibling := nil;
      Zone.FNextSibling.Update;
    end
    else
    begin
      // Not first zone in parent's child list, so remove zone from list and fix
      // up adjacent siblings
      Zone.FPrevSibling.FNextSibling := Zone.FNextSibling;
      if Zone.FNextSibling <> nil then
        Zone.FNextSibling.FPrevSibling := Zone.FPrevSibling;
      Zone.FPrevSibling.ExpandZoneLimit(Zone.ZoneLimit);
      Zone.FPrevSibling.Update;
    end;
    ForEachAt(Zone.FParentZone, UpdateZone);
  end;
  Zone.Free;
end;

procedure TDockTree.ResetBounds(Force: Boolean);
var
  R: TRect;
begin
  if not (csLoading in FDockSite.ComponentState) and (FReplacementZone = nil) and
    (FTopZone <> nil) and (FDockSite.VisibleDockClientCount > 0) then
  begin
    R := FDockSite.ClientRect;
    FDockSite.AdjustClientRect(R);
    if Force or not EqualRect(R, FOldRect) then
    begin
      FOldRect := R;
      case FTopZone.FOrientation of
        doHorizontal:
          begin
            FTopZone.ZoneLimit := R.Right - R.Left;
            FTopXYLimit := R.Bottom - R.Top;
          end;
        doVertical:
          begin
            FTopZone.ZoneLimit := R.Bottom - R.Top;
            FTopXYLimit := R.Right - R.Left;
          end;
      end;
      if FDockSite.DockClientCount > 0 then
      begin
        SetNewBounds(nil);
        if FUpdateCount = 0 then ForEachAt(nil, UpdateZone);
      end;
    end;
  end;
end;

procedure TDockTree.ScaleZone(Zone: TDockZone);
begin
  if Zone = nil then Exit;
  if (Zone <> nil) and (Zone.FParentZone.FOrientation = FShiftScaleOrient) then
    with Zone do
      ZoneLimit := Integer(Round(ZoneLimit * FScaleBy));
end;

procedure TDockTree.SaveToStream(Stream: TStream);

  procedure WriteControlName(const ControlName: string);
  var
    NameLen: Integer;
    LBytes: TBytes;
  begin
    LBytes := TEncoding.UTF8.GetBytes(ControlName);
    NameLen := Length(LBytes);
    Stream.Write(NameLen, SizeOf(NameLen));
    if NameLen > 0 then
      Stream.Write(LBytes{$IFNDEF CLR}[0]{$ENDIF}, NameLen);
  end;

  procedure DoSaveZone(Zone: TDockZone; Level: Integer);
  var
    WrittenLimit: Integer;
    LOrientation: Integer;
  begin
    with Stream do
    begin
      { Don't write out "useless" zones. These have no children,
        or no control attached to them. Be sure to always write out the
        top zone, though. }
      if (Zone = FTopZone) or (Zone.FChildZones <> nil) or
        (Zone.FChildControl <> nil) then
      begin
        Write(Level, SizeOf(Level));
        LOrientation := Byte(Zone.FOrientation);
        Write(LOrientation, SizeOf(Zone.FOrientation));
        if FRelativeSizes then
          WrittenLimit := RelativeSize(Zone.FZoneLimit, ReferenceFromOrient(Zone.FOrientation))
        else
          WrittenLimit := Zone.FZoneLimit;
        Write(WrittenLimit, SizeOf(WrittenLimit));
        WriteControlName(Zone.GetControlName);
      end;
    end;
    // Recurse child
    if Zone.FChildZones <> nil then
      DoSaveZone(Zone.FChildZones, Level + 1);
    // Recurse sibling
    if Zone.FNextSibling <> nil then
      DoSaveZone(Zone.FNextSibling, Level);
  end;

var
  I, NVCount: Integer;
  Ctl: TControl;
  LBytes: array of Byte;
  NonVisList: TStringList;
begin
  // write stream version
  Stream.Write(FVersion, SizeOf(FVersion));
  { Write out the relative sizes option, and orientation for reference }
  SetLength(LBytes, 1);
  LBytes[0] := Byte(FRelativeSizes);
  Stream.Write(LBytes{$IFNDEF CLR}[0]{$ENDIF}, 1);
  LBytes[0] := Byte(FTopZone.FOrientation);
  Stream.Write(LBytes{$IFNDEF CLR}[0]{$ENDIF}, 1);
  // get list of non-visible dock clients
  NonVisList := TStringList.Create;
  try
    for I := 0 to FDockSite.DockClientCount - 1 do
    begin
      Ctl := FDockSite.DockClients[I];
      if (not Ctl.Visible) and (Ctl.Name <> '') then
        NonVisList.Add(Ctl.Name);
    end;
    // write non-visible dock client list
    NVCount := NonVisList.Count;
    Stream.Write(NVCount, SizeOf(NVCount));
    for I := 0 to NVCount - 1 do WriteControlName(NonVisList[I]);
  finally
    NonVisList.Free;
  end;
  // write top zone data
  if FRelativeSizes then
    I := RelativeSize(FTopXYLimit, ReferenceFromOrient(FTopZone.FOrientation))
  else
    I := FTopXYLimit;
  Stream.Write(I, SizeOf(I));
  // write all zones from tree
  DoSaveZone(FTopZone, 0);
  Stream.Write(TreeStreamEndFlag, SizeOf(TreeStreamEndFlag));
end;

procedure TDockTree.SetNewBounds(Zone: TDockZone);

  procedure DoSetNewBounds(Zone: TDockZone);
  begin
    if Zone <> nil then
    begin
      if (Zone.NextVisible = nil) and (Zone <> FTopZone) and (Zone.Visible) then
      begin
        if Zone.FParentZone = FTopZone then
          Zone.ZoneLimit := FTopXYLimit
        else
          Zone.ZoneLimit := Zone.FParentZone.FParentZone.ZoneLimit;
      end;
      DoSetNewBounds(Zone.FirstVisibleChild);
      DoSetNewBounds(Zone.NextVisible);
    end;
  end;

begin
  if Zone = nil then Zone := FTopZone.FChildZones;
  DoSetNewBounds(Zone);
  { Redraw client dock frames }
  FDockSite.Invalidate;
end;

procedure TDockTree.SetReplacingControl(Control: TControl);
begin
  FReplacementZone := FindControlZone(Control);
end;

procedure TDockTree.ShiftZone(Zone: TDockZone);
begin
  if (Zone <> nil) and (Zone <> FTopZone) and
     (Zone.FParentZone.FOrientation = FShiftScaleOrient) then
    Zone.ZoneLimit := Zone.ZoneLimit + FShiftBy;
end;

procedure TDockTree.SplitterMouseDown(OnZone: TDockZone; MousePos: TPoint);
begin
  FSizingZone := OnZone;
  Mouse.Capture := FDockSite.Handle;
  FSizingWnd := FDockSite.Handle;
  FSizingDC := GetDCEx(FSizingWnd, 0, DCX_CACHE or DCX_CLIPSIBLINGS or
    DCX_LOCKWINDOWUPDATE);
  FSizePos := MousePos;
  DrawSizeSplitter;
end;

procedure TDockTree.SplitterMouseUp;
begin
  Mouse.Capture := 0;
  DrawSizeSplitter;
  ReleaseDC(FSizingWnd, FSizingDC);
  if FSizingZone.FParentZone.FOrientation = doHorizontal then
    FSizingZone.ZoneLimit := FSizePos.y + (FBorderWidth div 2) else
    FSizingZone.ZoneLimit := FSizePos.x + (FBorderWidth div 2);
  SetNewBounds(FSizingZone.FParentZone);
  ForEachAt(FSizingZone.FParentZone, UpdateZone);
  FSizingZone := nil;
end;

procedure TDockTree.UpdateAll;
begin
  if (FUpdateCount = 0) and (FDockSite.DockClientCount > 0) then
    ForEachAt(nil, UpdateZone);
end;

procedure TDockTree.UpdateZone(Zone: TDockZone);
begin
  if FUpdateCount = 0 then Zone.Update;
end;

procedure TDockTree.WindowProc(var Message: TMessage);
begin
  WndProc(Message);
end;

procedure TDockTree.DrawSizeSplitter;
var
  R: TRect;
  PrevBrush: HBrush;
begin
  if FSizingZone <> nil then
  begin
    with R do
    begin
      if FSizingZone.FParentZone.FOrientation = doHorizontal then
      begin
        Left := FSizingZone.Left;
        Top := FSizePos.Y - (FBorderWidth div 2);
        Right := Left + FSizingZone.Width;
        Bottom := Top + FBorderWidth;
      end
      else begin
        Left := FSizePos.X - (FBorderWidth div 2);
        Top := FSizingZone.Top;
        Right := Left + FBorderWidth;
        Bottom := Top + FSizingZone.Height;
      end;
    end;
    PrevBrush := SelectObject(FSizingDC, FBrush.Handle);
    with R do
      PatBlt(FSizingDC, Left, Top, Right - Left, Bottom - Top, PATINVERT);
    SelectObject(FSizingDC, PrevBrush);
  end;
end;

function TDockTree.GetNextLimit(AZone: TDockZone): Integer;
var
  LimitResult: Integer;

  procedure DoGetNextLimit(Zone: TDockZone);
  begin
    if (Zone <> AZone) and
      (Zone.FParentZone.FOrientation = AZone.FParentZone.FOrientation) and
      (Zone.ZoneLimit > AZone.ZoneLimit) and ((Zone.FChildControl = nil) or
      ((Zone.FChildControl <> nil) and (Zone.FChildControl.Visible))) then
      LimitResult := Min(LimitResult, Zone.ZoneLimit);
    if Zone.FNextSibling <> nil then DoGetNextLimit(Zone.FNextSibling);
    if Zone.FChildZones <> nil then DoGetNextLimit(Zone.FChildZones);
  end;

begin
  if AZone.NextVisible <> nil then
    LimitResult := AZone.NextVisible.ZoneLimit
  else
    LimitResult := AZone.ZoneLimit + AZone.LimitSize;
  DoGetNextLimit(FTopZone.FChildZones);
  Result := LimitResult;
end;

procedure TDockTree.ControlVisibilityChanged(Control: TControl;
  Visible: Boolean);

  function GetDockAlign(Client, DropCtl: TControl): TAlign;
  var
    CRect, DRect: TRect;
  begin
    Result := alRight;
    if DropCtl <> nil then
    begin
      CRect := Client.BoundsRect;
      DRect := DropCtl.BoundsRect;
      if (CRect.Top <= DRect.Top) and (CRect.Bottom < DRect.Bottom) and
         (CRect.Right >= DRect.Right) then
        Result := alTop
      else if (CRect.Left <= DRect.Left) and (CRect.Right < DRect.Right) and
         (CRect.Bottom >= DRect.Bottom) then
        Result := alLeft
      else if CRect.Top >= ((DRect.Top + DRect.Bottom) div 2) then
        Result := alBottom;
    end;
  end;

  procedure HideZone(const Zone: TDockZone);
  begin
    if IsOrientationSet(Zone) then
      Zone.FOldSize := Zone.FZoneLimit - Zone.LimitBegin
    else
      Zone.FOldSize := 0;

    if Assigned(Zone.FParentZone) and not (Zone.FParentZone.Visible) then
      HideZone(Zone.FParentZone);
    { When hiding, increase ZoneLimit for the zone before us }
    if Zone.PrevVisible <> nil then
      Zone.PrevVisible.ExpandZoneLimit(Zone.FZoneLimit);
    ForEachAt(Zone.FParentZone, UpdateZone);
  end;

  procedure ShowZone(const Zone: TDockZone);
  var
    ResetAll: Boolean;
    MinSibSize: Integer;
  begin
    if Assigned(Zone.FParentZone) and (Zone.FParentZone <> FTopZone) and
       (Zone.FParentZone.VisibleChildCount = 1) then
      ShowZone(Zone.FParentZone);
    if (Zone.FParentZone.VisibleChildCount = 1) or (Zone.FOldSize = 0) then
      ResetAll := True
    else
    begin
      ResetAll := False;
      MinSibSize := FGrabberSize + FBorderWidth + 14;
      if (Zone.PrevVisible <> nil) then
        with Zone.PrevVisible do
        begin
          if ((ZoneLimit - LimitBegin) - Zone.FOldSize) < MinSibSize then
            { Resizing the previous sibling will make it too small, resize all }
            ResetAll := True
          else
          begin
            { Make room before us as needed }
            ZoneLimit := ZoneLimit - Zone.FOldSize;
            { and adjust our own zone limit to reflect the previous size }
            Zone.ZoneLimit := ZoneLimit + Zone.FOldSize;
            Zone.PrevVisible.ResetZoneLimits;
          end;
        end
      else if (Zone.NextVisible <> nil) then
      begin
        if (Zone.NextVisible.ZoneLimit - Zone.FOldSize) < MinSibSize then
          { Resizing the next sibling will make it too small, resize all }
          ResetAll := True
        else
        begin
          { Adjust zone limit to make room for controls following this one }
          Zone.ZoneLimit := Zone.LimitBegin + Zone.FOldSize;
          Zone.NextVisible.ResetZoneLimits;
        end;
      end;
    end;
    if ResetAll then
      Zone.FParentZone.ResetChildren;
    ForEachAt(Zone.FParentZone, UpdateZone);
  end;

var
  HitTest: Integer;
  CtlZone, DropCtlZone: TDockZone;
  DropCtl: TControl;
begin
  CtlZone := FindControlZone(Control);
  if Assigned(CtlZone) then
  begin
    if Visible then
      ShowZone(CtlZone)
    else
      HideZone(CtlZone);
    FDockSite.Invalidate;
  end
  { Existing control that was never docked, create a new dock zone for it }
  else if Visible then
  begin
    DropCtlZone := InternalHitTest(Point(Control.Left, Control.Top), HitTest);
    if DropCtlZone <> nil then
      DropCtl := DropCtlZone.FChildControl
    else
      DropCtl := nil;
    InsertControl(Control, GetDockAlign(Control, DropCtl), DropCtl);
  end;
end;

[SecurityPermission(SecurityAction.InheritanceDemand, UnmanagedCode=True)]
procedure TDockTree.WndProc(var Message: TMessage);
const
  SizeCursors: array[TDockOrientation] of TCursor = (crDefault, crVSplit, crHSplit);
var
  TempZone: TDockZone;
  P: TPoint;
  HitTestValue: Integer;
  Handled: Boolean;
{$IF DEFINED(CLR)}
  S: string;
  R: TRect;
  HI: THintInfo;
{$IFEND}
begin
  Handled := False;
  case Message.Msg of
    CM_DOCKNOTIFICATION:
      with TCMDockNotification(Message) do
        if (NotifyRec.ClientMsg = CM_VISIBLECHANGED) then
          ControlVisibilityChanged(Client, Boolean(NotifyRec.MsgWParam));
    WM_MOUSEMOVE:
      with TWMMouse(Message) do
        MouseMove(KeysToShiftState(Keys), XPos, YPos, Handled);
    WM_LBUTTONDBLCLK:
      with TWMMouse(Message) do
        MouseDown(mbLeft, KeysToShiftState(Keys) + [ssDouble], XPos, YPos, Handled);
    WM_RBUTTONDBLCLK:
      with TWMMouse(Message) do
        MouseDown(mbRight, KeysToShiftState(Keys) + [ssDouble], XPos, YPos, Handled);
    WM_LBUTTONDOWN:
      with TWMMouse(Message) do
        MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos, Handled);
    WM_RBUTTONDOWN:
      with TWMMouse(Message) do
        MouseDown(mbRight, KeysToShiftState(Keys), XPos, YPos, Handled);
    WM_LBUTTONUP:
      with TWMMouse(Message) do
        MouseUp(mbLeft, KeysToShiftState(Keys), XPos, YPos, Handled);
    WM_RBUTTONUP:
      with TWMMouse(Message) do
        MouseUp(mbRight, KeysToShiftState(Keys), XPos, YPos, Handled);
    WM_SETCURSOR:
      begin
        GetCursorPos(P);
        P := FDockSite.ScreenToClient(P);
        with TWMSetCursor(Message) do
          if (Smallint(HitTest) = HTCLIENT) and (CursorWnd = FDockSite.Handle)
            and (FDockSite.VisibleDockClientCount > 0) then
          begin
            TempZone := InternalHitTest(P, HitTestValue);
            if (TempZone <> nil) and (HitTestValue = HTBORDER) then
            begin
              Winapi.Windows.SetCursor(Screen.Cursors[SizeCursors[TempZone.FParentZone.FOrientation]]);
              Message.Result := 1;
              Handled := True;
            end;
          end;
      end;
    CM_HINTSHOW:
{$IF DEFINED(CLR)}
      with TCMHintShow.Create(Message) do
      begin
        if Assigned(FOldWndProc) then
          FOldWndProc(Message);
        if Message.Result = 0 then
        begin
          HI := HintInfo;
          R := HI.CursorRect;
          S := HI.HintStr;
          ShowHint(HintInfo.CursorPos, R, S);
          HI.CursorRect := R;
          HI.HintStr := S;
          HintInfo := HI;
        end;
        Handled := True;
      end;
{$ELSE}
      with TCMHintShow(Message) do
      begin
        FOldWndProc(Message);
        if Result = 0 then
          ShowHint(HintInfo.CursorPos, HintInfo.CursorRect, HintInfo.HintStr);
        Handled := True;
      end;
{$IFEND}
  end;
  if (not Handled) and Assigned(FOldWndProc) then
    FOldWndProc(Message);
end;

const
  cPercentageOffset = 10000;

function TDockTree.ActualSize(const RelativeSize, Reference: Integer): Integer;
begin
  Result := Round(RelativeSize / cPercentageOffset * Reference);
end;

function TDockTree.RelativeSize(const ActualSize, Reference: Integer): Integer;
begin
  if Reference <> 0 then
    Result := Round(ActualSize / Reference * cPercentageOffset)
  else
    Result := 0;
end;

function TDockTree.ReferenceFromOrient(const Orient: TDockOrientation): Integer;
begin
  if Orient = doHorizontal then
    Result := Screen.PrimaryMonitor.Width
  else
     Result := Screen.PrimaryMonitor.Height;
end;

procedure TDockTree.AdjustFrameRect(Control: TControl; var ARect: TRect);
var
  Temp: Integer;
begin
  { Adjust the frame rect based on the given control and dock rect (ARect) }
  ARect.Left := ARect.Left -  2 * (ARect.Left - Control.Left);
  ARect.Top := ARect.Top - 2 * (ARect.Top - Control.Top);
  ARect.Right := ARect.Right -  2 * (Control.Width - (ARect.Right - ARect.Left));
  Temp := 2 * (Control.Height - (ARect.Bottom - ARect.Top));
  if Temp > 0 then
    ARect.Bottom := ARect.Bottom - Temp;
end;

procedure TDockTree.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer; var Handled: Boolean);
var
  TempZone: TDockZone;
  HitTestValue: Integer;
  Msg: TMsg;
begin
  if Button = mbLeft then
  begin
    if ssDouble in Shift then
    begin
      TempZone := InternalHitTest(Point(X, Y), HitTestValue);
      if TempZone <> nil then
      with TempZone do
        if (FChildControl <> nil) and (HitTestValue = HTCAPTION) then
        begin
          CancelDrag;
          FChildControl.ManualDock(nil, nil, alTop);
        end;
    end
    else
    begin
      TempZone := InternalHitTest(Point(X, Y), HitTestValue);
      if (TempZone <> nil) then
      begin
        if HitTestValue = HTBORDER then
          SplitterMouseDown(TempZone, Point(X, Y))
        else if HitTestValue = HTCAPTION then
        begin
          if (not PeekMessage(Msg, FDockSite.Handle, WM_LBUTTONDBLCLK,
             WM_LBUTTONDBLCLK, PM_NOREMOVE)) and
             (TempZone.FChildControl is TWinControl) then
            TWinControl(TempZone.FChildControl).SetFocus;

          if (TempZone.FChildControl.DragKind = dkDock) and
             (TempZone.FChildControl.DragMode = dmAutomatic) then
            TempZone.FChildControl.BeginDrag(False);
          Handled := True; { Don't let the control get the message }
        end;
      end;
    end;
  end;
end;

procedure TDockTree.MouseMove(Shift: TShiftState; X, Y: Integer;
  var Handled: Boolean);

  procedure CalcSplitterPos;
  var
    MinWidth,
    TestLimit: Integer;
  begin
    MinWidth := FGrabberSize;
    if (FSizingZone.FParentZone.FOrientation = doHorizontal) then
    begin
      TestLimit := FSizingZone.Top + MinWidth;
      if FSizePos.y <= TestLimit then FSizePos.y := TestLimit;
      TestLimit := GetNextLimit(FSizingZone) - MinWidth;
      if FSizePos.y >= TestLimit then FSizePos.y := TestLimit;
    end
    else begin
      TestLimit := FSizingZone.Left + MinWidth;
      if FSizePos.x <= TestLimit then FSizePos.x := TestLimit;
      TestLimit := GetNextLimit(FSizingZone) - MinWidth;
      if FSizePos.x >= TestLimit then FSizePos.x := TestLimit;
    end;
  end;

begin
  if FSizingZone <> nil then
  begin
    DrawSizeSplitter;
    FSizePos := Point(X, Y);
    CalcSplitterPos;
    DrawSizeSplitter;
    { Don't set Handled }
  end;
end;

procedure TDockTree.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer; var Handled: Boolean);
var
  TempZone: TDockZone;
  HitTestValue: Integer;
begin
  if Button = mbLeft then
  begin
    if FSizingZone = nil then
    begin
      TempZone := InternalHitTest(Point(X,Y), HitTestValue);
      if (TempZone <> nil) and (HitTestValue = HTCLOSE) then
      begin
        if TempZone.FChildControl is TCustomForm then
          TCustomForm(TempZone.FChildControl).Close
        else
          TempZone.FChildControl.Visible := False;
      end;
    end
    else
      SplitterMouseUp;
  end;
end;

procedure TDockTree.ShowHint(CursorPos: TPoint; var CursorRect: TRect; var HintStr: string);
var
  Control: TControl;
  HitTestValue: Integer;
  R: TRect;
begin
  Control := HitTest(CursorPos, HitTestValue);
  if HitTestValue = HTBORDER then
    HintStr := ''
  else if (Control <> nil) and (HitTestValue in [HTCAPTION, HTCLOSE]) then
  begin
    R := Control.BoundsRect;
    AdjustDockRect(Control, R);
    Dec(R.Left, 2 * (R.Left - Control.Left));
    Dec(R.Top, 2 * (R.Top - Control.Top));
    Dec(R.Right, 2 * (Control.Width - (R.Right - R.Left)));
    Dec(R.Bottom, 2 * (Control.Height - (R.Bottom - R.Top)));
    HintStr := Control.Caption;
    CursorRect := R;
  end;
end;

{ TMouse }

constructor TMouse.Create;
begin
  inherited Create;
  FDragImmediate := True;
  FDragThreshold := 5;
  // Mouse wheel is natively supported on Windows 98 and higher
  // and Windows NT 4.0 and higher.
  FNativeWheelSupport :=
    ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 4)) or
    ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
    ((Win32MajorVersion > 4) or
    ((Win32MajorVersion = 4) and (Win32MinorVersion >= 10))));
  SettingChanged(0);
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
function TMouse.CreatePanningWindow: TCustomPanningWindow;
begin
  Result := nil;
  if Assigned(FPanningWindowClass) and WheelPresent then
  begin
    PanningWindow := nil;
    Result := FPanningWindowClass.Create(nil);
    FPanningWindow := Result;
  end;
end;

destructor TMouse.Destroy;
begin
  Capture := 0;
  inherited Destroy;
end;

function TMouse.GetCapture: HWND;
begin
  Result := Winapi.Windows.GetCapture;
end;

function TMouse.GetCursorPos: TPoint;
begin
  if not Winapi.Windows.GetCursorPos(Result) then
    Result := Point(0, 0);
end;

function TMouse.GetIsDragging: Boolean;
begin
  Result := ActiveDrag <> dopNone;
end;

function TMouse.GetIsPanning: Boolean;
begin
  Result := (FPanningWindow <> nil) and FPanningWindow.GetIsPanning;
end;

procedure TMouse.GetMouseData;
begin
  FMousePresent := BOOL(GetSystemMetrics(SM_MOUSEPRESENT));
end;

procedure TMouse.GetNativeData;
begin
  FWheelPresent := BOOL(GetSystemMetrics(SM_MOUSEWHEELPRESENT));
  if FWheelPresent then
    SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, FScrollLines, 0);
end;

procedure TMouse.GetRegisteredData;
var
  HasWheel: BOOL;
begin
  FWheelHwnd := HwndMsWheel(FWheelMessage, FWheelSupportMessage,
    FScrollLinesMessage, HasWheel, FScrollLines);
  FWheelPresent := FWheelMessage <> 0;
end;

procedure TMouse.SetCapture(const Value: HWND);
begin
  if Capture <> Value then
  begin
    if Value = 0 then ReleaseCapture
    else Winapi.Windows.SetCapture(Value);
  end;
end;

procedure TMouse.SetCursorPos(const Value: TPoint);
begin
  Winapi.Windows.SetCursorPos(Value.X, Value.Y);
end;

procedure TMouse.SetPanningWindow(const Value: TCustomPanningWindow);
begin
  if FPanningWindow <> Value then
  begin
    if Assigned(FPanningWindow) then
      FreeAndNil(FPanningWindow);
    FPanningWindow := Value;
  end;
end;

procedure TMouse.SettingChanged(Setting: Integer);
begin
  case Setting of
    0:
      begin
        GetMouseData;
        if not FNativeWheelSupport then GetRegisteredData
        else GetNativeData;
      end;
    SPI_GETWHEELSCROLLLINES:
      if FWheelPresent then
      begin
        if FNativeWheelSupport then
          SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, FScrollLines, 0)
        else
          FScrollLines := SendMessage(FWheelHwnd, FScrollLinesMessage, 0, 0)
      end;
  end;
end;

{ Input Method Editor (IME) support code }

var
  IMM32DLL: THandle = 0;

const
  Imm32ModName = 'imm32.dll';

procedure InitIMM32;
var
  OldError: Longint;
begin
  if not Syslocale.FarEast then Exit;
  OldError := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  try
    if IMM32DLL = 0 then
      IMM32DLL := LoadLibrary(Imm32ModName);
  finally
    SetErrorMode(OldError);
  end;
end;

function _WINNLSEnableIME(hWnd: HWND; Enable: BOOL): BOOL; stdcall;
  external user32 name 'WINNLSEnableIME' delayed;

function Win32NLSEnableIME(hWnd: HWND; Enable: Boolean): Boolean;
begin
  if IMM32DLL <> 0 then
    Result := _WINNLSEnableIME(hWnd, Enable)
  else
    Result := False;
end;

procedure SetImeMode(hWnd: HWND; Mode: TImeMode);
const
  ModeMap: array [imSAlpha..imHanguel] of Byte =  // flags in use are all < 255
    ( { imSAlpha: } IME_CMODE_ALPHANUMERIC,
      { imAlpha:  } IME_CMODE_ALPHANUMERIC or IME_CMODE_FULLSHAPE,
      { imHira:   } IME_CMODE_NATIVE or IME_CMODE_FULLSHAPE,
      { imSKata:  } IME_CMODE_NATIVE or IME_CMODE_KATAKANA,
      { imKata:   } IME_CMODE_NATIVE or IME_CMODE_KATAKANA or IME_CMODE_FULLSHAPE,
      { imChinese:} IME_CMODE_NATIVE or IME_CMODE_FULLSHAPE,
      { imSHanguel} IME_CMODE_NATIVE,
      { imHanguel } IME_CMODE_NATIVE or IME_CMODE_FULLSHAPE );
var
  IMC: HIMC;
  Conv, Sent: DWORD;
begin
  if (not SysLocale.FarEast) or (Mode = imDontCare) then Exit;

  if Mode = imDisable then
  begin
    if TOSVersion.Check(5, 1) then
      ImmAssociateContext(hWnd, 0)
    else
      Win32NLSEnableIME(hWnd, FALSE);
    Exit;
  end;

  Win32NLSEnableIME(hWnd, TRUE);

  if IMM32DLL = 0 then Exit;

  IMC := ImmGetContext(hWnd);
  if IMC = 0 then Exit;

  ImmGetConversionStatus(IMC, Conv, Sent);

  case Mode of
    imClose: ImmSetOpenStatus(IMC, FALSE);
    imOpen : ImmSetOpenStatus(IMC, TRUE);
  else
    ImmSetOpenStatus(IMC, TRUE);
    ImmGetConversionStatus(IMC, Conv, Sent);
    Conv := Conv and (not(IME_CMODE_LANGUAGE or IME_CMODE_FULLSHAPE)) or ModeMap[Mode];
  end;
  ImmSetConversionStatus(IMC, Conv, Sent);
  ImmReleaseContext(hWnd, IMC);
end;

procedure SetImeName(Name: TImeName);
var
  I: Integer;
  HandleToSet: HKL;
begin
  if not SysLocale.FarEast then Exit;
  if (Name <> '') and (Screen.Imes.Count <> 0) then
  begin
    HandleToSet := Screen.DefaultKbLayout;
    I := Screen.Imes.IndexOf(Name);
    if I >= 0 then HandleToSet := HKL(Screen.Imes.Objects[I]);
    ActivateKeyboardLayout(HandleToSet, KLF_ACTIVATE);
  end;
end;

function Imm32GetContext(hWnd: HWND): HIMC;
begin
  if IMM32DLL <> 0 then
    Result := ImmGetContext(hWnd)
  else
    Result := 0;
end;

function Imm32ReleaseContext(hWnd: HWND; hImc: HIMC): Boolean;
begin
  if IMM32DLL <> 0 then
    Result := ImmReleaseContext(hWnd, hImc)
  else
    Result := False;
end;

function Imm32GetConversionStatus(hImc: HIMC; var Conversion, Sentence: DWORD): Boolean;
begin
  if IMM32DLL <> 0 then
    Result := ImmGetConversionStatus(hImc, Conversion, Sentence)
  else
    Result := False;
end;

function Imm32SetConversionStatus(hImc: HIMC; Conversion, Sentence: DWORD): Boolean;
begin
  if IMM32DLL <> 0 then
    Result := ImmSetConversionStatus(hImc, Conversion, Sentence)
  else
    Result := False;
end;

function Imm32SetOpenStatus(hImc: HIMC; fOpen: Boolean): Boolean;
begin
  if IMM32DLL <> 0 then
    Result := ImmSetOpenStatus(hImc, fOpen)
  else
    Result := False;
end;

function Imm32SetCompositionWindow(hImc: HIMC; lpCompForm: TCOMPOSITIONFORM): Boolean;
begin
  if IMM32DLL <> 0 then
    Result := ImmSetCompositionWindow(hImc, @lpCompForm)
  else
    Result := False;
end;

{$IF NOT DEFINED(CLR)}
function Imm32SetCompositionWindow(hImc: HIMC; lpCompForm: PCOMPOSITIONFORM): Boolean;
begin
  Result := Imm32SetCompositionWindow(hImc, lpCompForm^)
end;
{$IFEND}

function Imm32SetCompositionFont(hImc: HIMC; var lpLogfont: LOGFONT): Boolean;
begin
  if IMM32DLL <> 0 then
    Result := ImmSetCompositionFont(hImc, @lpLogFont)
  else
    Result := False;
end;

{$IF NOT DEFINED(CLR)}
function Imm32SetCompositionFont(hImc: HIMC; lpLogfont: PLOGFONT): Boolean;
begin
  Result := Imm32SetCompositionFont(hImc, lpLogFont^)
end;
{$IFEND}

{$IF DEFINED(CLR)}
function Imm32GetCompositionString(hImc: HIMC; dWord1: DWORD; lpBuf: IntPtr; dwBufLen: DWORD): Longint;
{$ELSE}
function Imm32GetCompositionString(hImc: HIMC; dWord1: DWORD; lpBuf: pointer; dwBufLen: DWORD): Longint;
{$IFEND}
begin
  if IMM32DLL <> 0 then
    Result := ImmGetCompositionString(hImc, dWord1, lpBuf, dwBufLen)
  else
    Result := 0;
end;

function Imm32IsIME(hKl: HKL): Boolean;
begin
  if IMM32DLL <> 0 then
    Result := ImmIsIME(hKl)
  else
    Result := False;
end;

function Imm32NotifyIME(hImc: HIMC; dwAction, dwIndex, dwValue: DWORD): Boolean;
begin
  if IMM32DLL <> 0 then
    Result := ImmNotifyIME(hImc, dwAction, dwIndex, dwValue)
  else
    Result := False;
end;

{ Modal result testers }

function IsPositiveResult(const AModalResult: TModalResult): Boolean;
begin
  Result := System.UITypes.IsPositiveResult(AModalResult);
end;

function IsNegativeResult(const AModalResult: TModalResult): Boolean;
begin
  Result := System.UITypes.IsNegativeResult(AModalResult);
end;

function IsAbortResult(const AModalResult: TModalResult): Boolean;
begin
  Result := System.UITypes.IsAbortResult(AModalResult);
end;

function IsAnAllResult(const AModalResult: TModalResult): Boolean;
begin
  Result := System.UITypes.IsAnAllResult(AModalResult);
end;

function StripAllFromResult(const AModalResult: TModalResult): TModalResult;
begin
  Result := System.UITypes.StripAllFromResult(AModalResult);
end;

{ Initialization and cleanup }

procedure DoneControls;
begin
  FreeAndNil(FlagControl);
  Application.Free;
  Application := nil;
  Screen.Free;
  Screen := nil;
  Mouse.Free;
  Mouse := nil;
  CanvasList.Free;
  TStyleManager.UnInitialize;
{$IF DEFINED(CLR)}
  CanvasList := nil;
  IMM32Handle.Close;
{$ELSE}
  GlobalDeleteAtom(ControlAtom);
  ControlAtomString := '';
  GlobalDeleteAtom(WindowAtom);
  WindowAtomString := '';
  if IMM32DLL <> 0 then
    FreeLibrary(IMM32DLL);
{$IFEND}
end;

{$IF DEFINED(CLR)}
procedure AppDomainUnload(Sender: System.Object; E: EventArgs);
begin
  DoneControls;
end;
{$IFEND}

[SecurityPermission(SecurityAction.Assert, UnmanagedCode=True)]
procedure InitControls;
var
  UserHandle: HMODULE;
begin
{$IF NOT DEFINED(CLR)}
  WindowAtomString := Format('Delphi%.8X',[GetCurrentProcessID]);
  WindowAtom := GlobalAddAtom(PChar(WindowAtomString));
  ControlAtomString := Format('ControlOfs%.8X%.8X', [HInstance, GetCurrentThreadID]);
  ControlAtom := GlobalAddAtom(PChar(ControlAtomString));
  // WORKAROUND
  // This is fixed in XE3 and so this whole copy of the unit can be replaced,
  // after we upgrade to XE3
  RM_GetObjectInstance := RegisterWindowMessage('RM_GetObjectInstance');
{$IFEND}
  CanvasList := TThreadList.Create;
  InitIMM32;
  Mouse := TMouse.Create;
  Screen := TScreen.Create(nil);
  TStyleManager.Initialize;
  Application := TApplication.Create(nil);
  Application.ShowHint := True;
  FlagControl := TControl.Create(nil);
  UserHandle := GetModuleHandle('USER32');
{$IF DEFINED(CLR)}
  //CLR: This requires the same permissions as ProcessExit
  Include(AppDomain.CurrentDomain.DomainUnload, AppDomainUnload);
  RegisterIntegerConsts(TypeOf(TCursor), IdentToCursor, CursorToIdent);
  if UserHandle <> 0 then
  begin
    BindProcAddress(AnimateWindowProc, TypeOf(TAnimateWindowProc), UserHandle, 'AnimateWindow');
    SupportsAnimateWindow := Assigned(AnimateWindowProc);
  end;
{$ELSE}
  if not Assigned(FindIntToIdent(TypeInfo(TCursor))) then
    RegisterIntegerConsts(TypeInfo(TCursor), IdentToCursor, CursorToIdent);
//  RegisterCursorIntegerConsts;
  if UserHandle <> 0 then
    @AnimateWindowProc := GetProcAddress(UserHandle, 'AnimateWindow');
{$IFEND}
end;

{ TCustomListControl }

constructor TCustomListControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Touch.InteractiveGestures := [igPan, igPressAndTap];
  Touch.InteractiveGestureOptions := [igoPanInertia,
    igoPanSingleFingerHorizontal, igoPanSingleFingerVertical,
    igoPanGutter, igoParentPassthrough];
end;

function TCustomListControl.IsTouchPropertyStored(AProperty: TTouchProperty): Boolean;
begin
  Result := inherited IsTouchPropertyStored(AProperty);
  case AProperty of
    tpInteractiveGestures:
      Result := Touch.InteractiveGestures <> [igPan, igPressAndTap];
    tpInteractiveGestureOptions:
      Result := Touch.InteractiveGestureOptions <> [igoPanInertia,
        igoPanSingleFingerHorizontal, igoPanSingleFingerVertical,
        igoPanGutter, igoParentPassthrough];
  end;
end;

procedure TCustomListControl.MoveSelection(Destination: TCustomListControl);
begin
  CopySelection(Destination);
  DeleteSelected;
end;

{ TCustomControlAction }

procedure TCustomControlAction.SetDropdownMenu(Value: TPopupMenu);
var
  I: Integer;
  Link: TControlActionLink;
begin
  if FDropdownMenu <> Value then
  begin
    if FDropdownMenu <> nil then
      FDropdownMenu.RemoveFreeNotification(Self);
    for I := 0 to FClients.Count - 1 do
      if TObject(FClients[I]) is TControlActionLink then
      begin
        Link := TControlActionLink(FClients[I]);
        if Assigned(Link) then
          Link.SetDropdownMenu(Value);
      end;
    FDropdownMenu := Value;
    if FDropdownMenu <> nil then
      FDropdownMenu.FreeNotification(Self);
    Change;
  end;
end;

procedure TCustomControlAction.SetEnableDropdown(Value: Boolean);
var
  I: Integer;
  Link: TControlActionLink;
begin
  if FEnableDropdown <> Value then
  begin
    for I := 0 to FClients.Count - 1 do
      if TObject(FClients[I]) is TControlActionLink then
      begin
        Link := TControlActionLink(FClients[I]);
        if Assigned(Link) then
          Link.SetEnableDropdown(Value);
      end;
    FEnableDropdown := Value;
    Change;
  end;
end;

procedure TCustomControlAction.SetPopupMenu(Value: TPopupMenu);
var
  I: Integer;
  Link: TControlActionLink;
begin
  if FPopupMenu <> Value then
  begin
    if FPopupMenu <> nil then
      FPopupMenu.RemoveFreeNotification(Self);
    for I := 0 to FClients.Count - 1 do
      if TObject(FClients[I]) is TControlActionLink then
      begin
        Link := TControlActionLink(FClients[I]);
        if Assigned(Link) then
          Link.SetPopupMenu(Value);
      end;
    FPopupMenu := Value;
    if FPopupMenu <> nil then
      FPopupMenu.FreeNotification(Self);
    Change;
  end;
end;

{ TCustomTransparentControl }

constructor TCustomTransparentControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Brush.Style := bsClear;
end;

procedure TCustomTransparentControl.InvalidateControlsUnderneath;
var
  I: Integer;
  Invalidating: Boolean;
  Control: TControl;

  procedure DoInvalidate(AControl: TControl);
  var
    I: Integer;
    Control: TControl;
  begin
    if AControl is TWinControl then
    begin
      if TWinControl(AControl).HandleAllocated then
        with TWinControl(AControl) do
        begin
          RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_FRAME);
          InvalidateRect(Handle, nil, True);
        end;
      if (csAcceptsControls in AControl.ControlStyle) then
        for I := 0 to TWinControl(AControl).ControlCount - 1 do
        begin
          Control := TWinControl(AControl).Controls[I];
          DoInvalidate(Control);
        end;
    end else
      AControl.Invalidate;
  end;

begin
  Invalidating := False;
  if HandleAllocated then
  begin
    for I := Parent.ControlCount - 1 downto 0 do
    begin
      Control := Parent.Controls[I];
      if Invalidating then
        DoInvalidate(Control)
      else if Control = Self then
        Invalidating := True;
    end;
    InvalidateRect(Parent.Handle, nil, True);
  end;
end;

procedure TCustomTransparentControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

procedure TCustomTransparentControl.WMNCHitTest(var Message: TWMNCHitTest);
begin
  if not FInterceptMouse then
    Message.Result := HTTRANSPARENT
  else
    inherited;
end;

procedure TCustomTransparentControl.Invalidate;
begin
  InvalidateControlsUnderneath;
  inherited Invalidate;
end;

{ TMargins }

constructor TMargins.Create(Control: TControl);
begin
  inherited Create;
  FControl := Control;
  InitDefaults(Self);
end;

procedure TMargins.AssignTo(Dest: TPersistent);
begin
  if Dest is TMargins then
    with TMargins(Dest) do
    begin
      FLeft := Self.FLeft;
      FTop := Self.FTop;
      FRight := Self.FRight;
      FBottom := Self.FBottom;
      Change;
    end
  else
    inherited;
end;

procedure TMargins.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

class procedure TMargins.InitDefaults(Margins: TMargins);
begin
  with Margins do
  begin
    FLeft := 3;
    FRight := 3;
    FTop := 3;
    FBottom := 3;
  end;
end;

procedure TMargins.SetMargin(Index: Integer; Value: TMarginSize);
begin
  case Index of
    0:
      if Value <> FLeft then
      begin
        FLeft := Value;
        Change;
      end;
    1:
      if Value <> FTop then
      begin
        FTop := Value;
        Change;
      end;
    2:
      if Value <> FRight then
      begin
        FRight := Value;
        Change;
      end;
    3:
      if Value <> FBottom then
      begin
        FBottom := Value;
        Change;
      end;
  end;
end;

procedure TMargins.SetControlBounds(ALeft, ATop, AWidth, AHeight: Integer; Aligning: Boolean);
begin
  if Control <> nil then
  begin
    if Aligning then
    begin
      Control.FAnchorMove := True;
      Include(Control.FControlState, csAligning);
    end;
    try
      if Control.AlignWithMargins and (Control.Parent <> nil) then
        Control.SetBounds(ALeft + FLeft, ATop + FTop, AWidth - (FLeft + FRight), AHeight - (FTop + FBottom))
      else
        Control.SetBounds(ALeft, ATop, AWidth, AHeight);
    finally
      if Aligning then
      begin
        Control.FAnchorMove := False;
        Exclude(Control.FControlState, csAligning);
      end;
    end;
  end;
end;

procedure TMargins.SetControlBounds(const ARect: TRect; Aligning: Boolean);
begin
  SetControlBounds(ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, Aligning);
end;

procedure TMargins.SetBounds(ALeft, ATop, ARight, ABottom: Integer);
begin
  if (FLeft <> ALeft) or (FTop <> ATop) or (FRight <> ARight) or (FBottom <> ABottom) then
  begin
    FLeft := ALeft;
    FTop := ATop;
    FRight := ARight;
    FBottom := ABottom;
    Change;
  end;
end;

function TMargins.GetControlBound(Index: Integer): Integer;
begin
  Result := 0;
  if FControl <> nil then
    case Index of
      0:
        if FControl.AlignWithMargins and (FControl.Parent <> nil) then
          Result := FControl.Left - FLeft
        else
          Result := FControl.Left;
      1:
        if FControl.AlignWithMargins and (FControl.Parent <> nil) then
          Result := FControl.Top - FTop
        else
          Result := FControl.Top;
      2:
        if FControl.AlignWithMargins and (FControl.Parent <> nil) then
          Result := FControl.Width + FLeft + FRight
        else
          Result := FControl.Width;
      3:
        if FControl.AlignWithMargins and (FControl.Parent <> nil) then
          Result := FControl.Height + FTop + FBottom
        else
          Result := FControl.Height;
      4:
        if FControl.AlignWithMargins and (FControl.Parent <> nil) then
          Result := FControl.ExplicitLeft - FLeft
        else
          Result := FControl.ExplicitLeft;
      5:
        if FControl.AlignWithMargins and (FControl.Parent <> nil) then
          Result := FControl.ExplicitTop - FTop
        else
          Result := FControl.ExplicitTop;
      6:
        if FControl.AlignWithMargins and (FControl.Parent <> nil) then
          Result := FControl.ExplicitWidth + FLeft + FRight
        else
          Result := FControl.ExplicitWidth;
      7:
        if FControl.AlignWithMargins and (FControl.Parent <> nil) then
          Result := FControl.ExplicitHeight + FTop + FBottom
        else
          Result := FControl.ExplicitHeight;
    end;
end;

{ TPadding }

class procedure TPadding.InitDefaults(Margins: TMargins);
begin
  { Zero initialization is sufficient here }
end;

{ TCustomHintWindow }

procedure TCustomHintWindow.AutoSize;
begin
  HintParent.SetHintSize(Self);
end;

procedure TCustomHintWindow.CMTextChanged(var Message: TMessage);
begin
  inherited;
end;

constructor TCustomHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Visible := False;
  Color := $80FFFF;
  Canvas.Font := Screen.HintFont;
  Canvas.Brush.Style := bsClear;
  Width := 0;
  Height := 0;
end;

procedure TCustomHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
  end;
end;

procedure TCustomHintWindow.CreateWnd;
begin
  inherited;
  if StyleServices.Enabled then
  begin
    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
    SetLayeredWindowAttributes(Handle, $0000FF00, 0, LWA_COLORKEY or LWA_ALPHA);
  end;
end;

function TCustomHintWindow.IsThemed: Boolean;
begin
  Result := StyleServices.Enabled and CheckWin32Version(6) and StyleServices.IsSystemStyle;
end;

function TCustomHintWindow.NewStylePainting: Boolean;
begin
  Result := CheckWin32Version(5, 1) and StyleServices.Enabled;
end;

procedure TCustomHintWindow.NCPaint(DC: HDC);
begin
  HintParent.NCPaintHint(Self, DC);
end;

procedure TCustomHintWindow.Paint;
begin
  HintParent.PaintHint(Self);
end;

procedure TCustomHintWindow.PositionAtCursor;
var
  Pos: TPoint;
begin
  GetCursorPos(Pos);
  PositionAt(Pos);
end;

procedure TCustomHintWindow.PositionAt(Rect: TRect);
begin
  AutoSize;

  Top := Rect.Bottom;
  Left := Rect.Left + RectWidth(Rect) div 2 - (Width) div 2;

  if HintParent.Style = bhsBalloon then
    Left := Left + cBalloonStemHeight;

  FPopAbove := Top > Screen.Height div 2;
  if FPopAbove then
    Top := Top - Height - RectHeight(Rect);
end;

procedure TCustomHintWindow.PositionAt(Point: TPoint);
begin
  PositionAt(Rect(Point.X, Point.Y, Point.X, Point.Y));
end;

procedure TCustomHintWindow.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := HTTRANSPARENT;
end;

procedure TCustomHintWindow.WMNCPaint(var Message: TWMNCPaint);
var
  DC: HDC;
begin
  DC := GetWindowDC(Handle);
  try
    NCPaint(DC);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TCustomHintWindow.WMPrint(var Message: TMessage);
begin
  PaintTo(Message.WParam, 0, 0);
  NCPaint(Message.WParam);
end;

{ TCustomHint }

procedure TCustomHint.SetHintSize(HintWindow: TCustomHintWindow);
var
  LWidth, LHeight: Integer;
  TextWidth, TextHeight: Integer;
  MeasureRect: TRect;
  WasBold: Boolean;
begin
  with HintWindow do
  begin
    LWidth := 0;
    LHeight := 0;
    TextWidth := 0;
    TextHeight := 0;

    with HintParent do
    begin
      if Title <> '' then
      begin
        WasBold := fsBold in Canvas.Font.Style;
        Canvas.Font.Style := Canvas.Font.Style + [fsBold];
        Canvas.TextRect(MeasureRect, FTitle, [tfLeft, tfCalcRect]);
        if not WasBold then
          Canvas.Font.Style := Canvas.Font.Style - [fsBold];
        TextWidth := RectWidth(MeasureRect) + cTextHorizontalMargin * 2;
        TextHeight := TextHeight + RectHeight(MeasureRect) + cTextVerticalMargin;
      end;

      if Description <> '' then
      begin
        Canvas.TextRect(MeasureRect, FDescription, [tfLeft, tfCalcRect]);
        TextWidth := Max(TextWidth, RectWidth(MeasureRect) + cTextHorizontalMargin * 2);
        TextHeight := TextHeight + RectHeight(MeasureRect) + cTextVerticalMargin;
      end;
      LHeight := LHeight + TextHeight;
      LWidth := LWidth + TextWidth;
    end;
    HintWindow.Height := LHeight;
    HintWindow.Width := LWidth;
  end;
end;

procedure TCustomHint.SetImages(Value: TImageList);
begin
  if Value <> FImages then
  begin
    FImages := Value;
    if Images <> nil then
      Images.FreeNotification(Self);
  end;
end;

constructor TCustomHint.Create(AOwner: TComponent);
begin
  inherited;
  FHideAfter := -1;
  FStyle := bhsBalloon;
  FAnimateThread := nil;
  FShowDelay := 500;
  FShow := True;
end;

destructor TCustomHint.Destroy;
begin
  HideHint;
  FWorkComplete := True;
  if FAnimateThread <> nil then
  begin
    FAnimateThread.ResumeWork;
    FAnimateThread.Terminate;
    FAnimateThread.Free;
    FAnimateThread := nil;
  end;
  inherited;
end;

procedure TCustomHint.HideHint(HidingControl: TControl);
begin
  if FLatestHintControl = HidingControl then
    HideHint;
end;

procedure TCustomHint.HideHint;
begin
  FShow := False;
end;

procedure TCustomHint.NCPaintHint(HintWindow: TCustomHintWindow; DC: HDC);
begin
  // do nothing
end;

procedure TCustomHint.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Images then
      Images := nil;
  end;
end;

procedure TCustomHint.PaintHint(HintWindow: TCustomHintWindow);
var
  CRect, TopText, BottomText: TRect;
  Region, OldRegion: HRGN;
  TextSize: TRect;
begin
  with HintWindow do
  begin
    CRect := SplitRect(ClientRect, srBottom, Height);
    Canvas.Brush.Color := $F0FFFF;
    Canvas.FillRect(ClientRect);
    Region := CreateRectRgn(CRect.Left, CRect.Top, CRect.Right, CRect.Bottom);
    OldRegion := SelectObject(Canvas.Handle, Region);
    try
      Canvas.Brush.Color := clBlack;
      FrameRgn(Canvas.Handle, Region, Canvas.Brush.Handle, 1, 1);
      if FDescription <> '' then
        TopText := SplitRect(CRect, srTop, 0.50)
      else
        TopText := CRect;
      if FTitle <> '' then
        BottomText := SplitRect(CRect, srBottom, 0.50)
      else
        BottomText := CRect;
      Canvas.Brush.Style := bsClear;
      Canvas.TextRect(TextSize, FTitle, [tfCalcRect]);
      TopText := CenteredRect(TopText, TextSize);
      Canvas.TextRect(TopText, FTitle, [tfLeft, tfTop, tfWordBreak, tfNoClip]);
      Canvas.TextRect(TextSize, FDescription, [tfCalcRect]);
      BottomText := CenteredRect(BottomText, TextSize);
      Canvas.TextRect(BottomText, FDescription, [tfLeft,  tfTop, tfWordBreak, tfNoClip]);
    finally
      SelectObject(Canvas.Handle, OldRegion);
      DeleteObject(Region);
    end;
  end;
end;

procedure TCustomHint.ShowAnotherHint;
begin
  FShow := True;
end;

procedure TCustomHint.ShowHint(Control: TControl);
var
  Pos: TPoint;
  Index: Integer;
begin
  if Control = nil then
    Exit;

  if Control.CustomHint = Self then
  begin
    if Control.Hint = '' then
      Exit;

    Index := AnsiPos('|', Control.Hint); //Do Not Localize
    Title := GetShortHint(Control.Hint);
    if Index <> 0 then
      Description := GetLongHint(Control.Hint)
    else
      Description := '';

    Index := AnsiPos('|', Description); //Do Not Localize
    if Index <> 0 then
    begin
      ImageIndex := StrToInt(Copy(Description, Index + 1, MaxInt));
      Description := Copy(Description, 0, Index - 1);
    end
    else
      ImageIndex := -1;

    FLatestHintControl := Control;
    GetCursorPos(Pos);
  end
  else
    Pos := Control.ClientToScreen(Point(Control.Width div 2, Control.Height));

  ShowHint(Pos);
end;

procedure TCustomHint.ShowHint(Rect: TRect);
var
  Hint: TCustomHintWindow;
begin
  FShow := True;
  Hint := TCustomHintWindow.Create(nil);
  Hint.HintParent := Self;
  Hint.HandleNeeded;
  Hint.FTitle := Title;
  Hint.FDescription := Description;
  Hint.FImageIndex := ImageIndex;
  Hint.PositionAt(Rect);

  FWorkComplete := False;
  if FAnimateThread = nil then
  begin
    FAnimateThread := TCustomHintShowHideThread.Create(Hint, Self);
  end
  else
  begin
    FAnimateThread.QueHintWindow(Hint);
    FAnimateThread.ResumeWork;
  end;
end;

procedure TCustomHint.ShowHint(Point: TPoint);
begin
  ShowHint(Rect(Point.X, Point.Y, Point.X, Point.Y));
end;

procedure TCustomHint.ShowHint;
begin
  ShowHint(Point(0, 0));
end;

{ TCustomHintShowHideThread }

constructor TCustomHintShowHideThread.Create(Hint: TCustomHintWindow; HintObject: TCustomHint);
begin
  inherited Create(False);
  FWaitEvent := TEvent.Create;{(nil, False, False, 'BalloonHintWaitEvent');}
  FHintWindowQueue := TThreadList.Create;
  FHintWindowQueue.Duplicates := dupAccept;
  QueHintWindow(Hint);
  FHideHint := False;
  FActive := True;
  FHintObject := HintObject;
end;

destructor TCustomHintShowHideThread.Destroy;
var
  I: Integer;
begin
  FActive := False;
  ResumeWork;
  inherited;
  with FHintWindowQueue.LockList do
  try
    for I := 0 to Count - 1 do
      TObject(Items[I]).Free;
  finally
    FHintwindowQueue.UnlockList;
  end;
  FHintWindowQueue.Clear;
  FHintWindowQueue.Free;
  FWaitEvent.Free;
end;

procedure TCustomHintShowHideThread.Execute;
const
  cFadeFrames = 10;
  cFadeMSPF = 20;
var
  I: Integer;
  LHintParent: TCustomHint;
  LHintWindow: TCustomHintWindow;
  ShowAnotherWindow: Boolean;
  FirstShow: Boolean;
  ListCount: Integer;

  function LatestHintWindow: TCustomHintWindow;
  var
    I: Integer;
    LCount: Integer;
    FreeList: TList;
  begin
    Result := nil;
    FreeList := TList.Create;
    try
      with FHintWindowQueue.LockList do
      try
        LCount := Count;
        for I := 0 to LCount - 1 do
        begin
          if I = LCount - 1 then
            Result := TCustomHintWindow(Items[0])
          else
          begin
            FreeList.Add(Items[0]);
            Delete(0);
          end;
        end;
      finally
        FHintWindowQueue.UnlockList;
      end;

      for I := 0 to FreeList.Count - 1 do
        Synchronize(TCustomHintWindow(FreeList[I]).Free);
    finally
      FreeList.Free;
    end;
  end;

begin
  while FActive do
  begin
    FirstShow := True;
    LHintWindow := LatestHintWindow;
    LHintParent := FHintObject;
    if LHintWindow <> nil then
    begin
      with LHintWindow do
      begin
        try
          if HandleAllocated then
            SetLayeredWindowAttributes(Handle, $0000FF00, 0, LWA_ALPHA or LWA_COLORKEY);
          Sleep(HintParent.Delay);

          if HintParent.ShowingHint then
          begin
            while not Terminated do
            begin
              ShowAnotherWindow := False;
              if HandleAllocated then
              begin
                Synchronize(LHintWindow.Paint);
                ShowWindow(Handle, SW_SHOWNOACTIVATE);
              end;

              if FirstShow then
              begin
                FirstShow := False;
                for I := 1 to cFadeFrames do
                begin
                  if HandleAllocated then
                    SetLayeredWindowAttributes(Handle, $0000FF00, Trunc((I / cFadeFrames) * 255), LWA_ALPHA or LWA_COLORKEY);

                  with FHintWindowQueue.Locklist do
                  try
                    ListCount := Count;
                  finally
                    FHintWindowQueue.UnlockList;
                  end;
                  if ListCount > 1 then
                  begin
                    LHintWindow := LatestHintWindow;
                    ShowAnotherWindow := True;
                    LHintParent.ShowAnotherHint;
                    Break;
                  end;
                  Sleep(cFadeMSPF);
                end;
              end;

              if ShowAnotherWindow then
                Continue;
              FDisplayTime := GetTickCount;
              if HandleAllocated then
                SetLayeredWindowAttributes(Handle, $0000FF00, 255, LWA_ALPHA or LWA_COLORKEY);
              while HintParent.ShowingHint do
              begin
                Sleep(cFadeMSPF);
                with FHintWindowQueue.Locklist do
                try
                  ListCount := Count;
                finally
                  FHintWindowQueue.UnlockList;
                end;
                if ListCount > 1 then
                begin
                  LHintWindow := LatestHintWindow;
                  ShowAnotherWindow := True;
                  LHintParent.ShowAnotherHint;
                  Break;
                end;
                if HintParent.HideAfter <> -1 then
                begin
                  if Cardinal(HintParent.HideAfter) < GetTickCount - FDisplayTime then
                    HintParent.HideHint;
                end
              end;

              if ShowAnotherWindow then
                Continue;

              for I := cFadeFrames downto 0 do
              begin
                if HandleAllocated then
                  SetLayeredWindowAttributes(Handle, $0000FF00, Trunc((I / cFadeFrames) * 255), LWA_ALPHA or LWA_COLORKEY);
                with FHintWindowQueue.Locklist do
                try
                  ListCount := Count;
                finally
                  FHintWindowQueue.UnlockList;
                end;
                if ListCount > 1 then
                begin
                  LHintWindow := LatestHintWindow;
                  ShowAnotherWindow := True;
                  LHintParent.ShowAnotherHint;
                  Break;
                end;
                Sleep(cFadeMSPF);
              end;

              if ShowAnotherWindow then
                Continue;

              with FHintWindowQueue.Locklist do
              try
                ListCount := Count;
              finally
                FHintWindowQueue.UnlockList;
              end;
              if ListCount = 1 then
                Break;
            end;
          end;
        finally
          Synchronize(Free);
          with FHintWindowQueue.Locklist do
          try
            Delete(0);
          finally
            FHintWindowQueue.UnlockList;
          end;
        end;
      end;
    end;

    if (not LHintParent.FWorkComplete) and (not Application.Terminated) then
    begin
      FWaitEvent.ResetEvent;
      FWaitEvent.WaitFor(INFINITE);
    end;
  end;
end;

procedure TCustomHintShowHideThread.HideHint;
begin
  FHideHint := True;
end;

procedure TCustomHintShowHideThread.QueHintWindow(Value: TCustomHintWindow);
begin
  FHintWindowQueue.Add(Value);
end;

procedure TCustomHintShowHideThread.ResumeWork;
begin
  FWaitEvent.SetEvent;
end;

{ TBalloonHint }

procedure TBalloonHint.SetHintSize(HintWindow: TCustomHintWindow);
var
  LWidth, LHeight: Integer;
  ImageHeight: Integer;
  TextWidth, TextHeight: Integer;
  MeasureRect: TRect;
  Theme: HTHEME;
  WasBold: Boolean;
begin
  with HintWindow do
  begin
    if not NewStylePainting then
    begin
      inherited SetHintSize(HintWindow);
      Exit;
    end;

    LWidth := 0;
    TextWidth := 0;
    TextHeight := 0;
    ImageHeight := 0;

    if HintParent.Style = bhsBalloon then
      LHeight := cBalloonStemHeight
    else
      LHeight := 0;

    Theme := StyleServices.Theme[teToolTip];

    with HintParent do
    begin
      if (Images <> nil) and (ImageIndex <> -1) then
      begin
        LWidth := LWidth + Images.Width + cImageMargin;
        ImageHeight := Images.Height + cImageMargin;
      end;

      if Title <> '' then
      begin
        if IsThemed then
          GetThemeTextExtent(Theme, Canvas.Handle, TTP_STANDARDTITLE, TTSS_NORMAL, {$IFNDEF CLR}PWideChar{$ENDIF}(UnicodeString(Title)), -1, 0, {$IFNDEF CLR}nil{$ELSE}Rect(0, 0, 0, 0){$ENDIF}, MeasureRect)
        else
        begin
          WasBold := fsBold in Canvas.Font.Style;
          Canvas.Font.Style := Canvas.Font.Style + [fsBold];
          Canvas.TextRect(MeasureRect, FTitle, [tfLeft, tfCalcRect]);
          if not WasBold then
            Canvas.Font.Style := Canvas.Font.Style - [fsBold];
        end;
        TextWidth := RectWidth(MeasureRect) + cTextHorizontalMargin * 2;
        TextHeight := TextHeight + RectHeight(MeasureRect) + cTextVerticalMargin;
      end;

      if Description <> '' then
      begin
        if IsThemed then
          GetThemeTextExtent(Theme, Canvas.Handle, TTP_STANDARD, TTSS_NORMAL, {$IFNDEF CLR}PWideChar{$ENDIF}(UnicodeString(Description)), Length(Description), 0, {$IFNDEF CLR}nil{$ELSE}Rect(0, 0, 0, 0){$ENDIF}, MeasureRect)
        else
        begin
          Canvas.TextRect(MeasureRect, FDescription, [tfLeft, tfCalcRect]);
        end;
        TextWidth := Max(TextWidth, RectWidth(MeasureRect) + cTextHorizontalMargin * 2);
        TextHeight := TextHeight + RectHeight(MeasureRect) + cTextVerticalMargin;
      end;

      //Add some space for the non-themed painting since Canvas.TextRect returns
      //a slightly different result then the themed GetThemeTextExtent
      if not IsThemed then
      begin
        TextWidth := TextWidth + 4;
        TextHeight := TextHeight + 4;
      end;

      LHeight := LHeight + Max(ImageHeight, TextHeight);
      LWidth := LWidth + TextWidth;
    end;

    HintWindow.Height := LHeight;
    HintWindow.Width := LWidth;
  end;
end;

procedure TBalloonHint.PaintHint(HintWindow: TCustomHintWindow);
var
  Theme: HTHEME;
  CRect, TopText, BottomText, MeasureRect, ImageRect: TRect;
  Region, Bubble, Stem, OldRegion: HRGN;
  StemPts: array[0..2] of TPoint;
  Details: TThemedElementDetails;
  TextSize: TRect;
  FontWasBold: Boolean;
  OldFontColor, LColor: TColor;
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  BkColor, GradientStartColor, GradientEndColor, TextColor: TColor;
begin
  Stem := 0;

  with HintWindow do
  begin
    if not NewStylePainting then
    begin
      inherited PaintHint(HintWindow);
      Exit;
    end;

    if HintParent.Style = bhsBalloon then
    begin
      if FPopAbove then
        CRect := SplitRect(ClientRect, srTop, Height - cBalloonStemHeight)
      else
        CRect := SplitRect(ClientRect, srBottom, Height - cBalloonStemHeight);
    end
    else
    begin
      CRect := SplitRect(ClientRect, srBottom, Height);
    end;

    Canvas.Brush.Color := clLime;
    Canvas.FillRect(ClientRect);

    if not IsThemed then // Only use "themed" painting for Windows theme
    begin
      Bubble := CreateRoundRectRgn(CRect.Left, CRect.Top, CRect.Right, CRect.Bottom, cEdgeRadius, cEdgeRadius);

      if HintParent.Style = bhsBalloon then
      begin
        if FPopAbove then
        begin
          MeasureRect := CenteredRect(SplitRect(ClientRect, srBottom, cBalloonStemHeight + 1), Rect(0, 0, cBalloonStemHeight, cBalloonStemHeight));
          StemPts[0] := MeasureRect.TopLeft;
          StemPts[1] := Point(MeasureRect.Right, MeasureRect.Top);
          StemPts[2] := Point(MeasureRect.Left, MeasureRect.Bottom);
          Stem := CreatePolygonRgn(StemPts, 3, WINDING);
        end
        else
        begin
          MeasureRect := CenteredRect(SplitRect(ClientRect, srTop, cBalloonStemHeight), Rect(0, 0, cBalloonStemHeight, cBalloonStemHeight));
          StemPts[0] := MeasureRect.TopLeft;
          StemPts[1] := MeasureRect.BottomRight;
          StemPts[2] := Point(MeasureRect.Left, MeasureRect.Bottom);
          Stem := CreatePolygonRgn(StemPts, 3, WINDING)
        end;

        Region := CreateRectRgn(0, 0, 1, 1);
        CombineRgn(Region, Bubble, Stem, RGN_OR);

        OldRegion := SelectObject(Canvas.Handle, Region);
      end
      else
      begin
        OldRegion := SelectObject(Canvas.Handle, Bubble);
        Region := Bubble;
      end;

      BkColor := $00767676;
      GradientStartColor := clWhite;
      GradientEndColor := $EFE4E3;
      TextColor := $00575757;
      LStyle := StyleServices;
      if LStyle.Enabled then
      begin
        LDetails := LStyle.GetElementDetails(thHintBalloon);
        if LStyle.GetElementColor(LDetails, ecBorderColor, LColor) and (LColor <> clNone) then
          BkColor := LColor;
        if LStyle.GetElementColor(LDetails, ecGradientColor1, LColor) and (LColor <> clNone) then
          GradientStartColor := LColor;
        if LStyle.GetElementColor(LDetails, ecGradientColor2, LColor) and (LColor <> clNone) then
          GradientEndColor := LColor;
        if LStyle.GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
          TextColor := LColor;
      end;

      GradientFillCanvas(Canvas, GradientStartColor, GradientEndColor, ClientRect, gdVertical);

      Canvas.Brush.Color := BkColor;
      FrameRgn(Canvas.Handle, Region, Canvas.Brush.Handle, 1, 1);

      if (HintParent.Images <> nil) and (FImageIndex <> -1) then
      begin
        ImageRect := SplitRect(CRect, srLeft, HintParent.Images.Width + cImageMargin * 2);
        ImageRect := CenteredRect(ImageRect, Rect(0, 0, HintParent.Images.Width, HintParent.Images.Height));
        CRect := SplitRect(CRect, srRight, RectWidth(CRect) - (HintParent.Images.Width + cImageMargin));
      end;

      if (HintParent.Images <> nil) and (FImageIndex <> -1) then
        HintParent.Images.Draw(Canvas, ImageRect.Left, ImageRect.Top, HintParent.ImageIndex);

      if FDescription <> '' then
        TopText := SplitRect(CRect, srTop, 0.50)
      else
        TopText := CRect;

      if FTitle <> '' then
        BottomText := SplitRect(CRect, srBottom, 0.50)
      else
        BottomText := CRect;

      Canvas.Brush.Style := bsClear;

      FontWasBold := fsBold in Canvas.Font.Style;
      OldFontColor := Canvas.Font.Color;
      Canvas.Font.Color := TextColor;

      Canvas.Font.Style := Canvas.Font.Style + [fsBold];
      Canvas.TextRect(TextSize, FTitle, [tfCalcRect]);
      TopText := CenteredRect(TopText, TextSize);
      Canvas.TextRect(TopText, FTitle, [tfLeft, tfTop]);

      Canvas.Font.Style := Canvas.Font.Style - [fsBold];
      Canvas.TextRect(TextSize, FDescription, [tfCalcRect]);
      BottomText := CenteredRect(BottomText, TextSize);
      Canvas.TextRect(BottomText, FDescription, [tfLeft, tfWordBreak]);

      if FontWasBold then
        Canvas.Font.Style := Canvas.Font.Style + [fsBold];
      Canvas.Font.Color := OldFontColor;

      SelectObject(Canvas.Handle, OldRegion);

      if HintParent.Style = bhsBalloon then
      begin
        DeleteObject(Stem);
        DeleteObject(Region);
      end;
      DeleteObject(Bubble);
    end
    else
    begin
      Theme := StyleServices.Theme[teToolTip];
      GetThemeBackgroundRegion(Theme, Canvas.Handle, TTP_STANDARD, TTSS_NORMAL, CRect, Region);
      Canvas.Brush.Color := clWhite;
      FillRgn(Canvas.Handle, Region, Canvas.Brush.Handle);
      DeleteObject(Region);

      Details := StyleServices.GetElementDetails(tttStandardNormal);
      StyleServices.DrawElement(Canvas.Handle, Details, CRect);

      if HintParent.Style = bhsBalloon then
      begin
        if FPopAbove then
        begin
          MeasureRect := SplitRect(ClientRect, srBottom, cBalloonStemHeight + 1);
          DrawThemeBackground(Theme, Canvas.Handle, TTP_BALLOONSTEM, TTBSS_POINTINGDOWNLEFTWALL, SplitRect(ClientRect, srBottom, cBalloonStemHeight + 11), {$IFNDEF CLR}@{$ENDIF}MeasureRect) //This +11 is a vista hack till I can find how to measure the Stem properly
        end
        else
        begin
          MeasureRect := SplitRect(ClientRect, srTop, cBalloonStemHeight + 1);
          DrawThemeBackground(Theme, Canvas.Handle, TTP_BALLOONSTEM, TTBSS_POINTINGUPLEFTWALL, SplitRect(ClientRect, srTop, cBalloonStemHeight + 1), {$IFNDEF CLR}@{$ENDIF}MeasureRect);
        end;
      end;

      if (HintParent.Images <> nil) and (FImageIndex <> -1) then
      begin
        ImageRect := SplitRect(CRect, srLeft, HintParent.Images.Width + cImageMargin);
        ImageRect := CenteredRect(ImageRect, Rect(0, 0, HintParent.Images.Width, HintParent.Images.Height));
        CRect := SplitRect(CRect, srRight, RectWidth(CRect) - (HintParent.Images.Width + cImageMargin));
      end;

      if FDescription <> '' then
        TopText := SplitRect(CRect, srTop, 0.50)
      else
        TopText := CRect;

      if FTitle <> '' then
        BottomText := SplitRect(CRect, srBottom, 0.50)
      else
        BottomText := CRect;

      GetThemeTextExtent(Theme, Canvas.Handle, TTP_STANDARDTITLE, TTSS_NORMAL, {$IFNDEF CLR}PWideChar{$ENDIF}(UnicodeString(FTitle)), -1, 0, {$IFNDEF CLR}nil{$ELSE}Rect(0, 0, 0, 0){$ENDIF}, MeasureRect);
      TopText := CenteredRect(TopText, MeasureRect);
      TopText.Left := CRect.Left + cTextHorizontalMargin;
      TopText.Right := CRect.Right - cTextHorizontalMargin;

      GetThemeTextExtent(Theme, Canvas.Handle, TTP_STANDARD, TTSS_NORMAL, {$IFNDEF CLR}PWideChar{$ENDIF}(UnicodeString(FDescription)), -1, 0, {$IFNDEF CLR}nil{$ELSE}Rect(0, 0, 0, 0){$ENDIF}, MeasureRect);
      BottomText := CenteredRect(BottomText, MeasureRect);
      BottomText.Left := CRect.Left + cTextHorizontalMargin;
      BottomText.Right := CRect.Right - cTextHorizontalMargin;

      Details := StyleServices.GetElementDetails(tttStandardTitleNormal);
      StyleServices.DrawText(Canvas.Handle, Details, FTitle, TopText, []);
      Details := StyleServices.GetElementDetails(tttStandardNormal);
      StyleServices.DrawText(Canvas.Handle, Details, FDescription, BottomText, []);

      if (HintParent.Images <> nil) and (FImageIndex <> -1) then
        HintParent.Images.Draw(Canvas, ImageRect.Left, ImageRect.Top, FImageIndex);
    end;
  end;
end;

{ TCustomGestureCollection }

function TCustomGestureCollection.GetItem(Index: Integer): TCustomGestureCollectionItem;
begin
  Result := TCustomGestureCollectionItem(inherited GetItem(Index));
end;

procedure TCustomGestureCollection.SetItem(Index: Integer; const Value: TCustomGestureCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TCustomGestureEngine }

class function TCustomGestureEngine.Supported: Boolean;
begin
  Result := False;
end;

{ TCustomTouchManager }

constructor TCustomTouchManager.Create(AControl: TControl);
begin
  inherited Create;
  FControl := AControl;
  FGestureEngine := nil;
  FGestureManager := nil;
  FNotifyList := TList.Create;
  FNotifyList.Add(AControl);
end;

destructor TCustomTouchManager.Destroy;
begin
  if FGestureManager <> nil then
    SetGestureManager(nil);
  FreeAndNil(FNotifyList);
  inherited;
end;

procedure TCustomTouchManager.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomTouchManager then
    with TCustomTouchManager(Dest) do
    begin
      FControl := Self.FControl;
      FGestureManager := Self.FGestureManager;
      FGestureEngine := Self.FGestureEngine;
      FInteractiveGestures := Self.FInteractiveGestures;
      FInteractiveGestureOptions := Self.FInteractiveGestureOptions;
      FNotifyList.Assign(Self.FNotifyList);
      FStandardGestures := Self.FStandardGestures;
      FTabletOptions := Self.FTabletOptions;
    end
  else
    inherited;
end;

procedure TCustomTouchManager.ChangeNotification(AControl: TControl);
begin
  if AControl <> nil then
    FNotifyList.Add(AControl);
end;

function TCustomTouchManager.FindGesture(AGestureID: TGestureID): TCustomGestureCollectionItem;
begin
  Result := nil;
  if FGestureManager <> nil then
    Result := FGestureManager.FindGesture(FControl, AGestureID);
end;

function TCustomTouchManager.FindGesture(const AName: string): TCustomGestureCollectionItem;
begin
  Result := nil;
  if FGestureManager <> nil then
    Result := FGestureManager.FindGesture(FControl, AName);
end;

function TCustomTouchManager.GetGestureList: TGestureArray;
begin
  Result := nil;
  if FGestureManager <> nil then
    Result := FGestureManager.GestureList[FControl];
end;

function TCustomTouchManager.GetStandardGestures: TStandardGestures;
begin
  if FGestureManager <> nil then
    FStandardGestures := FGestureManager.StandardGestures[FControl];
  Result := FStandardGestures;
end;

function TCustomTouchManager.IsDefault: Boolean;
begin
  Result := (FGestureManager = nil) and
            not IsInteractiveGestureOptionsStored and
            not IsInteractiveGesturesStored and
            not IsParentTabletOptionsStored and
            not IsTabletOptionsStored;
end;

function TCustomTouchManager.IsInteractiveGestureOptionsStored: Boolean;
begin
  Result := FControl.IsTouchPropertyStored(tpInteractiveGestureOptions);
end;

function TCustomTouchManager.IsInteractiveGesturesStored: Boolean;
begin
  Result := FControl.IsTouchPropertyStored(tpInteractiveGestures);
end;

function TCustomTouchManager.IsParentTabletOptionsStored: Boolean;
begin
  Result := FControl.IsTouchPropertyStored(tpParentTabletOptions);
end;

function TCustomTouchManager.IsTabletOptionsStored: Boolean;
begin
  Result := FControl.IsTouchPropertyStored(tpTabletOptions);
end;

procedure TCustomTouchManager.RemoveChangeNotification(AControl: TControl);
var
  I: Integer;
begin
  I := FNotifyList.IndexOf(AControl);
  if I <> -1 then
    FNotifyList.Remove(AControl);
end;

function TCustomTouchManager.SelectGesture(AGestureID: TGestureID): Boolean;
begin
  Result := False;
  if FGestureManager <> nil then
    Result := FGestureManager.SelectGesture(FControl, AGestureID);
end;

function TCustomTouchManager.SelectGesture(const AName: string): Boolean;
begin
  Result := False;
  if FGestureManager <> nil then
    Result := FGestureManager.SelectGesture(FControl, AName);
end;

procedure TCustomTouchManager.SetGestureEngine(const Value: TCustomGestureEngine);
begin
  if (Value <> FGestureEngine) and (FControl is TWinControl) then
  begin
    FGestureEngine := Value;
    if TWinControl(FControl).HandleAllocated and (csGestures in FControl.ControlStyle) then
      FGestureEngine.Active := True;
  end;
end;

procedure TCustomTouchManager.SetGestureManager(const Value: TCustomGestureManager);
var
  I: Integer;
begin
  if Value <> FGestureManager then
  begin
    if FGestureManager <> nil then
    begin
      FGestureManager.RemoveFreeNotification(FControl);
      FGestureManager.UnregisterControl(FControl);
      FGestureManager := nil; // Must be set to nil before calling RegisterControl!!
    end;
    if Value <> nil then
    begin
      Value.FreeNotification(FControl);
      Value.RegisterControl(FControl);
    end;
    FGestureManager := Value; // Must be assigned after registering the control!!
    for I := 0 to FNotifyList.Count - 1 do
      TControl(FNotifyList[I]).Perform(CM_GESTUREMANAGERCHANGED, 0, 0);
  end;
end;

procedure TCustomTouchManager.SetParentTabletOptions(const Value: Boolean);
begin
  if Value <> FParentTabletOptions then
  begin
    FParentTabletOptions := Value;
    if (FControl.FParent <> nil) and not (csReading in FControl.ComponentState) then
      FControl.Perform(CM_PARENTTABLETOPTIONSCHANGED, 0, 0);
  end;
end;

procedure TCustomTouchManager.SetStandardGestures(const Value: TStandardGestures);
begin
  if Value <> FStandardGestures then
  begin
    FStandardGestures := Value;
    if FGestureManager <> nil then
      FGestureManager.StandardGestures[FControl] := FStandardGestures;
  end;
end;

procedure TCustomTouchManager.SetTabletOptions(const Value: TTabletOptions);
begin
  if Value <> FTabletOptions then
  begin
    FTabletOptions := Value;
    FParentTabletOptions := False;
    FControl.Perform(CM_TABLETOPTIONSCHANGED, 0, 0);
  end;
end;

procedure TCustomTouchManager.UnselectGesture(AGestureID: TGestureID);
begin
  if FGestureManager <> nil then
    FGestureManager.UnselectGesture(FControl, AGestureID);
end;

initialization
  NewStyleControls := LoByte(LoWord(GetVersion)) >= 4;
  InitControls;
{$IF DEFINED(CLR)}
  NullDockSite := TWinControl.Create(nil);
{$ELSE}
  StartClassGroup(TControl);
  ActivateClassGroup(TControl);
  GroupDescendentsWith(TCustomImageList, TControl);
  GroupDescendentsWith(TContainedAction, TControl);
  GroupDescendentsWith(TCustomActionList, TControl);

finalization
  FreeAndNil(DockSiteList);
  DoneControls;
{$IFEND}
end.
