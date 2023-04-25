{ MP }
unit TB2Item;

{
  Toolbar2000
  Copyright (C) 1998-2005 by Jordan Russell
  All rights reserved.

  The contents of this file are subject to the "Toolbar2000 License"; you may
  not use or distribute this file except in compliance with the
  "Toolbar2000 License". A copy of the "Toolbar2000 License" may be found in
  TB2k-LICENSE.txt or at:
    https://jrsoftware.org/files/tb2k/TB2k-LICENSE.txt

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License (the "GPL"), in which case the provisions of the
  GPL are applicable instead of those in the "Toolbar2000 License". A copy of
  the GPL may be found in GPL-LICENSE.txt or at:
    https://jrsoftware.org/files/tb2k/GPL-LICENSE.txt
  If you wish to allow use of your version of this file only under the terms of
  the GPL and not to allow others to use your version of this file under the
  "Toolbar2000 License", indicate your decision by deleting the provisions
  above and replace them with the notice and other provisions required by the
  GPL. If you do not delete the provisions above, a recipient may use your
  version of this file under either the "Toolbar2000 License" or the GPL.

  $jrsoftware: tb2k/Source/TB2Item.pas,v 1.277 2005/06/23 21:55:44 jr Exp $
}

interface

{$I TB2Ver.inc}
{x$DEFINE TB2K_NO_ANIMATION}
  { Enabling the above define disables all menu animation. For debugging
    purpose only. }
{x$DEFINE TB2K_USE_STRICT_O2K_MENU_STYLE}
  { Enabling the above define forces it to use clBtnFace for the menu color
    instead of clMenu, and disables the use of flat menu borders on Windows
    XP with themes enabled. }

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CommCtrl, Menus, ActnList, ImgList, TB2Anim, UITypes;

const
  WM_TB2K_POPUPSHOWING = WM_USER + 554;

  { Parameter in LParam of WM_TB2K_POPUPSHOWING }
  TPS_ANIMSTART     = 1;   // animation query: if Result <> 0, do not animate!
  TPS_ANIMFINISHED  = 2;   // only fired when animation thread is done
  TPS_NOANIM        = 3;   // fired when animation is done, or if showing with no animation

type
  TTBCustomItem = class;
  TTBCustomItemClass = class of TTBCustomItem;
  TTBCustomItemActionLink = class;
  TTBCustomItemActionLinkClass = class of TTBCustomItemActionLink;
  TTBItemViewer = class;
  TTBItemViewerClass = class of TTBItemViewer;
  TTBPopupWindow = class;
  TTBPopupWindowClass = class of TTBPopupWindow;
  TTBView = class;

  TTBDoneAction = (tbdaNone, tbdaCancel, tbdaClickItem, tbdaOpenSystemMenu,
    tbdaHelpContext { MP }, tbdaHelpKeyword { /MP });
  PTBDoneActionData = ^TTBDoneActionData;
  TTBDoneActionData = record
    DoneAction: TTBDoneAction;
    case TTBDoneAction of
      tbdaClickItem: (ClickItem: TTBCustomItem; Sound: Boolean);
      tbdaOpenSystemMenu: (Wnd: HWND; Key: Cardinal);
      tbdaHelpContext: (ContextID: Integer);
      { MP }
      tbdaHelpKeyword: (HelpKeyword: String[100]);
  end;
  TTBInsertItemProc = procedure(AParent: TComponent; AItem: TTBCustomItem) of object;
  TTBItemChangedAction = (tbicInserted, tbicDeleting, tbicSubitemsChanged,
    tbicSubitemsBeginUpdate, tbicSubitemsEndUpdate, tbicInvalidate,
    tbicInvalidateAndResize, tbicRecreateItemViewers, tbicNameChanged,
    tbicSubMenuImagesChanged);
  TTBItemChangedProc = procedure(Sender: TTBCustomItem; Relayed: Boolean;
    Action: TTBItemChangedAction; Index: Integer; Item: TTBCustomItem) of object;
  TTBItemData = record
    Item: TTBCustomItem;
  end;
  PTBItemDataArray = ^TTBItemDataArray;
  TTBItemDataArray = array[0..$7FFFFFFF div SizeOf(TTBItemData)-1] of TTBItemData;
  TTBItemDisplayMode = (nbdmDefault, nbdmTextOnly, nbdmTextOnlyInMenus, nbdmImageAndText);
  TTBItemOption = (tboDefault, tboDropdownArrow, tboImageAboveCaption,
    tboLongHintInMenuOnly, tboNoAutoHint, tboNoRotation, tboSameWidth,
    tboShowHint, tboToolbarStyle, tboToolbarSize);
  TTBItemOptions = set of TTBItemOption;
  TTBItemStyle = set of (tbisSubmenu, tbisSelectable, tbisSeparator,
    tbisEmbeddedGroup, tbisClicksTransparent, tbisCombo, tbisNoAutoOpen,
    tbisSubitemsEditable, tbisNoLineBreak, tbisRightAlign, tbisDontSelectFirst,
    tbisRedrawOnSelChange, tbisRedrawOnMouseOverChange, tbisStretch);
  TTBPopupAlignment = (tbpaLeft, tbpaRight, tbpaCenter);
  TTBPopupEvent = procedure(Sender: TTBCustomItem; FromLink: Boolean) of object;
  TTBSelectEvent = procedure(Sender: TTBCustomItem; Viewer: TTBItemViewer;
    Selecting: Boolean) of object;

  ETBItemError = class(Exception);

  TTBImageChangeLink = class(TChangeLink)
  private
    FLastWidth, FLastHeight: Integer;
  end;
  TTBPopupPositionRec = record
    PositionAsSubmenu: Boolean;
    Alignment: TTBPopupAlignment;
    Opposite: Boolean;
    MonitorRect: TRect;
    ParentItemRect: TRect;
    NCSizeX: Integer;
    NCSizeY: Integer;
    X, Y, W, H: Integer;
    AnimDir: TTBAnimationDirection;
    PlaySound: Boolean;
  end;

  TTBCustomItem = class(TComponent)
  private
    FActionLink: TTBCustomItemActionLink;
    FAutoCheck: Boolean;
    FCaption: String;
    FChecked: Boolean;
    FDisplayMode: TTBItemDisplayMode;
    FEnabled: Boolean;
    FEffectiveOptions: TTBItemOptions;
    FGroupIndex: Integer;
    FHelpContext: THelpContext;
    { MP }
    FHelpKeyword: String;
    FHint: String;
    FImageIndex: TImageIndex;
    FImages: TCustomImageList;
    FImagesChangeLink: TTBImageChangeLink;
    FItems: PTBItemDataArray;
    FItemCount: Integer;
    FItemStyle: TTBItemStyle;
    FLinkParents: TList;
    FMaskOptions: TTBItemOptions;
    FOptions: TTBItemOptions;
    FInheritOptions: Boolean;
    FNotifyList: TList;
    FOnClick: TNotifyEvent;
    FOnPopup: TTBPopupEvent;
    FOnSelect: TTBSelectEvent;
    FParent: TTBCustomItem;
    FParentComponent: TComponent;
    FRadioItem: Boolean;
    FShortCut: TShortCut;
    FSubMenuImages: TCustomImageList;
    FSubMenuImagesChangeLink: TTBImageChangeLink;
    FLinkSubitems: TTBCustomItem;
    FVisible: Boolean;

    procedure DoActionChange(Sender: TObject);
    function ChangeImages(var AImages: TCustomImageList;
      const Value: TCustomImageList; var AChangeLink: TTBImageChangeLink): Boolean;
    class procedure ClickWndProc(var Message: TMessage);
    function FindItemWithShortCut(AShortCut: TShortCut;
      var ATopmostParent: TTBCustomItem): TTBCustomItem;
    function FixOptions(const AOptions: TTBItemOptions): TTBItemOptions;
    function GetAction: TBasicAction;
    function GetItem(Index: Integer): TTBCustomItem;
    procedure ImageListChangeHandler(Sender: TObject);
    procedure InternalNotify(Ancestor: TTBCustomItem; NestingLevel: Integer;
      Action: TTBItemChangedAction; Index: Integer; Item: TTBCustomItem);
    function IsAutoCheckStored: Boolean;
    function IsCaptionStored: Boolean;
    function IsCheckedStored: Boolean;
    function IsEnabledStored: Boolean;
    function IsHelpContextStored: Boolean;
    function IsHintStored: Boolean;
    function IsImageIndexStored: Boolean;
    function IsOnClickStored: Boolean;
    function IsShortCutStored: Boolean;
    function IsVisibleStored: Boolean;
    procedure Notify(Action: TTBItemChangedAction; Index: Integer; Item: TTBCustomItem);
    procedure RefreshOptions;
    procedure SetAction(Value: TBasicAction);
    procedure SetCaption(Value: String);
    procedure SetChecked(Value: Boolean);
    procedure SetDisplayMode(Value: TTBItemDisplayMode);
    procedure SetEnabled(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetImages(Value: TCustomImageList);
    procedure SetInheritOptions(Value: Boolean);
    procedure SetLinkSubitems(Value: TTBCustomItem);
    procedure SetMaskOptions(Value: TTBItemOptions);
    procedure SetOptions(Value: TTBItemOptions);
    procedure SetRadioItem(Value: Boolean);
    procedure SetSubMenuImages(Value: TCustomImageList);
    procedure SetVisible(Value: Boolean);
    procedure SubMenuImagesChanged;
    procedure TurnSiblingsOff;
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); dynamic;
    procedure Change(NeedResize: Boolean); virtual;
    function CreatePopup(const ParentView: TTBView; const ParentViewer: TTBItemViewer;
      const PositionAsSubmenu, SelectFirstItem, Customizing: Boolean;
      const APopupPoint: TPoint; const Alignment: TTBPopupAlignment): TTBPopupWindow; virtual;
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); virtual;
    procedure EnabledChanged; virtual;
    function GetActionLinkClass: TTBCustomItemActionLinkClass; dynamic;
    function GetChevronParentView: TTBView; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; virtual;
    procedure GetPopupPosition(ParentView: TTBView;
      PopupWindow: TTBPopupWindow; var PopupPositionRec: TTBPopupPositionRec); virtual;
    function GetPopupWindowClass: TTBPopupWindowClass; virtual;
    procedure IndexError;
    procedure Loaded; override;
    function NeedToRecreateViewer(AViewer: TTBItemViewer): Boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function OpenPopup(const SelectFirstItem, TrackRightButton: Boolean;
      const PopupPoint: TPoint; const Alignment: TTBPopupAlignment;
      const ReturnClickedItemOnly: Boolean; PositionAsSubmenu: Boolean): TTBCustomItem;
    procedure RecreateItemViewers;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetName(const NewName: TComponentName); override;
    procedure SetParentComponent(Value: TComponent); override;

    property ActionLink: TTBCustomItemActionLink read FActionLink write FActionLink;
    property ItemStyle: TTBItemStyle read FItemStyle write FItemStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
    function GetTopComponent: TComponent;

    procedure Add(AItem: TTBCustomItem);
    procedure Clear;
    procedure Click; virtual;
    function ContainsItem(AItem: TTBCustomItem): Boolean;
    procedure Delete(Index: Integer);
    function GetShortCutText: String;
    function IndexOf(AItem: TTBCustomItem): Integer;
    procedure InitiateAction; virtual;
    procedure Insert(NewIndex: Integer; AItem: TTBCustomItem);
    function IsShortCut(var Message: TWMKey): Boolean;
    procedure Move(CurIndex, NewIndex: Integer);
    function Popup(X, Y: Integer; TrackRightButton: Boolean;
      Alignment: TTBPopupAlignment = tbpaLeft;
      ReturnClickedItemOnly: Boolean = False;
      PositionAsSubmenu: Boolean = False): TTBCustomItem;
    procedure PostClick;
    procedure RegisterNotification(ANotify: TTBItemChangedProc);
    procedure Remove(Item: TTBCustomItem);
    procedure UnregisterNotification(ANotify: TTBItemChangedProc);
    procedure ViewBeginUpdate;
    procedure ViewEndUpdate;
    procedure ChangeScale(M, D: Integer); virtual;

    property Action: TBasicAction read GetAction write SetAction;
    property AutoCheck: Boolean read FAutoCheck write FAutoCheck stored IsAutoCheckStored default False;
    property Caption: String read FCaption write SetCaption stored IsCaptionStored;
    property Count: Integer read FItemCount;
    property Checked: Boolean read FChecked write SetChecked stored IsCheckedStored default False;
    property DisplayMode: TTBItemDisplayMode read FDisplayMode write SetDisplayMode default nbdmDefault;
    property EffectiveOptions: TTBItemOptions read FEffectiveOptions;
    property Enabled: Boolean read FEnabled write SetEnabled stored IsEnabledStored default True;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property HelpContext: THelpContext read FHelpContext write FHelpContext stored IsHelpContextStored default 0;
    { MP }
    property HelpKeyword: String read FHelpKeyword write FHelpKeyword stored IsHelpContextStored;
    property Hint: String read FHint write FHint stored IsHintStored;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex stored IsImageIndexStored default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property InheritOptions: Boolean read FInheritOptions write SetInheritOptions default True;
    property Items[Index: Integer]: TTBCustomItem read GetItem; default;
    property LinkSubitems: TTBCustomItem read FLinkSubitems write SetLinkSubitems;
    property MaskOptions: TTBItemOptions read FMaskOptions write SetMaskOptions default [];
    property Options: TTBItemOptions read FOptions write SetOptions default [];
    property Parent: TTBCustomItem read FParent;
    property ParentComponent: TComponent read FParentComponent write FParentComponent;
    property RadioItem: Boolean read FRadioItem write SetRadioItem default False;
    property ShortCut: TShortCut read FShortCut write FShortCut stored IsShortCutStored default 0;
    property SubMenuImages: TCustomImageList read FSubMenuImages write SetSubMenuImages;
    property Visible: Boolean read FVisible write SetVisible stored IsVisibleStored default True;
    property OnClick: TNotifyEvent read FOnClick write FOnClick stored IsOnClickStored;
    property OnPopup: TTBPopupEvent read FOnPopup write FOnPopup;
    property OnSelect: TTBSelectEvent read FOnSelect write FOnSelect;
  end;

  TTBCustomItemActionLink = class(TActionLink)
  protected
    FClient: TTBCustomItem;
    procedure AssignClient(AClient: TObject); override;
    function IsAutoCheckLinked: Boolean; virtual;
    function IsCaptionLinked: Boolean; override;
    function IsCheckedLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHelpContextLinked: Boolean; override;
    { MP }
    function IsHelpLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsShortCutLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    procedure SetAutoCheck(Value: Boolean); override;
    procedure SetCaption(const Value: String); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHelpContext(Value: THelpContext); override;
    { MP }
    procedure SetHelpKeyword(const Value: string); override;
    procedure SetHint(const Value: String); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetShortCut(Value: TShortCut); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
  end;

  TTBBaseAccObject = class(TInterfacedObject, IDispatch)
  public
    procedure ClientIsDestroying; virtual; abstract;
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  end;

  TTBItemViewer = class
  private
    FBoundsRect: TRect;
    FClipped: Boolean;
    FGroupLevel: Integer;
    FItem: TTBCustomItem;
    FOffEdge: Boolean;
    FShow: Boolean;
    FView: TTBView;
    procedure AccSelect(const AExecute: Boolean);
    function GetIndex: Integer;
  protected
    FAccObjectInstance: TTBBaseAccObject;
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer);
      virtual;
    function CaptionShown: Boolean; dynamic;
    function DoExecute: Boolean; virtual;
    procedure DrawItemCaption(const Canvas: TCanvas; ARect: TRect;
      const ACaption: String; ADrawDisabledShadow: Boolean; AFormat: UINT); virtual;
    procedure Entering(OldSelected: TTBItemViewer); virtual;
    function GetAccRole: Integer; virtual;
    function GetAccValue(var Value: WideString): Boolean; virtual;
    function GetCaptionText: String; virtual;
    procedure GetCursor(const Pt: TPoint; var ACursor: HCURSOR); virtual;
    function GetImageList: TCustomImageList;
    function ImageShown: Boolean;
    function IsRotated: Boolean;
    function IsToolbarSize: Boolean; virtual;
    function IsPtInButtonPart(X, Y: Integer): Boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure Leaving; virtual;
    procedure LosingCapture; virtual;
    procedure MouseDown(Shift: TShiftState; X, Y: Integer;
      var MouseDownOnMenu: Boolean); virtual;
    procedure MouseMove(X, Y: Integer); virtual;
    procedure MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean); virtual;
    procedure MouseWheel(WheelDelta: Integer; X, Y: Integer); virtual;
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect;
      IsSelected, IsPushed, UseDisabledShadow: Boolean); virtual;
    procedure PostAccSelect(const AExecute: Boolean);
    function UsesSameWidth: Boolean; virtual;
  public
    State: set of (tbisInvalidated, tbisLineSep);
    property BoundsRect: TRect read FBoundsRect;
    property Clipped: Boolean read FClipped;
    property Index: Integer read GetIndex;
    property Item: TTBCustomItem read FItem;
    property OffEdge: Boolean read FOffEdge;
    property Show: Boolean read FShow;
    property View: TTBView read FView;
    constructor Create(AView: TTBView; AItem: TTBCustomItem; AGroupLevel: Integer); virtual;
    destructor Destroy; override;
    procedure Execute(AGivePriority: Boolean);
    function GetAccObject: IDispatch;
    function GetHintText: String;
    function IsAccessible: Boolean;
    function IsToolbarStyle: Boolean; virtual;
    function ScreenToClient(const P: TPoint): TPoint;
  end;
  PTBItemViewerArray = ^TTBItemViewerArray;
  TTBItemViewerArray = array[0..$7FFFFFFF div SizeOf(TTBItemViewer)-1] of TTBItemViewer;
  TTBViewOrientation = (tbvoHorizontal, tbvoVertical, tbvoFloating);
  TTBEnterToolbarLoopOptions = set of (tbetMouseDown, tbetExecuteSelected,
    tbetFromMSAA);
  TTBViewState = set of (vsModal, vsMouseInWindow, vsDrawInOrder, vsOppositePopup,
    vsIgnoreFirstMouseUp, vsShowAccels, vsDropDownMenus, vsNoAnimation);
  TTBViewStyle = set of (vsMenuBar, vsUseHiddenAccels, vsAlwaysShowHints);
  TTBViewTimerID = (tiOpen, tiClose, tiScrollUp, tiScrollDown);

  TTBViewClass = class of TTBView;
  TTBView = class(TComponent)
  private
    FActiveTimers: set of TTBViewTimerID;
    FBackgroundColor: TColor;
    FBaseSize: TPoint;
    FCapture: Boolean;
    FCaptureWnd: HWND;
    FChevronOffset: Integer;
    FChevronParentView: TTBView;
    FChevronSize: Integer;
    FCurParentItem: TTBCustomItem;
    FCustomizing: Boolean;
    FDoneActionData: TTBDoneActionData;
    FInternalViewersAtEnd: Integer;
    FInternalViewersAtFront: Integer;
    FIsPopup: Boolean;
    FIsToolbar: Boolean;
    FMaxHeight: Integer;
    FMonitorRect: TRect;
    FMouseOverSelected: Boolean;
    FNewViewersGetHighestPriority: Boolean;
    FOpenViewer: TTBItemViewer;
    FOpenViewerView: TTBView;
    FOpenViewerWindow: TTBPopupWindow;
    FParentView: TTBView;
    FParentItem: TTBCustomItem;
    FPriorityList: TList;
    FOrientation: TTBViewOrientation;
    FScrollOffset: Integer;
    FSelected: TTBItemViewer;
    FSelectedViaMouse: Boolean;
    FShowDownArrow: Boolean;
    FShowUpArrow: Boolean;
    FState: TTBViewState;
    FStyle: TTBViewStyle;
    FUpdating: Integer;
    FUsePriorityList: Boolean;
    FValidated: Boolean;
    FViewerCount: Integer;
    FViewers: PTBItemViewerArray;
    FWindow: TWinControl;
    FWrapOffset: Integer;

    procedure DeletingViewer(Viewer: TTBItemViewer);
    procedure DrawItem(Viewer: TTBItemViewer; DrawTo: TCanvas; Offscreen: Boolean);
    procedure FreeViewers;
    procedure ImagesChanged;
    function InsertItemViewers(const NewIndex: Integer;
      const AItem: TTBCustomItem; const AGroupLevel: Integer;
      const AddToPriorityList, TopOfPriorityList: Boolean): Integer;
    procedure ItemNotification(Ancestor: TTBCustomItem; Relayed: Boolean;
      Action: TTBItemChangedAction; Index: Integer; Item: TTBCustomItem);
    procedure LinkNotification(Ancestor: TTBCustomItem; Relayed: Boolean;
      Action: TTBItemChangedAction; Index: Integer; Item: TTBCustomItem);
    procedure RecreateItemViewer(const I: Integer);
    procedure Scroll(ADown: Boolean);
    procedure SetCustomizing(Value: Boolean);
    procedure SetSelected(Value: TTBItemViewer);
    procedure SetUsePriorityList(Value: Boolean);
    procedure StartTimer(const ATimer: TTBViewTimerID; const Interval: Integer);
    procedure StopAllTimers;
    procedure StopTimer(const ATimer: TTBViewTimerID);
    procedure UpdateCurParentItem;
  protected
    FAccObjectInstance: TTBBaseAccObject;
    procedure AutoSize(AWidth, AHeight: Integer); virtual;
    function CalculatePositions(const CanMoveControls: Boolean;
      const AOrientation: TTBViewOrientation;
      AWrapOffset, AChevronOffset, AChevronSize: Integer;
      var ABaseSize, TotalSize: TPoint;
      var AWrappedLines: Integer): Boolean;
    procedure DoUpdatePositions(var ASize: TPoint); virtual;
    function GetChevronItem: TTBCustomItem; virtual;
    procedure GetMargins(AOrientation: TTBViewOrientation; var Margins: TRect);
      virtual;
    function GetMDIButtonsItem: TTBCustomItem; virtual;
    function GetMDISystemMenuItem: TTBCustomItem; virtual;
    function GetParentToolbarView: TTBView;
    function GetRootView: TTBView;
    function HandleWMGetObject(var Message: TMessage): Boolean;
    procedure InitiateActions;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAccelsVisibility(AShowAccels: Boolean);
    procedure SetState(AState: TTBViewState);
    property DoneActionData: TTBDoneActionData read FDoneActionData write FDoneActionData;
    property ShowDownArrow: Boolean read FShowDownArrow; {vb+}
    property ShowUpArrow: Boolean read FShowUpArrow; {vb+}
  public
    constructor CreateView(AOwner: TComponent; AParentView: TTBView;
      AParentItem: TTBCustomItem; AWindow: TWinControl;
      AIsToolbar, ACustomizing, AUsePriorityList: Boolean); virtual;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure CancelCapture;
    procedure CancelChildPopups;
    procedure CancelMode;
    procedure CloseChildPopups;
    function ContainsView(AView: TTBView): Boolean;
    procedure DrawSubitems(ACanvas: TCanvas);
    procedure EndModal;
    procedure EndModalWithClick(AViewer: TTBItemViewer);
    { MP }
    procedure EndModalWithHelp(AContextID: Integer); overload;
    procedure EndModalWithHelp(HelpKeyword: string); overload;
    { /MP }
    procedure EndModalWithSystemMenu(AWnd: HWND; AKey: Cardinal);
    procedure EndUpdate;
    procedure EnterToolbarLoop(Options: TTBEnterToolbarLoopOptions); virtual;
    procedure ExecuteSelected(AGivePriority: Boolean);
    function Find(Item: TTBCustomItem): TTBItemViewer;
    function FirstSelectable: TTBItemViewer;
    function GetAccObject: IDispatch;
    function GetCaptureWnd: HWND;
    function GetFont: TFont; virtual;
    procedure GetOffEdgeControlList(const List: TList);
    procedure GivePriority(AViewer: TTBItemViewer);
    function HighestPriorityViewer: TTBItemViewer;
    procedure Invalidate(AViewer: TTBItemViewer);
    procedure InvalidatePositions; virtual;
    function IndexOf(AViewer: TTBItemViewer): Integer;
    function IsModalEnding: Boolean;
    function NextSelectable(CurViewer: TTBItemViewer; GoForward: Boolean): TTBItemViewer;
    function NextSelectableWithAccel(CurViewer: TTBItemViewer; Key: Char;
      RequirePrimaryAccel: Boolean; var IsOnlyItemWithAccel: Boolean): TTBItemViewer;
    procedure NotifyFocusEvent;
    function OpenChildPopup(const SelectFirstItem: Boolean): Boolean;
    procedure RecreateAllViewers;
    procedure ScrollSelectedIntoView;
    procedure Select(Value: TTBItemViewer; ViaMouse: Boolean);
    procedure SetCapture;
    procedure TryValidatePositions;
    procedure UpdateSelection(const P: PPoint; const AllowNewSelection: Boolean);
    function UpdatePositions: TPoint;
    procedure ValidatePositions;
    function ViewerFromPoint(const P: TPoint): TTBItemViewer;
    function GetMonitor: TMonitor; virtual;

    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property BaseSize: TPoint read FBaseSize;
    property Capture: Boolean read FCapture;
    property ChevronOffset: Integer read FChevronOffset write FChevronOffset;
    property ChevronSize: Integer read FChevronSize write FChevronSize;
    property Customizing: Boolean read FCustomizing write SetCustomizing;
    property IsPopup: Boolean read FIsPopup;
    property IsToolbar: Boolean read FIsToolbar;
    property MouseOverSelected: Boolean read FMouseOverSelected;
    property NewViewersGetHighestPriority: Boolean read FNewViewersGetHighestPriority
      write FNewViewersGetHighestPriority;
    property ParentView: TTBView read FParentView;
    property ParentItem: TTBCustomItem read FParentItem;
    property OpenViewer: TTBItemViewer read FOpenViewer;
    property OpenViewerView: TTBView read FOpenViewerView;
    property Orientation: TTBViewOrientation read FOrientation write FOrientation;
    property Selected: TTBItemViewer read FSelected write SetSelected;
    property SelectedViaMouse: Boolean read FSelectedViaMouse;
    property State: TTBViewState read FState;
    property Style: TTBViewStyle read FStyle write FStyle;
    property UsePriorityList: Boolean read FUsePriorityList write SetUsePriorityList;
    property Viewers: PTBItemViewerArray read FViewers;
    property ViewerCount: Integer read FViewerCount;
    property Window: TWinControl read FWindow;
    property WrapOffset: Integer read FWrapOffset write FWrapOffset;
  end;

  TTBRootItemClass = class of TTBRootItem;
  TTBRootItem = class(TTBCustomItem);
  { same as TTBCustomItem, except there's a property editor for it }

  TTBItem = class(TTBCustomItem)
  published
    property Action;
    property AutoCheck;
    property Caption;
    property Checked;
    property DisplayMode;
    property Enabled;
    property GroupIndex;
    property HelpContext;
    { MP }
    property HelpKeyword;
    property Hint;
    property ImageIndex;
    property Images;
    property InheritOptions;
    property MaskOptions;
    property Options;
    property RadioItem;
    property ShortCut;
    property Visible;

    property OnClick;
    property OnSelect;
  end;

  TTBGroupItem = class(TTBCustomItem)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property InheritOptions;
    property LinkSubitems;
    property MaskOptions;
    property Options;
  end;

  TTBSubmenuItem = class(TTBCustomItem)
  private
    function GetDropdownCombo: Boolean;
    procedure SetDropdownCombo(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Action;
    property AutoCheck;
    property Caption;
    property Checked;
    //property DisplayAsToolbar;
    property DisplayMode;
    property DropdownCombo: Boolean read GetDropdownCombo write SetDropdownCombo default False;
    property Enabled;
    property GroupIndex;
    property HelpContext;
    { MP }
    property HelpKeyword;
    property Hint;
    property ImageIndex;
    property Images;
    property InheritOptions;
    property LinkSubitems;
    property MaskOptions;
    property Options;
    property RadioItem;
    property ShortCut;
    property SubMenuImages;
    property Visible;

    property OnClick;
    property OnPopup;
    property OnSelect;
  end;

  TTBSeparatorItem = class(TTBCustomItem)
  private
    FBlank: Boolean;
    procedure SetBlank(Value: Boolean);
  protected
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Blank: Boolean read FBlank write SetBlank default False;
    property Hint;
    property Visible;
  end;

  TTBSeparatorItemViewer = class(TTBItemViewer)
  protected
    procedure CalcSize(const Canvas: TCanvas;
      var AWidth, AHeight: Integer); override;
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect;
      IsSelected, IsPushed, UseDisabledShadow: Boolean); override;
    function UsesSameWidth: Boolean; override;
  end;

  TTBControlItem = class(TTBCustomItem)
  private
    FControl: TControl;
    FDontFreeControl: Boolean;
    procedure SetControl(Value: TControl);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateControlItem(AOwner: TComponent; AControl: TControl);
    destructor Destroy; override;
    property DontFreeControl: Boolean read FDontFreeControl write FDontFreeControl;
  published
    property Control: TControl read FControl write SetControl;
  end;

  TTBPopupView = class(TTBView)
  protected
    procedure AutoSize(AWidth, AHeight: Integer); override;
  public
    function GetMonitor: TMonitor; override;
    function GetFont: TFont; override;
  end;

  ITBPopupWindow = interface
    ['{E45CBE74-1ECF-44CB-B064-6D45B1924708}']
  end;

  TTBPopupWindow = class(TCustomControl, ITBPopupWindow)
  private
    FAccelsVisibilitySet: Boolean;
    FAnimationDirection: TTBAnimationDirection;
    FView: TTBView;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMHintShowPause(var Message: TMessage); message CM_HINTSHOWPAUSE;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMClose(var Message: TWMClose); message WM_CLOSE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetObject(var Message: TMessage); message WM_GETOBJECT;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMPrint(var Message: TMessage); message WM_PRINT;
    procedure WMPrintClient(var Message: TMessage); message WM_PRINTCLIENT;
    procedure WMTB2kStepAnimation(var Message: TMessage); message WM_TB2K_STEPANIMATION;
    procedure WMTB2kAnimationEnded (var Message: TMessage); message WM_TB2K_ANIMATIONENDED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWindowHandle; override;
    function GetNCSize: TPoint; dynamic;
    function GetViewClass: TTBViewClass; dynamic;
    procedure Paint; override;
    procedure PaintScrollArrows; virtual;
    property AnimationDirection: TTBAnimationDirection read FAnimationDirection;
    {MP}
    procedure Cancel; dynamic;
  public
    constructor CreatePopupWindow(AOwner: TComponent; const AParentView: TTBView;
      const AItem: TTBCustomItem; const ACustomizing: Boolean; const PopupPoint: TPoint); virtual;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    property View: TTBView read FView;
  end;

  ITBItems = interface
    ['{A5C0D7CC-3EC4-4090-A0F8-3D03271877EA}']
    function GetItems: TTBCustomItem;
  end;

  TTBItemContainer = class(TComponent, ITBItems)
  private
    FItem: TTBRootItem;
    function GetImages: TCustomImageList;
    function GetItems: TTBCustomItem;
    procedure SetImages(Value: TCustomImageList);
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Items: TTBRootItem read FItem;
  published
    property Images: TCustomImageList read GetImages write SetImages;
  end;

  TTBPopupMenu = class(TPopupMenu, ITBItems)
  private
    FItem: TTBRootItem;
    //procedure SetItems(Value: TTBCustomItem);
    function GetImages: TCustomImageList;
    function GetItems: TTBCustomItem;
    function GetLinkSubitems: TTBCustomItem;
    function GetOptions: TTBItemOptions;
    procedure RootItemClick(Sender: TObject);
    procedure SetImages(Value: TCustomImageList);
    procedure SetLinkSubitems(Value: TTBCustomItem);
    procedure SetOptions(Value: TTBItemOptions);
  protected
    function GetRootItemClass: TTBRootItemClass; dynamic;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsShortCut(var Message: TWMKey): Boolean; override;
    procedure Popup(X, Y: Integer); override;
    function PopupEx(X, Y: Integer; ReturnClickedItemOnly: Boolean = False): TTBCustomItem;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  published
    property Images: TCustomImageList read GetImages write SetImages;
    property Items: TTBRootItem read FItem;
    property LinkSubitems: TTBCustomItem read GetLinkSubitems write SetLinkSubitems;
    property Options: TTBItemOptions read GetOptions write SetOptions default [];
  end;

  TTBCustomImageList = class(TImageList)
  private
    FCheckedImages: TCustomImageList;
    FCheckedImagesChangeLink: TChangeLink;
    FDisabledImages: TCustomImageList;
    FDisabledImagesChangeLink: TChangeLink;
    FHotImages: TCustomImageList;
    FHotImagesChangeLink: TChangeLink;
    FImagesBitmap: TBitmap;
    FImagesBitmapMaskColor: TColor;
    procedure ChangeImages(var AImageList: TCustomImageList;
      Value: TCustomImageList; AChangeLink: TChangeLink);
    procedure ImageListChanged(Sender: TObject);
    procedure ImagesBitmapChanged(Sender: TObject);
    procedure SetCheckedImages(Value: TCustomImageList);
    procedure SetDisabledImages(Value: TCustomImageList);
    procedure SetHotImages(Value: TCustomImageList);
    procedure SetImagesBitmap(Value: TBitmap);
    procedure SetImagesBitmapMaskColor(Value: TColor);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property CheckedImages: TCustomImageList read FCheckedImages write SetCheckedImages;
    property DisabledImages: TCustomImageList read FDisabledImages write SetDisabledImages;
    property HotImages: TCustomImageList read FHotImages write SetHotImages;
    property ImagesBitmap: TBitmap read FImagesBitmap write SetImagesBitmap;
    property ImagesBitmapMaskColor: TColor read FImagesBitmapMaskColor
      write SetImagesBitmapMaskColor default clFuchsia;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawState(Canvas: TCanvas; X, Y, Index: Integer;
      Enabled, Selected, Checked: Boolean); virtual;
  end;

  TTBImageList = class(TTBCustomImageList)
  published
    property CheckedImages;
    property DisabledImages;
    property HotImages;
    property ImagesBitmap;
    property ImagesBitmapMaskColor;
  end;

const
  {$IFNDEF TB2K_USE_STRICT_O2K_MENU_STYLE}
  tbMenuBkColor = clMenu;
  tbMenuTextColor = clMenuText;
  {$ELSE}
  tbMenuBkColor = clBtnFace;
  tbMenuTextColor = clBtnText;
  {$ENDIF}

  tbMenuVerticalMargin = 4;
  tbMenuImageTextSpace = 1;
  tbMenuLeftTextMargin = 2;
  tbMenuRightTextMargin = 3;

  tbMenuSeparatorOffset = 12;

  tbMenuScrollArrowHeight = 19;

  tbDropdownArrowWidth = 8;
  tbDropdownArrowMargin = 3;
  tbDropdownComboArrowWidth = 11;
  tbDropdownComboMargin = 2;

  tbLineSpacing = 6;
  tbLineSepOffset = 1;
  tbDockedLineSepOffset = 4;

  WM_TB2K_CLICKITEM = WM_USER + $100;

procedure TBInitToolbarSystemFont;
function GetToolbarFont(PixelsPerInch: Integer): TFont; overload;
function GetToolbarFont(Control: TControl): TFont; overload;

type
  TTBModalHandler = class
  private
    FCreatedWnd: Boolean;
    FInited: Boolean;
    FWnd: HWND;
    FRootPopup: TTBPopupWindow;
    procedure WndProc(var Msg: TMessage);
  public
    constructor Create(AExistingWnd: HWND);
    destructor Destroy; override;
    procedure Loop(const RootView: TTBView; const AMouseDown, AExecuteSelected,
      AFromMSAA, TrackRightButton: Boolean);
    property RootPopup: TTBPopupWindow read FRootPopup write FRootPopup;
    property Wnd: HWND read FWnd;

    class procedure DoLockForegroundWindow(LockCode: Cardinal);
    class procedure LockForegroundWindow;
    class procedure UnlockForegroundWindow;
  end;

function ProcessDoneAction(const DoneActionData: TTBDoneActionData;
  const ReturnClickedItemOnly: Boolean): TTBCustomItem;

implementation

uses
  MMSYSTEM, TB2Consts, TB2Common, IMM, TB2Acc, Winapi.oleacc, Types, PasTools, Generics.Collections, TB2Toolbar;

var
  LastPos: TPoint;

threadvar
  ClickWndRefCount: Integer;
  ClickWnd: HWND;
  ClickList: TList;

type
  PItemChangedNotificationData = ^TItemChangedNotificationData;
  TItemChangedNotificationData = record
    Proc: TTBItemChangedProc;
    RefCount: Integer;
  end;

  TComponentAccess = class(TComponent);
  TControlAccess = class(TControl);

const
  ViewTimerBaseID = 9000;
  MaxGroupLevel = 10;


{ Misc. }

var
  ShortCutToTextFixes: TStrings = nil;

function ShortCutToTextFix(AShortCut: TShortCut): string;
begin
  Result := ShortCutToText(AShortCut);
  // WORKAROUND: German keyboard driver is giving wrong name for VK_MULTIPLY
  if (AShortCut = ShortCut(VK_MULTIPLY, [])) and
     (Result = ' (ZEHNERTASTATUR)') then
  begin
    Result := ShortCutToText(ShortCut(VK_ADD, []));
    Result := StringReplace(Result, '+', '*', []);
  end;
end;

procedure DestroyClickWnd;
begin
  if ClickWnd <> 0 then begin
    Classes.DeallocateHWnd(ClickWnd);
    ClickWnd := 0;
  end;
  FreeAndNil(ClickList);
end;

procedure ReferenceClickWnd;
begin
  Inc(ClickWndRefCount);
end;

procedure ReleaseClickWnd;
begin
  Dec(ClickWndRefCount);
  if ClickWndRefCount = 0 then
    DestroyClickWnd;
end;

procedure QueueClick(const AItem: TObject; const AArg: Integer);
{ Adds an item to ClickList and posts a message to handle it. AItem must be
  either a TTBCustomItem or TTBItemViewer. }
var
  I: Integer;
begin
  if ClickWnd = 0 then
    ClickWnd := Classes.AllocateHWnd(TTBCustomItem.ClickWndProc);
  if ClickList = nil then
    ClickList := TList.Create;

  { Add a new item to ClickList or replace an empty one }
  I := ClickList.IndexOf(nil);
  if I = -1 then
    I := ClickList.Add(AItem)
  else
    ClickList[I] := AItem;

  PostMessage(ClickWnd, WM_TB2K_CLICKITEM, AArg, I);
end;

procedure RemoveFromClickList(const AItem: TObject);
{ Any class that potentially calls QueueClick needs to call RemoveFromClickList
  before an instance is destroyed to ensure that any references to the
  instance still in ClickList are removed. }
var
  I: Integer;
begin
  if Assigned(ClickList) and Assigned(AItem) then
    for I := 0 to ClickList.Count-1 do
      if ClickList[I] = AItem then
        ClickList[I] := Pointer(1);
        { ^ The special value of Pointer(1) is assigned to the item instead of
          of nil because we want the index to stay reserved until the
          WM_TB2K_CLICKITEM message for the index is processed. We don't want
          the WM_TB2K_CLICKITEM message that's still in the queue to later
          refer to a different item; this would result in queued clicks being
          processed in the wrong order in a case like this:
            A.PostClick; B.PostClick; A.Free; C.PostClick;
          C's click would end up being processed before A's, because C would
          get A's index. }
end;

function ProcessDoneAction(const DoneActionData: TTBDoneActionData;
  const ReturnClickedItemOnly: Boolean): TTBCustomItem;
begin
  Result := nil;
  case DoneActionData.DoneAction of
    tbdaNone: ;
    tbdaClickItem: begin
        if DoneActionData.Sound and NeedToPlaySound('MenuCommand') then
          PlaySoundA('MenuCommand', 0, SND_ALIAS or SND_ASYNC or SND_NODEFAULT or SND_NOSTOP);
        Result := DoneActionData.ClickItem;
        if not ReturnClickedItemOnly then
          Result.PostClick;
      end;
    tbdaOpenSystemMenu: begin
        SendMessage(DoneActionData.Wnd, WM_SYSCOMMAND, SC_KEYMENU, DoneActionData.Key);
      end;
    tbdaHelpContext: begin
        { Based on code in TPopupList.WndProc: }
        if Assigned(Screen.ActiveForm) and
           (biHelp in Screen.ActiveForm.BorderIcons) then
          Application.HelpCommand(HELP_CONTEXTPOPUP, DoneActionData.ContextID)
        else
          Application.HelpContext(DoneActionData.ContextID);
      end;
    { MP }
    tbdaHelpKeyword: begin
        Application.HelpKeyword(string(DoneActionData.HelpKeyword));
      end;
    { /MP }
  end;
end;


{ TTBItemDataArray routines }

procedure InsertIntoItemArray(var AItems: PTBItemDataArray;
  var AItemCount: Integer; NewIndex: Integer; AItem: TTBCustomItem);
begin
  ReallocMem(AItems, (AItemCount+1) * SizeOf(AItems[0]));
  if NewIndex < AItemCount then
    System.Move(AItems[NewIndex], AItems[NewIndex+1],
      (AItemCount-NewIndex) * SizeOf(AItems[0]));
  AItems[NewIndex].Item := AItem;
  Inc(AItemCount);
end;

procedure DeleteFromItemArray(var AItems: PTBItemDataArray;
  var AItemCount: Integer; Index: Integer);
begin
  Dec(AItemCount);
  if Index < AItemCount then
    System.Move(AItems[Index+1], AItems[Index],
      (AItemCount-Index) * SizeOf(AItems[0]));
  ReallocMem(AItems, AItemCount * SizeOf(AItems[0]));
end;

procedure InsertIntoViewerArray(var AItems: PTBItemViewerArray;
  var AItemCount: Integer; NewIndex: Integer; AItem: TTBItemViewer);
begin
  ReallocMem(AItems, (AItemCount+1) * SizeOf(AItems[0]));
  if NewIndex < AItemCount then
    System.Move(AItems[NewIndex], AItems[NewIndex+1],
      (AItemCount-NewIndex) * SizeOf(AItems[0]));
  AItems[NewIndex] := AItem;
  Inc(AItemCount);
end;

procedure DeleteFromViewerArray(var AItems: PTBItemViewerArray;
  var AItemCount: Integer; Index: Integer);
begin
  Dec(AItemCount);
  if Index < AItemCount then
    System.Move(AItems[Index+1], AItems[Index],
      (AItemCount-Index) * SizeOf(AItems[0]));
  ReallocMem(AItems, AItemCount * SizeOf(AItems[0]));
end;


{ TTBCustomItemActionLink }

procedure TTBCustomItemActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TTBCustomItem;
end;

function TTBCustomItemActionLink.IsAutoCheckLinked: Boolean;
begin
  Result := (FClient.AutoCheck = (Action as TCustomAction).AutoCheck);
end;

function TTBCustomItemActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
    (FClient.Caption = (Action as TCustomAction).Caption);
end;

function TTBCustomItemActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
    (FClient.Checked = (Action as TCustomAction).Checked);
end;

function TTBCustomItemActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and
    (FClient.Enabled = (Action as TCustomAction).Enabled);
end;

function TTBCustomItemActionLink.IsHelpContextLinked: Boolean;
begin
  Result := inherited IsHelpContextLinked and
    (FClient.HelpContext = (Action as TCustomAction).HelpContext);
end;

{ MP }
function TTBCustomItemActionLink.IsHelpLinked: Boolean;
begin
  Result := inherited IsHelpLinked and
    (FClient.HelpContext = (Action as TCustomAction).HelpContext) and
    (FClient.HelpKeyword = (Action as TCustomAction).HelpKeyword){ and
    (FClient.HelpType = (Action as TCustomAction).HelpType);} // TODO
end;
{ /MP }

function TTBCustomItemActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked and
    (FClient.Hint = (Action as TCustomAction).Hint);
end;

function TTBCustomItemActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

function TTBCustomItemActionLink.IsShortCutLinked: Boolean;
begin
  Result := inherited IsShortCutLinked and
    (FClient.ShortCut = (Action as TCustomAction).ShortCut);
end;

function TTBCustomItemActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked and
    (FClient.Visible = (Action as TCustomAction).Visible);
end;

function TTBCustomItemActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked and
    MethodsEqual(TMethod(FClient.OnClick), TMethod(Action.OnExecute));
end;

procedure TTBCustomItemActionLink.SetAutoCheck(Value: Boolean);
begin
  if IsAutoCheckLinked then FClient.AutoCheck := Value;
end;

procedure TTBCustomItemActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then FClient.Caption := Value;
end;

procedure TTBCustomItemActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then FClient.Checked := Value;
end;

procedure TTBCustomItemActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then FClient.Enabled := Value;
end;

procedure TTBCustomItemActionLink.SetHelpContext(Value: THelpContext);
begin
  if { MP } IsHelpLinked { /MP } then FClient.HelpContext := Value;
end;

{ MP }
procedure TTBCustomItemActionLink.SetHelpKeyword(const Value: String);
begin
  if IsHelpLinked then FClient.HelpKeyword := Value;
end;
{ /MP }

procedure TTBCustomItemActionLink.SetHint(const Value: string);
begin
  if IsHintLinked then FClient.Hint := Value;
end;

procedure TTBCustomItemActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then FClient.ImageIndex := Value;
end;

procedure TTBCustomItemActionLink.SetShortCut(Value: TShortCut);
begin
  if IsShortCutLinked then FClient.ShortCut := Value;
end;

procedure TTBCustomItemActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then FClient.Visible := Value;
end;

procedure TTBCustomItemActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then FClient.OnClick := Value;
end;


{ TTBCustomItem }

{}function ItemContainingItems(const AItem: TTBCustomItem): TTBCustomItem;
begin
  if Assigned(AItem) and Assigned(AItem.FLinkSubitems) then
    Result := AItem.FLinkSubitems
  else
    Result := AItem;
end;

constructor TTBCustomItem.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := True;
  FImageIndex := -1;
  FInheritOptions := True;
  FItemStyle := [tbisSelectable, tbisRedrawOnSelChange, tbisRedrawOnMouseOverChange];
  FVisible := True;
  ReferenceClickWnd;
end;

destructor TTBCustomItem.Destroy;
var
  I: Integer;
begin
  Destroying;
  RemoveFromClickList(Self);
  { Changed in 0.33. Moved FParent.Remove call *after* the child items are
    deleted. }
  for I := Count-1 downto 0 do
    Items[I].Free;
  if Assigned(FParent) then
    FParent.Remove(Self);
  FreeMem(FItems);
  FActionLink.Free;
  FActionLink := nil;
  FreeAndNil(FSubMenuImagesChangeLink);
  FreeAndNil(FImagesChangeLink);
  inherited;
  if Assigned(FNotifyList) then begin
    for I := FNotifyList.Count-1 downto 0 do
      Dispose(PItemChangedNotificationData(FNotifyList[I]));
    FNotifyList.Free;
  end;
  FLinkParents.Free;
  ReleaseClickWnd;
end;

function TTBCustomItem.IsAutoCheckStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsAutoCheckLinked;
end;

function TTBCustomItem.IsCaptionStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsCaptionLinked;
end;

function TTBCustomItem.IsCheckedStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsCheckedLinked;
end;

function TTBCustomItem.IsEnabledStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsEnabledLinked;
end;

function TTBCustomItem.IsHintStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsHintLinked;
end;

function TTBCustomItem.IsHelpContextStored: Boolean;
begin
  { MP }
  Result := (ActionLink = nil) or not FActionLink.IsHelpLinked;
end;

function TTBCustomItem.IsImageIndexStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsImageIndexLinked;
end;

function TTBCustomItem.IsShortCutStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsShortCutLinked;
end;

function TTBCustomItem.IsVisibleStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsVisibleLinked;
end;

function TTBCustomItem.IsOnClickStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsOnExecuteLinked;
end;

function TTBCustomItem.GetAction: TBasicAction;
begin
  if FActionLink <> nil then
    Result := FActionLink.Action
  else
    Result := nil;
end;

function TTBCustomItem.GetActionLinkClass: TTBCustomItemActionLinkClass;
begin
  Result := TTBCustomItemActionLink;
end;

procedure TTBCustomItem.DoActionChange(Sender: TObject);
begin
  if Sender = Action then ActionChange(Sender, False);
end;

procedure TTBCustomItem.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  if Action is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.AutoCheck = False) then
        Self.AutoCheck := AutoCheck;
      if not CheckDefaults or (Self.Caption = '') then
        Self.Caption := Caption;
      if not CheckDefaults or (Self.Checked = False) then
        Self.Checked := Checked;
      if not CheckDefaults or (Self.Enabled = True) then
        Self.Enabled := Enabled;
      if not CheckDefaults or (Self.HelpContext = 0) then
        Self.HelpContext := HelpContext;
      { MP }
      if not CheckDefaults or (Self.HelpKeyword = '') then
        Self.HelpKeyword := HelpKeyword;
      { /MP }
      if not CheckDefaults or (Self.Hint = '') then
        Self.Hint := Hint;
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
      if not CheckDefaults or (Self.ShortCut = scNone) then
        Self.ShortCut := ShortCut;
      if not CheckDefaults or (Self.Visible = True) then
        Self.Visible := Visible;
      if not CheckDefaults or not Assigned(Self.OnClick) then
        Self.OnClick := OnExecute;
    end;
end;

procedure TTBCustomItem.SetAction(Value: TBasicAction);
begin
  if Value = nil then begin
    FActionLink.Free;
    FActionLink := nil;
  end
  else begin
    if FActionLink = nil then
      FActionLink := GetActionLinkClass.Create(Self);
    FActionLink.Action := Value;
    FActionLink.OnChange := DoActionChange;
    { Note: Delphi's Controls.pas and Menus.pas merely check for
      "csLoading in Value.ComponentState" here. But that doesn't help when
      the Action property references an action on another form / data module
      that has already finished loading. So we check two things:
        1. csLoading in Value.ComponentState
        2. csLoading in ComponentState
      In the typical case where the item and action list reside on the same
      form, #1 and #2 are both true.
      Only #1 is true when Action references an action on another form / data
      module that is created *after* the item (e.g. if Form1.TBItem1.Action =
      Form2.Action1, and Form1 is created before Form2).
      Only #2 is true when Action references an action on another form / data
      module that is created *before* the item (e.g. if Form2.TBItem1.Action =
      Form1.Action1, and Form1 is created before Form2). }
    ActionChange(Value, (csLoading in Value.ComponentState) or
      (csLoading in ComponentState));
    Value.FreeNotification(Self);
  end;
end;

procedure TTBCustomItem.InitiateAction;
begin
  if FActionLink <> nil then FActionLink.Update;
end;

procedure TTBCustomItem.Loaded;
begin
  inherited;
  if Action <> nil then ActionChange(Action, True);
end;

procedure TTBCustomItem.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to FItemCount-1 do
    Proc(FItems[I].Item);
end;

procedure TTBCustomItem.SetChildOrder(Child: TComponent; Order: Integer);
var
  I: Integer;
begin
  I := IndexOf(Child as TTBCustomItem);
  if I <> -1 then
    Move(I, Order);
end;

function TTBCustomItem.HasParent: Boolean;
begin
  Result := True;
end;

function TTBCustomItem.GetParentComponent: TComponent;
begin
  if (FParent <> nil) and (FParent.FParentComponent <> nil) then
    Result := FParent.FParentComponent
  else
    Result := FParent;
end;

function TTBCustomItem.GetTopComponent: TComponent;
begin
  if Parent <> nil then Result := Parent.GetTopComponent
    else Result := FParentComponent;
end;

procedure TTBCustomItem.SetName(const NewName: TComponentName);
begin
  if Name <> NewName then begin
    inherited;
    if Assigned(FParent) then
      FParent.Notify(tbicNameChanged, -1, Self);
  end;
end;

procedure TTBCustomItem.SetParentComponent(Value: TComponent);
var
  Intf: ITBItems;
begin
  if FParent <> nil then FParent.Remove(Self);
  if Value <> nil then begin
    if Value is TTBCustomItem then
      TTBCustomItem(Value).Add(Self)
    else if Value.GetInterface(ITBItems, Intf) then
      Intf.GetItems.Add(Self);
  end;
end;

procedure TTBCustomItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    RemoveFromList(FLinkParents, AComponent);
    if AComponent = Action then Action := nil;
    if AComponent = Images then Images := nil;
    if AComponent = SubMenuImages then SubMenuImages := nil;
    if AComponent = LinkSubitems then LinkSubitems := nil;
  end;
end;

procedure TTBCustomItem.IndexError;
begin
  raise ETBItemError.Create(STBToolbarIndexOutOfBounds);
end;

class procedure TTBCustomItem.ClickWndProc(var Message: TMessage);
var
  List: TList;
  I: Integer;
  Item: TObject;
begin
  if Message.Msg = WM_TB2K_CLICKITEM then begin
    List := ClickList;  { optimization... }
    if Assigned(List) then begin
      I := Message.LParam;
      if (I >= 0) and (I < List.Count) then begin
        Item := List[I];
        List[I] := nil;
        if Item = Pointer(1) then  { is it a destroyed item? }
          Item := nil;
      end
      else
        Item := nil;

      { Remove trailing nil items from ClickList. This is not *necessary*, but
        it will make RemoveFromClickList faster if we clean out items that
        aren't used, and may never be used again. }
      for I := List.Count-1 downto 0 do begin
        if List[I] = nil then
          List.Delete(I)
        else
          Break;
      end;

      if Assigned(Item) then begin
        try
          if Item is TTBCustomItem then
            TTBCustomItem(Item).Click
          else if Item is TTBItemViewer then
            TTBItemViewer(Item).AccSelect(Message.WParam <> 0);
        except
          Application.HandleException(Item);
        end;
      end;
    end;
  end
  else
    with Message do
      Result := DefWindowProc(ClickWnd, Msg, wParam, lParam);
end;

procedure TTBCustomItem.PostClick;
{ Posts a message to the message queue that causes the item's Click handler to
  be executed when control is returned to the message loop.
  This should be called instead of Click when a WM_SYSCOMMAND message is
  (possibly) currently being handled, because TApplication.WndProc's
  CM_APPSYSCOMMAND handler disables the VCL's processing of focus messages
  until the Perform(WM_SYSCOMMAND, ...) call returns. (An OnClick handler which
  calls TForm.ShowModal needs focus messages to be enabled or else the form
  will be shown with no initial focus.) }
begin
  QueueClick(Self, 0);
end;

procedure TTBCustomItem.Click;
begin
  if Enabled then begin
    { Following code based on D6's TMenuItem.Click }
    if (not Assigned(ActionLink) and AutoCheck) or
       (Assigned(ActionLink) and not ActionLink.IsAutoCheckLinked and AutoCheck) then
      Checked := not Checked;
    { Following code based on D4's TControl.Click }
    { Call OnClick if assigned and not equal to associated action's OnExecute.
      If associated action's OnExecute assigned then call it, otherwise, call
      OnClick. }
    if Assigned(FOnClick) and (Action <> nil) and
       not MethodsEqual(TMethod(FOnClick), TMethod(Action.OnExecute)) then
      FOnClick(Self)
    else
    if not(csDesigning in ComponentState) and (ActionLink <> nil) then
      ActionLink.Execute(Self)
    else
    if Assigned(FOnClick) then
      FOnClick(Self);
  end;
end;

function TTBCustomItem.GetItem(Index: Integer): TTBCustomItem;
begin
  if (Index < 0) or (Index >= FItemCount) then IndexError;
  Result := FItems[Index].Item;
end;

procedure TTBCustomItem.Add(AItem: TTBCustomItem);
begin
  Insert(Count, AItem);
end;

procedure TTBCustomItem.InternalNotify(Ancestor: TTBCustomItem;
  NestingLevel: Integer; Action: TTBItemChangedAction; Index: Integer;
  Item: TTBCustomItem);
{ Note: Ancestor is Item's parent, or in the case of a group item relayed
  notification, it can also be a group item which *links* to Item's parent
  (i.e. ItemContainingItems(Ancestor) = Item.Parent). }

  procedure RelayToParentOf(const AItem: TTBCustomItem);
  begin
    if NestingLevel > MaxGroupLevel then
      Exit;
    if (tbisEmbeddedGroup in AItem.ItemStyle) and Assigned(AItem.Parent) then begin
      if Ancestor = Self then
        AItem.Parent.InternalNotify(AItem, NestingLevel + 1, Action, Index, Item)
      else
        { Don't alter Ancestor on subsequent relays; only on the first. }
        AItem.Parent.InternalNotify(Ancestor, NestingLevel + 1, Action, Index, Item);
    end;
  end;

var
  I: Integer;
  P: TTBCustomItem;
  SaveProc: TTBItemChangedProc;
begin
  { If Self is a group item, relay the notification to the parent }
  RelayToParentOf(Self);
  { If any group items are linked to Self, relay the notification to
    those items' parents }
  if Assigned(FLinkParents) then
    for I := 0 to FLinkParents.Count-1 do begin
      P := FLinkParents[I];
      if P <> Parent then
        RelayToParentOf(P);
    end;
  if Assigned(FNotifyList) then begin
    I := 0;
    while I < FNotifyList.Count do begin
      with PItemChangedNotificationData(FNotifyList[I])^ do begin
        SaveProc := Proc;
        Proc(Ancestor, Ancestor <> Self, Action, Index, Item);
      end;
      { Is I now out of bounds? }
      if I >= FNotifyList.Count then
        Break;
      { Only proceed to the next index if the list didn't change }
      if MethodsEqual(TMethod(PItemChangedNotificationData(FNotifyList[I])^.Proc),
         TMethod(SaveProc)) then
        Inc(I);
    end;
  end;
end;

procedure TTBCustomItem.Notify(Action: TTBItemChangedAction; Index: Integer;
  Item: TTBCustomItem);
begin
  InternalNotify(Self, 0, Action, Index, Item);
end;

procedure TTBCustomItem.ViewBeginUpdate;
begin
  Notify(tbicSubitemsBeginUpdate, -1, nil);
end;

procedure TTBCustomItem.ViewEndUpdate;
begin
  Notify(tbicSubitemsEndUpdate, -1, nil);
end;

procedure TTBCustomItem.Insert(NewIndex: Integer; AItem: TTBCustomItem);
begin
  if Assigned(AItem.FParent) then
    raise ETBItemError.Create(STBToolbarItemReinserted);
  if (NewIndex < 0) or (NewIndex > FItemCount) then IndexError;
  InsertIntoItemArray(FItems, FItemCount, NewIndex, AItem);
  AItem.FParent := Self;
  ViewBeginUpdate;
  try
    Notify(tbicInserted, NewIndex, AItem);
    AItem.RefreshOptions;
  finally
    ViewEndUpdate;
  end;
end;

procedure TTBCustomItem.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FItemCount) then IndexError;
  Notify(tbicDeleting, Index, FItems[Index].Item);
  FItems[Index].Item.FParent := nil;
  DeleteFromItemArray(FItems, FItemCount, Index);
end;

function TTBCustomItem.IndexOf(AItem: TTBCustomItem): Integer;
var
  I: Integer;
begin
  for I := 0 to FItemCount-1 do
    if FItems[I].Item = AItem then begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

procedure TTBCustomItem.Remove(Item: TTBCustomItem);
var
  I: Integer;
begin
  I := IndexOf(Item);
  //if I = -1 then raise ETBItemError.Create(STBToolbarItemNotFound);
  if I <> -1 then
    Delete(I);
end;

procedure TTBCustomItem.Clear;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
    Items[I].Free;
end;

procedure TTBCustomItem.Move(CurIndex, NewIndex: Integer);
var
  Item: TTBCustomItem;
begin
  if CurIndex <> NewIndex then begin
    if (NewIndex < 0) or (NewIndex >= FItemCount) then IndexError;
    Item := Items[CurIndex];
    ViewBeginUpdate;
    try
      Delete(CurIndex);
      Insert(NewIndex, Item);
    finally
      ViewEndUpdate;
    end;
  end;
end;

function TTBCustomItem.ContainsItem(AItem: TTBCustomItem): Boolean;
begin
  while Assigned(AItem) and (AItem <> Self) do
    AItem := AItem.Parent;
  Result := Assigned(AItem);
end;

procedure TTBCustomItem.RegisterNotification(ANotify: TTBItemChangedProc);
var
  I: Integer;
  Data: PItemChangedNotificationData;
begin
  if FNotifyList = nil then FNotifyList := TList.Create;
  for I := 0 to FNotifyList.Count-1 do
    with PItemChangedNotificationData(FNotifyList[I])^ do
      if MethodsEqual(TMethod(ANotify), TMethod(Proc)) then begin
        Inc(RefCount);
        Exit;
      end;
  FNotifyList.Expand;
  New(Data);
  Data.Proc := ANotify;
  Data.RefCount := 1;
  FNotifyList.Add(Data);
end;

procedure TTBCustomItem.UnregisterNotification(ANotify: TTBItemChangedProc);
var
  I: Integer;
  Data: PItemChangedNotificationData;
begin
  if Assigned(FNotifyList) then
    for I := 0 to FNotifyList.Count-1 do begin
      Data := FNotifyList[I];
      if MethodsEqual(TMethod(Data.Proc), TMethod(ANotify)) then begin
        Dec(Data.RefCount);
        if Data.RefCount = 0 then begin
          FNotifyList.Delete(I);
          Dispose(Data);
          if FNotifyList.Count = 0 then begin
            FNotifyList.Free;
            FNotifyList := nil;
          end;
        end;
        Break;
      end;
    end;
end;

function TTBCustomItem.GetPopupWindowClass: TTBPopupWindowClass;
begin
  Result := TTBPopupWindow;
end;

procedure TTBCustomItem.DoPopup(Sender: TTBCustomItem; FromLink: Boolean);
begin
  if Assigned(FOnPopup) then
    FOnPopup(Sender, FromLink);
  if not(tbisCombo in ItemStyle) then
    Click;
end;

var
  PlayedSound: Boolean = False;

procedure TTBCustomItem.GetPopupPosition(ParentView: TTBView;
  PopupWindow: TTBPopupWindow; var PopupPositionRec: TTBPopupPositionRec);
var
  X2, Y2: Integer;
  RepeatCalcX: Boolean;

  function CountObscured(X, Y, W, H: Integer): Integer;
  var
    I: Integer;
    P: TPoint;
    V: TTBItemViewer;
  begin
    Result := 0;
    if ParentView = nil then
      Exit;
    P := ParentView.FWindow.ClientToScreen(Point(0, 0));
    Dec(X, P.X);
    Dec(Y, P.Y);
    Inc(W, X);
    Inc(H, Y);
    for I := 0 to ParentView.FViewerCount-1 do begin
      V := ParentView.FViewers[I];
      if V.Show and (V.BoundsRect.Left >= X) and (V.BoundsRect.Right <= W) and
         (V.BoundsRect.Top >= Y) and (V.BoundsRect.Bottom <= H) then
        Inc(Result);
    end;
  end;

begin
  with PopupPositionRec do
  begin
    { Adjust the Y position of the popup window }
    { If the window is going off the bottom of the monitor, try placing it
      above the parent item }
    if (Y + H > MonitorRect.Bottom) and
       ((ParentView = nil) or (ParentView.FOrientation <> tbvoVertical)) then begin
      if not PositionAsSubmenu then
        Y2 := ParentItemRect.Top
      else
        Y2 := ParentItemRect.Bottom + NCSizeY;
      Dec(Y2, H);
      { Only place it above the parent item if it isn't going to go off the
        top of the monitor }
      if Y2 >= MonitorRect.Top then
        Y := Y2;
    end;
    { If it's still going off the bottom (which can be possible if a menu bar
      was off the screen to begin with), clip it to the bottom of the monitor }
    if Y + H > MonitorRect.Bottom then
      Y := MonitorRect.Bottom - H;
    if Y < MonitorRect.Top then
      Y := MonitorRect.Top;

    { Other adjustments to the position of the popup window }
    if not PositionAsSubmenu then begin
      if (ParentView = nil) and (Alignment = tbpaRight) and (X < MonitorRect.Left) then
        Inc(X, W);
      if X + W > MonitorRect.Right then begin
        if Assigned(ParentView) or (Alignment <> tbpaLeft) then
          X := MonitorRect.Right;
        Dec(X, W);
      end;
      if X < MonitorRect.Left then
        X := MonitorRect.Left;
      if (ParentView = nil) or (ParentView.FOrientation <> tbvoVertical) then begin
        Y2 := ParentItemRect.Top - H;
        if Y2 >= MonitorRect.Top then begin
          { Would the popup window obscure less items if it popped out to the
            top instead? }
          if (CountObscured(X, Y2, W, H) < CountObscured(X, Y, W, H)) or
             ((Y < ParentItemRect.Bottom) and (Y + H > ParentItemRect.Top) and
              (X < ParentItemRect.Right) and (X + W > ParentItemRect.Left)) then
            Y := Y2;
        end;
        { Make sure a tall popup window doesn't overlap the parent item }
        if (Y < ParentItemRect.Bottom) and (Y + H > ParentItemRect.Top) and
           (X < ParentItemRect.Right) and (X + W > ParentItemRect.Left) then begin
          if ParentItemRect.Right + W <= MonitorRect.Right then
            X := ParentItemRect.Right
          else
            X := ParentItemRect.Left - W;
          if X < MonitorRect.Top then
            X := MonitorRect.Top;
        end;
      end
      else begin
        X2 := ParentItemRect.Right;
        if X2 + W <= MonitorRect.Right then begin
          { Would the popup window obscure less items if it popped out to the
            right instead? }
          if (CountObscured(X2, Y, W, H) < CountObscured(X, Y, W, H)) or
             ((Y < ParentItemRect.Bottom) and (Y + H > ParentItemRect.Top) and
              (X < ParentItemRect.Right) and (X + W > ParentItemRect.Left)) then
            X := X2;
        end;
        { Make sure a wide popup window doesn't overlap the parent item }
        if (Y < ParentItemRect.Bottom) and (Y + H > ParentItemRect.Top) and
           (X < ParentItemRect.Right) and (X + W > ParentItemRect.Left) then begin
          if ParentItemRect.Bottom + H <= MonitorRect.Bottom then
            Y := ParentItemRect.Bottom
          else
            Y := ParentItemRect.Top - H;
          if Y < MonitorRect.Top then
            Y := MonitorRect.Top;
        end;
      end;
    end
    else begin
      { Make nested submenus go from left to right on the screen. Each it
        runs out of space on the screen, switch directions }
      repeat
        RepeatCalcX := False;
        X2 := X;
        if Opposite or (X2 + W > MonitorRect.Right) then begin
          if Assigned(ParentView) then
            X2 := ParentItemRect.Left + NCSizeX;
          Dec(X2, W);
          if not Opposite then
            Include(PopupWindow.View.FState, vsOppositePopup)
          else begin
            if X2 < MonitorRect.Left then begin
              Opposite := False;
              RepeatCalcX := True;
            end
            else
              Include(PopupWindow.View.FState, vsOppositePopup);
          end;
        end;
      until not RepeatCalcX;
      X := X2;
      if X < MonitorRect.Left then
        X := MonitorRect.Left;
    end;

    { Determine animation direction }
    AnimDir := [];
    if not PositionAsSubmenu then begin
      if Y >= ParentItemRect.Bottom then
        Include(AnimDir, tbadDown)
      else if Y + H <= ParentItemRect.Top then
        Include(AnimDir, tbadUp);
      if X >= ParentItemRect.Right then
        Include(AnimDir, tbadRight)
      else if X + W <= ParentItemRect.Left then
        Include(AnimDir, tbadLeft);
    end
    else begin
      if X + W div 2 >= ParentItemRect.Left + (ParentItemRect.Right - ParentItemRect.Left) div 2 then
        Include(AnimDir, tbadRight)
      else
        Include(AnimDir, tbadLeft);
    end;
  end;
end;

function TTBCustomItem.CreatePopup(const ParentView: TTBView;
  const ParentViewer: TTBItemViewer; const PositionAsSubmenu, SelectFirstItem,
  Customizing: Boolean; const APopupPoint: TPoint;
  const Alignment: TTBPopupAlignment): TTBPopupWindow;
var
  EventItem, ParentItem: TTBCustomItem;
  Opposite: Boolean;
  ChevronParentView: TTBView;
  ChevronMenu: Boolean;
  X, Y, W, H: Integer;
  P: TPoint;
  ParentItemRect: TRect;
  MonitorRect: TRect;
  PopupRec: TTBPopupPositionRec;
  NCSize: TPoint;
begin
  EventItem := ItemContainingItems(Self);
  if EventItem <> Self then
    EventItem.DoPopup(Self, True);
  DoPopup(Self, False);

  ChevronParentView := GetChevronParentView;
  ChevronMenu := False; // shut up
  if ChevronParentView = nil then
    ParentItem := Self
  else begin
    ParentItem := ChevronParentView.FParentItem;
    Assert(ParentItem.ParentComponent is TTBCustomToolbar);
    ChevronMenu :=
      (ParentItem.ParentComponent is TTBCustomToolbar) and
      TTBCustomToolbar(ParentItem.ParentComponent).ChevronMenu;
  end;

  Opposite := Assigned(ParentView) and (vsOppositePopup in ParentView.FState);
  Result := GetPopupWindowClass.CreatePopupWindow(nil, ParentView, ParentItem,
    Customizing, APopupPoint);
  try
    if Assigned(ChevronParentView) then begin
      ChevronParentView.FreeNotification(Result.View);
      Result.View.FChevronParentView := ChevronParentView;
      Result.View.FIsToolbar := not ChevronMenu;
      Result.View.Style := Result.View.Style +
        (ChevronParentView.Style * [vsAlwaysShowHints]);
      Result.Color := clBtnFace;
    end;

    { Calculate ParentItemRect, and MonitorRect (the rectangle of the monitor
      that the popup window will be confined to) }
    if Assigned(ParentView) then begin
      ParentView.ValidatePositions;
      ParentItemRect := ParentViewer.BoundsRect;
      P := ParentView.FWindow.ClientToScreen(Point(0, 0));
      OffsetRect(ParentItemRect, P.X, P.Y);
      if not IsRectEmpty(ParentView.FMonitorRect) then
        MonitorRect := ParentView.FMonitorRect
      else
        { MP (display menu on correct monitor) }
        MonitorRect := GetRectOfMonitorContainingRect(ParentItemRect, True);
        {MonitorRect := GetRectOfMonitorContainingPoint(APopupPoint, False);} {vb-}
        { MP }
        {MonitorRect := GetRectOfMonitorContainingPoint(APopupPoint, True);} {vb+}
    end
    else begin
      ParentItemRect.TopLeft := APopupPoint;
      ParentItemRect.BottomRight := APopupPoint;
      {MonitorRect := GetRectOfMonitorContainingPoint(APopupPoint, False);} {vb-}
      MonitorRect := GetRectOfMonitorContainingPoint(APopupPoint, True); {vb+}
    end;
    Result.View.FMonitorRect := MonitorRect;

    { Initialize item positions and size of the popup window }
    NCSize := Result.GetNCSize;
    if ChevronParentView = nil then
      Result.View.FMaxHeight := (MonitorRect.Bottom - MonitorRect.Top) -
        (NCSize.Y * 2)
    else
      Result.View.WrapOffset := (MonitorRect.Right - MonitorRect.Left) -
        (NCSize.X * 2);
    if SelectFirstItem then
      Result.View.Selected := Result.View.FirstSelectable;
    Result.View.UpdatePositions;
    W := Result.Width;
    H := Result.Height;

    { Calculate initial X,Y position of the popup window }
    if Assigned(ParentView) then begin
      if not PositionAsSubmenu then begin
        if ChevronParentView = nil then begin
          if (ParentView = nil) or (ParentView.FOrientation <> tbvoVertical) then begin
            if GetSystemMetrics(SM_MENUDROPALIGNMENT) = 0 then
              X := ParentItemRect.Left
            else
              X := ParentItemRect.Right - W;
            Y := ParentItemRect.Bottom;
          end
          else begin
            X := ParentItemRect.Left - W;
            Y := ParentItemRect.Top;
          end;
        end
        else begin
          if ChevronParentView.FOrientation <> tbvoVertical then begin
            X := ParentItemRect.Right - W;
            Y := ParentItemRect.Bottom;
          end
          else begin
            X := ParentItemRect.Left - W;
            Y := ParentItemRect.Top;
          end;
        end;
      end
      else begin
        X := ParentItemRect.Right - NCSize.X;
        Y := ParentItemRect.Top - NCSize.Y;
      end;
    end
    else begin
      X := APopupPoint.X;
      Y := APopupPoint.Y;
      case Alignment of
        tbpaRight: Dec(X, W);
        tbpaCenter: Dec(X, W div 2);
      end;
    end;

    PopupRec.PositionAsSubmenu := PositionAsSubmenu;
    PopupRec.Alignment := Alignment;
    PopupRec.Opposite := Opposite;
    PopupRec.MonitorRect := MonitorRect;
    PopupRec.ParentItemRect := ParentItemRect;
    PopupRec.NCSizeX := NCSize.X;
    PopupRec.NCSizeY := NCSize.Y;
    PopupRec.X := X;
    PopupRec.Y := Y;
    PopupRec.W := W;
    PopupRec.H := H;
    PopupRec.AnimDir := [];
    PopupRec.PlaySound := True;
    GetPopupPosition(ParentView, Result, PopupRec);
    X := PopupRec.X;
    Y := PopupRec.Y;
    W := PopupRec.W;
    H := PopupRec.H;
    Result.FAnimationDirection := PopupRec.AnimDir;
    Result.SetBounds(X, Y, W, H);
    if Assigned(ParentView) then begin
      Result.FreeNotification(ParentView);
      ParentView.FOpenViewerWindow := Result;
      ParentView.FOpenViewerView := Result.View;
      ParentView.FOpenViewer := ParentViewer;
      if ParentView.FIsToolbar then begin
        Include(ParentView.FState, vsDropDownMenus);
        ParentView.Invalidate(ParentViewer);
        ParentView.FWindow.Update;
      end;
    end;
    Include(Result.View.FState, vsDrawInOrder);
    if not PopupRec.PlaySound or not NeedToPlaySound('MenuPopup') then begin
      { Don't call PlaySound if we don't have to }
      Result.Visible := True;
    end
    else begin
      if not PlayedSound then begin
        { Work around Windows 2000 "bug" where there's a 1/3 second delay upon the
          first call to PlaySound (or sndPlaySound) by painting the window
          completely first. This way the delay isn't very noticable. }
        PlayedSound := True;
        Result.Visible := True;
        Result.Update;
        PlaySoundA('MenuPopup', 0, SND_ALIAS or SND_ASYNC or SND_NODEFAULT or SND_NOSTOP);
      end
      else begin
        PlaySoundA('MenuPopup', 0, SND_ALIAS or SND_ASYNC or SND_NODEFAULT or SND_NOSTOP);
        Result.Visible := True;
      end;
    end;
    NotifyWinEvent(EVENT_SYSTEM_MENUPOPUPSTART, Result.View.FWindow.Handle,
      OBJID_CLIENT, CHILDID_SELF);
    { Call NotifyFocusEvent now that the window is visible }
    if Assigned(Result.View.Selected) then
      Result.View.NotifyFocusEvent;
  except
    Result.Free;
    raise;
  end;
end;

function TTBCustomItem.OpenPopup(const SelectFirstItem, TrackRightButton: Boolean;
  const PopupPoint: TPoint; const Alignment: TTBPopupAlignment;
  const ReturnClickedItemOnly: Boolean; PositionAsSubmenu: Boolean): TTBCustomItem;
var
  ModalHandler: TTBModalHandler;
  Popup: TTBPopupWindow;
  DoneActionData: TTBDoneActionData;
begin
  ModalHandler := TTBModalHandler.Create(0);
  try
    Popup := CreatePopup(nil, nil, PositionAsSubmenu, SelectFirstItem, False, PopupPoint,
      Alignment);
    try
      Include(Popup.View.FState, vsIgnoreFirstMouseUp);
      ModalHandler.RootPopup := Popup;
      ModalHandler.Loop(Popup.View, False, False, False, TrackRightButton);
      DoneActionData := Popup.View.FDoneActionData;
    finally
      ModalHandler.RootPopup := nil;
      { Remove vsModal state from the root view before any TTBView.Destroy
        methods get called, so that NotifyFocusEvent becomes a no-op }
      Exclude(Popup.View.FState, vsModal);
      Popup.Free;
    end;
  finally
    ModalHandler.Free;
  end;
  Result := ProcessDoneAction(DoneActionData, ReturnClickedItemOnly);
end;

function TTBCustomItem.Popup(X, Y: Integer; TrackRightButton: Boolean;
  Alignment: TTBPopupAlignment = tbpaLeft;
  ReturnClickedItemOnly: Boolean = False;
  PositionAsSubmenu: Boolean = False): TTBCustomItem;
var
  P: TPoint;
begin
  P.X := X;
  P.Y := Y;
  Result := OpenPopup(False, TrackRightButton, P, Alignment,
    ReturnClickedItemOnly, PositionAsSubmenu);
end;

function TTBCustomItem.FindItemWithShortCut(AShortCut: TShortCut;
  var ATopmostParent: TTBCustomItem): TTBCustomItem;

  function DoItem(AParentItem: TTBCustomItem; LinkDepth: Integer): TTBCustomItem;
  var
    I: Integer;
    NewParentItem, Item: TTBCustomItem;
  begin
    Result := nil;
    NewParentItem := AParentItem;
    if Assigned(NewParentItem.LinkSubitems) then begin
      NewParentItem := NewParentItem.LinkSubitems;
      Inc(LinkDepth);
      if LinkDepth > 25 then
        Exit;  { prevent infinite link recursion }
    end;
    for I := 0 to NewParentItem.Count-1 do begin
      Item := NewParentItem.Items[I];
      if Item.ShortCut = AShortCut then begin
        Result := Item;
        Exit;
      end;
      Result := DoItem(Item, LinkDepth);
      if Assigned(Result) then begin
        ATopmostParent := Item;
        Exit;
      end;
    end;
  end;

begin
  ATopmostParent := nil;
  Result := DoItem(Self, 0);
end;

function TTBCustomItem.IsShortCut(var Message: TWMKey): Boolean;
var
  ShortCut: TShortCut;
  ShiftState: TShiftState;
  ShortCutItem, TopmostItem, Item, EventItem: TTBCustomItem;
  I: Integer;
label 1;
begin
  Result := False;
  ShiftState := KeyDataToShiftState(Message.KeyData);
  ShortCut := Menus.ShortCut(Message.CharCode, ShiftState);
1:ShortCutItem := FindItemWithShortCut(ShortCut, TopmostItem);
  if Assigned(ShortCutItem) then begin
    { Send OnPopup/OnClick events to ShortCutItem's parents so that they can
      update the Enabled state of ShortCutItem if needed }
    Item := Self;
    repeat
      if not Item.Enabled then
        Exit;
      EventItem := ItemContainingItems(Item);
      if not(csDesigning in ComponentState) then begin
        for I := 0 to EventItem.Count-1 do
          EventItem.Items[I].InitiateAction;
      end;
      if not(tbisEmbeddedGroup in Item.ItemStyle) then begin
        if EventItem <> Item then begin
          try
            EventItem.DoPopup(Item, True);
          except
            Application.HandleException(Self);
          end;
        end;
        try
          Item.DoPopup(Item, False);
        except
          Application.HandleException(Self);
        end;
      end;
      ShortCutItem := Item.FindItemWithShortCut(ShortCut, TopmostItem);
      if ShortCutItem = nil then
        { Can no longer find the shortcut inside TopmostItem. Start over
          because the shortcut might have moved. }
        goto 1;
      Item := TopmostItem;
    until Item = nil;
    if ShortCutItem.Enabled then begin
      try
        ShortCutItem.Click;
      except
        Application.HandleException(Self);
      end;
      Result := True;
    end;
  end;
end;

function TTBCustomItem.GetChevronParentView: TTBView;
begin
  Result := nil;
end;

function TTBCustomItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TTBItemViewer;
end;

function TTBCustomItem.NeedToRecreateViewer(AViewer: TTBItemViewer): Boolean;
begin
  Result := False;
end;

function TTBCustomItem.GetShortCutText: String;
var
  P: Integer;
begin
  P := Pos(#9, Caption);
  if P = 0 then begin
    if ShortCut <> 0 then
      Result := ShortCutToTextFix(ShortCut)
    else
      Result := '';
  end
  else
    Result := Copy(Caption, P+1, Maxint);
end;

procedure TTBCustomItem.Change(NeedResize: Boolean);
const
  ItemChangedActions: array[Boolean] of TTBItemChangedAction =
    (tbicInvalidate, tbicInvalidateAndResize);
begin
  if Assigned(FParent) then
    FParent.Notify(ItemChangedActions[NeedResize], -1, Self);
end;

procedure TTBCustomItem.RecreateItemViewers;
begin
  if Assigned(FParent) then
    FParent.Notify(tbicRecreateItemViewers, -1, Self);
end;

procedure TTBCustomItem.ImageListChangeHandler(Sender: TObject);
var
  Resize: Boolean;
begin
  if Sender = FSubMenuImages then begin
    FSubMenuImagesChangeLink.FLastWidth := FSubMenuImages.Width;
    FSubMenuImagesChangeLink.FLastHeight := FSubMenuImages.Height;
    SubMenuImagesChanged;
  end
  else begin
    { Sender is FImages }
    Resize := False;
    if (FImagesChangeLink.FLastWidth <> FImages.Width) or
       (FImagesChangeLink.FLastHeight <> FImages.Height) then begin
      FImagesChangeLink.FLastWidth := FImages.Width;
      FImagesChangeLink.FLastHeight := FImages.Height;
      Resize := True;
    end;
    Change(Resize);
  end;
end;

procedure TTBCustomItem.SubMenuImagesChanged;
begin
  Notify(tbicSubMenuImagesChanged, -1, nil);
end;

procedure TTBCustomItem.TurnSiblingsOff;
var
  I: Integer;
  Item: TTBCustomItem;
begin
  if (GroupIndex <> 0) and Assigned(FParent) then begin
    for I := 0 to FParent.Count-1 do begin
      Item := FParent[I];
      if (Item <> Self) and (Item.GroupIndex = GroupIndex) then
        Item.Checked := False;
    end;
  end;
end;

procedure TTBCustomItem.SetCaption(Value: String);
begin
  if FCaption <> Value then begin
    FCaption := Value;
    Change(True);
  end;
end;

procedure TTBCustomItem.SetChecked(Value: Boolean);
begin
  if FChecked <> Value then begin
    FChecked := Value;
    Change(False);
    if Value then
      TurnSiblingsOff;
  end;
end;

procedure TTBCustomItem.SetDisplayMode(Value: TTBItemDisplayMode);
begin
  if FDisplayMode <> Value then begin
    FDisplayMode := Value;
    Change(True);
  end;
end;

procedure TTBCustomItem.EnabledChanged;
begin
  Change(False);
end;

procedure TTBCustomItem.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    EnabledChanged;
  end;
end;

procedure TTBCustomItem.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then begin
    FGroupIndex := Value;
    if Checked then
      TurnSiblingsOff;
  end;
end;

procedure TTBCustomItem.SetImageIndex(Value: TImageIndex);
var
  HadNoImage: Boolean;
begin
  if FImageIndex <> Value then begin
    HadNoImage := FImageIndex = -1;
    FImageIndex := Value;
    Change(HadNoImage xor (Value = -1));
  end;
end;

function TTBCustomItem.ChangeImages(var AImages: TCustomImageList;
  const Value: TCustomImageList; var AChangeLink: TTBImageChangeLink): Boolean;
{ Returns True if image list was resized }
var
  LastWidth, LastHeight: Integer;
begin
  Result := False;
  LastWidth := -1;
  LastHeight := -1;
  if Assigned(AImages) then begin
    LastWidth := AImages.Width;
    LastHeight := AImages.Height;
    AImages.UnregisterChanges(AChangeLink);
    if Value = nil then begin
      AChangeLink.Free;
      AChangeLink := nil;
      Result := True;
    end;
  end;
  AImages := Value;
  if Assigned(Value) then begin
    Result := (Value.Width <> LastWidth) or (Value.Height <> LastHeight);
    if AChangeLink = nil then begin
      AChangeLink := TTBImageChangeLink.Create;
      AChangeLink.FLastWidth := Value.Width;
      AChangeLink.FLastHeight := Value.Height;
      AChangeLink.OnChange := ImageListChangeHandler;
    end;
    Value.RegisterChanges(AChangeLink);
    Value.FreeNotification(Self);
  end;
end;

procedure TTBCustomItem.SetImages(Value: TCustomImageList);
begin
  if FImages <> Value then
    Change(ChangeImages(FImages, Value, FImagesChangeLink));
end;

procedure TTBCustomItem.SetSubMenuImages(Value: TCustomImageList);
begin
  if FSubMenuImages <> Value then begin
    ChangeImages(FSubMenuImages, Value, FSubMenuImagesChangeLink);
    SubMenuImagesChanged;
  end;
end;

procedure TTBCustomItem.SetInheritOptions(Value: Boolean);
begin
  if FInheritOptions <> Value then begin
    FInheritOptions := Value;
    RefreshOptions;
  end;
end;

procedure TTBCustomItem.SetLinkSubitems(Value: TTBCustomItem);
begin
  if Value = Self then
    Value := nil;
  if FLinkSubitems <> Value then begin
    if Assigned(FLinkSubitems) then
      RemoveFromList(FLinkSubitems.FLinkParents, Self);
    FLinkSubitems := Value;
    if Assigned(Value) then begin
      Value.FreeNotification(Self);
      AddToList(Value.FLinkParents, Self);
    end;
    Notify(tbicSubitemsChanged, -1, nil);
  end;
end;

function TTBCustomItem.FixOptions(const AOptions: TTBItemOptions): TTBItemOptions;
begin
  Result := AOptions;
  if not(tboToolbarStyle in Result) then
    Exclude(Result, tboToolbarSize);
end;

procedure TTBCustomItem.RefreshOptions;
const
  NonInheritedOptions = [tboDefault];
  ChangeOptions = [tboDefault, tboDropdownArrow, tboImageAboveCaption,
    tboNoRotation, tboSameWidth, tboToolbarStyle, tboToolbarSize];
var
  OldOptions, NewOptions: TTBItemOptions;
  I: Integer;
  Item: TTBCustomItem;
begin
  OldOptions := FEffectiveOptions;
  if FInheritOptions and Assigned(FParent) then
    NewOptions := FParent.FEffectiveOptions - NonInheritedOptions
  else
    NewOptions := [];
  NewOptions := FixOptions(NewOptions - FMaskOptions + FOptions);
  if FEffectiveOptions <> NewOptions then begin
    FEffectiveOptions := NewOptions;
    if (OldOptions * ChangeOptions) <> (NewOptions * ChangeOptions) then
      Change(True);
    for I := 0 to FItemCount-1 do begin
      Item := FItems[I].Item;
      if Item.FInheritOptions then
        Item.RefreshOptions;
    end;
  end;
end;

procedure TTBCustomItem.SetMaskOptions(Value: TTBItemOptions);
begin
  if FMaskOptions <> Value then begin
    FMaskOptions := Value;
    RefreshOptions;
  end;
end;

procedure TTBCustomItem.SetOptions(Value: TTBItemOptions);
begin
  Value := FixOptions(Value);
  if FOptions <> Value then begin
    FOptions := Value;
    RefreshOptions;
  end;
end;

procedure TTBCustomItem.SetRadioItem(Value: Boolean);
begin
  if FRadioItem <> Value then begin
    FRadioItem := Value;
    Change(False);
  end;
end;

procedure TTBCustomItem.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then begin
    FVisible := Value;
    Change(True);
  end;
end;

procedure TTBCustomItem.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Items[I].ChangeScale(M, D);
  end;
end;


{ TTBGroupItem }

constructor TTBGroupItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle + [tbisEmbeddedGroup, tbisSubitemsEditable];
end;


{ TTBSubmenuItem }

constructor TTBSubmenuItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle + [tbisSubMenu, tbisSubitemsEditable];
end;

function TTBSubmenuItem.GetDropdownCombo: Boolean;
begin
  Result := tbisCombo in ItemStyle;
end;

procedure TTBSubmenuItem.SetDropdownCombo(Value: Boolean);
begin
  if (tbisCombo in ItemStyle) <> Value then begin
    if Value then
      ItemStyle := ItemStyle + [tbisCombo]
    else
      ItemStyle := ItemStyle - [tbisCombo];
    Change(True);
  end;
end;


{ TTBSeparatorItem }

constructor TTBSeparatorItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle - [tbisSelectable, tbisRedrawOnSelChange,
    tbisRedrawOnMouseOverChange] + [tbisSeparator, tbisClicksTransparent];
end;

function TTBSeparatorItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TTBSeparatorItemViewer;
end;

procedure TTBSeparatorItem.SetBlank(Value: Boolean);
begin
  if FBlank <> Value then begin
    FBlank := Value;
    Change(False);
  end;
end;


{ TTBSeparatorItemViewer }

procedure TTBSeparatorItemViewer.CalcSize(const Canvas: TCanvas;
  var AWidth, AHeight: Integer);
begin
  if not IsToolbarStyle then
    Inc(AHeight, DivRoundUp(GetTextHeight(Canvas.Handle) * 2, 3))
  else begin
    AWidth := 6;
    AHeight := 6;
  end;
end;

procedure TTBSeparatorItemViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsSelected, IsPushed, UseDisabledShadow: Boolean);
var
  DC: HDC;
  R: TRect;
  ToolbarStyle, Horiz, LineSep: Boolean;
begin
  DC := Canvas.Handle;
  if TTBSeparatorItem(Item).FBlank then
    Exit;

  R := ClientAreaRect;
  ToolbarStyle := IsToolbarStyle;
  Horiz := not ToolbarStyle or (View.FOrientation = tbvoVertical);
  LineSep := tbisLineSep in State;
  if LineSep then
    Horiz := not Horiz;
  if Horiz then begin
    R.Top := R.Bottom div 2 - 1;
    if not ToolbarStyle then
      InflateRect(R, -tbMenuSeparatorOffset, 0)
    else if LineSep then begin
      if View.FOrientation = tbvoFloating then
        InflateRect(R, -tbLineSepOffset, 0)
      else
        InflateRect(R, -tbDockedLineSepOffset, 0);
    end;
    DrawEdge(DC, R, EDGE_ETCHED, BF_TOP);
  end
  else begin
    R.Left := R.Right div 2 - 1;
    if LineSep then
      InflateRect(R, 0, -tbDockedLineSepOffset);
    DrawEdge(DC, R, EDGE_ETCHED, BF_LEFT);
  end;
end;

function TTBSeparatorItemViewer.UsesSameWidth: Boolean;
begin
  Result := False;
end;


{ TTBControlItem }

constructor TTBControlItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle - [tbisSelectable] + [tbisClicksTransparent];
end;

constructor TTBControlItem.CreateControlItem(AOwner: TComponent;
  AControl: TControl);
begin
  FControl := AControl;
  AControl.FreeNotification(Self);
  Create(AOwner);
end;

destructor TTBControlItem.Destroy;
begin
  inherited;
  { Free the associated control *after* the item is completely destroyed }
  if not FDontFreeControl and Assigned(FControl) and
     not(csAncestor in FControl.ComponentState) then
    FControl.Free;
end;

procedure TTBControlItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FControl) then
    Control := nil;
end;

procedure TTBControlItem.SetControl(Value: TControl);
begin
  if FControl <> Value then begin
    FControl := Value;
    if Assigned(Value) then
      Value.FreeNotification(Self);
    Change(True);
  end;
end;


{ TTBItemViewer }

constructor TTBItemViewer.Create(AView: TTBView; AItem: TTBCustomItem;
  AGroupLevel: Integer);
begin
  FItem := AItem;
  FView := AView;
  FGroupLevel := AGroupLevel;
  ReferenceClickWnd;
end;

destructor TTBItemViewer.Destroy;
begin
  RemoveFromClickList(Self);
  if Assigned(FAccObjectInstance) then begin
    FAccObjectInstance.ClientIsDestroying;
    FAccObjectInstance := nil;
  end;
  inherited;
  ReleaseClickWnd;
end;

function TTBItemViewer.GetAccObject: IDispatch;
begin
  if FAccObjectInstance = nil then begin
    FAccObjectInstance := TTBItemViewerAccObject.Create(Self);
  end;
  Result := FAccObjectInstance;
end;

procedure TTBItemViewer.AccSelect(const AExecute: Boolean);
{ Called by ClickWndProc when an item of type TTBItemViewer is in ClickList }
var
  Obj: IDispatch;
begin
  { Ensure FAccObjectInstance is created by calling GetAccObject }
  Obj := GetAccObject;
  if Assigned(Obj) then
    (FAccObjectInstance as TTBItemViewerAccObject).HandleAccSelect(AExecute);
end;

procedure TTBItemViewer.PostAccSelect(const AExecute: Boolean);
{ Internally called by TTBItemViewerAccObject. Don't call directly. }
begin
  QueueClick(Self, Ord(AExecute));
end;

function TTBItemViewer.IsAccessible: Boolean;
{ Returns True if MSAA clients should know about the viewer, specifically
  if it's either shown, off-edge, or clipped (in other words, not completely
  invisible/inaccessible). }
begin
  { Note: Can't simply check Item.Visible because the chevron item's Visible
    property is always True }
  Result := Show or OffEdge or Clipped;
end;

function TTBItemViewer.GetCaptionText: String;
var
  P: Integer;
begin
  Result := Item.Caption;
  P := Pos(#9, Result);
  if P <> 0 then
    SetLength(Result, P-1);
  { MP }
  if IsToolbarStyle and not (vsMenuBar in View.Style) then
    Result := StripAccelChars(StripTrailingPunctuation(Result), True);
end;

function TTBItemViewer.GetHintText: String;
var
  P: Integer;
  LongHint: string;
  HintStyleCaption: string;
begin
  if Pos('|', Item.Hint) > 0 then
  begin
    Result := GetShortHint(Item.Hint);
    LongHint := GetLongHint(Item.Hint);
  end
    else
  begin
    LongHint := Item.Hint;
  end;
  HintStyleCaption := StripAccelChars(StripTrailingPunctuation(GetCaptionText));
  { If there is no short hint, use the caption for the hint. Like Office,
    strip any trailing colon or ellipsis. }
  if (Result = '') and (not(tboNoAutoHint in Item.EffectiveOptions) or (LongHint <> '')) and
     (not(tbisSubmenu in Item.ItemStyle) or (tbisCombo in Item.ItemStyle) or
      not CaptionShown) then
    Result := HintStyleCaption;
  { Call associated action's OnHint event handler to post-process the hint }
  if Assigned(Item.ActionLink) and
     (Item.ActionLink.Action is TCustomAction) then begin
    if not TCustomAction(Item.ActionLink.Action).DoHint(Result) then
      Result := '';
    { Note: TControlActionLink.DoShowHint actually misinterprets the result
      of DoHint, but we get it right... }
  end;
  if Result = '' then
    Result := LongHint;
  // "Select all" and "Select All" are still the same
  if SameText(LongHint, Result) then
    LongHint := '';
  if CaptionShown and (LongHint = '') and SameText(Result, HintStyleCaption) then
    Result := '';
  { Add shortcut text }
  if (Result <> '') and Application.HintShortCuts then
  begin
    { Custom shortcut }
    P := Pos(#9, Item.Caption);
    if (P <> 0) and (P < Length(Item.Caption)) then
      Result := Format('%s (%s)', [Result, Copy(Item.Caption, P+ 1, MaxInt)])
    else
      if (Item.ShortCut <> scNone) then
        Result := Format('%s (%s)', [Result, ShortCutToTextFix(Item.ShortCut)]);
  end;
  if LongHint <> '' then
    Result := Result + '|' + GetLongHint(LongHint);
end;

function TTBItemViewer.CaptionShown: Boolean;
begin
  Result := (GetCaptionText <> '') and (not IsToolbarSize or
    (Item.ImageIndex < 0) or (Item.DisplayMode in [nbdmTextOnly, nbdmImageAndText])) or
    (tboImageAboveCaption in Item.EffectiveOptions);
end;

function TTBItemViewer.ImageShown: Boolean;
begin
  {}{should also return false if Images=nil (use UsedImageList?)}
  ImageShown := (Item.ImageIndex >= 0) and
    ((Item.DisplayMode in [nbdmDefault, nbdmImageAndText]) or
     (IsToolbarStyle and (Item.DisplayMode = nbdmTextOnlyInMenus)));
end;

function TTBItemViewer.GetImageList: TCustomImageList;
var
  V: TTBView;
begin
  Result := Item.Images;
  if Assigned(Result) then
    Exit;
  V := View;
  repeat
    if Assigned(V.FCurParentItem) then begin
      Result := V.FCurParentItem.SubMenuImages;
      if Assigned(Result) then
        Break;
    end;
    if Assigned(V.FParentItem) then begin
      Result := V.FParentItem.SubMenuImages;
      if Assigned(Result) then
        Break;
    end;
    V := V.FParentView;
  until V = nil;
end;

function TTBItemViewer.IsRotated: Boolean;
{ Returns True if the caption should be drawn with rotated (vertical) text,
  underneath the image }
begin
  Result := (View.Orientation = tbvoVertical) and
    not (tboNoRotation in Item.EffectiveOptions) and
    not (tboImageAboveCaption in Item.EffectiveOptions);
end;

procedure TTBItemViewer.CalcSize(const Canvas: TCanvas;
  var AWidth, AHeight: Integer);
var
  ToolbarStyle: Boolean;
  DC: HDC;
  TextMetrics: TTextMetric;
  H, LeftMargin: Integer;
  ImgList: TCustomImageList;
  S: String;
  RotatedFont, SaveFont: HFONT;
begin
  ToolbarStyle := IsToolbarStyle;
  DC := Canvas.Handle;
  ImgList := GetImageList;
  if ToolbarStyle then begin
    AWidth := 6;
    AHeight := 6;
  end
  else begin
    AWidth := 0;
    AHeight := 0;
  end;
  if not ToolbarStyle or CaptionShown then begin
    if not IsRotated then begin
      GetTextMetrics(DC, TextMetrics);
      Inc(AHeight, TextMetrics.tmHeight);
      Inc(AWidth, GetTextWidth(DC, GetCaptionText, True));
      if ToolbarStyle then
        Inc(AWidth, 6);
    end
    else begin
      { Vertical text isn't always the same size as horizontal text, so we have
        to select the rotated font into the DC to get an accurate size }
      RotatedFont := CreateRotatedFont(DC);
      SaveFont := SelectObject(DC, RotatedFont);
      GetTextMetrics(DC, TextMetrics);
      Inc(AWidth, TextMetrics.tmHeight);
      Inc(AHeight, GetTextWidth(DC, GetCaptionText, True));
      if ToolbarStyle then
        Inc(AHeight, 6);
      SelectObject(DC, SaveFont);
      DeleteObject(RotatedFont);
    end;
  end;
  if ToolbarStyle and ImageShown and Assigned(ImgList) then begin
    if not IsRotated and not(tboImageAboveCaption in Item.EffectiveOptions) then begin
      Inc(AWidth, ImgList.Width + 1);
      if AHeight < ImgList.Height + 6 then
        AHeight := ImgList.Height + 6;
    end
    else begin
      Inc(AHeight, ImgList.Height);
      if AWidth < ImgList.Width + 7 then
        AWidth := ImgList.Width + 7;
    end;
  end;
  if ToolbarStyle and (tbisSubmenu in Item.ItemStyle) then begin
    if tbisCombo in Item.ItemStyle then
      Inc(AWidth, tbDropdownComboArrowWidth)
    else
    if tboDropdownArrow in Item.EffectiveOptions then begin
      if View.Orientation <> tbvoVertical then
        Inc(AWidth, tbDropdownArrowWidth)
      else
        Inc(AHeight, tbDropdownArrowWidth);
    end;
  end;
  if not ToolbarStyle then begin
    Inc(AHeight, TextMetrics.tmExternalLeading + tbMenuVerticalMargin);
    if Assigned(ImgList) then begin
      H := ImgList.Height + 3;
      if H > AHeight then
        AHeight := H;
      LeftMargin := MulDiv(ImgList.Width + 3, AHeight, H);
    end
    else
      LeftMargin := AHeight;
    Inc(AWidth, LeftMargin + tbMenuImageTextSpace + tbMenuLeftTextMargin +
      tbMenuRightTextMargin);
    S := Item.GetShortCutText;
    if S <> '' then
      Inc(AWidth, (AHeight - 6) + GetTextWidth(DC, S, True));
    Inc(AWidth, AHeight);
  end;
end;

procedure TTBItemViewer.DrawItemCaption(const Canvas: TCanvas; ARect: TRect;
  const ACaption: String; ADrawDisabledShadow: Boolean; AFormat: UINT);
var
  DC: HDC;

  procedure Draw;
  begin
    if not IsRotated then
      DrawText(DC, PChar(ACaption), Length(ACaption), ARect, AFormat)
    else
      DrawRotatedText(DC, ACaption, ARect, AFormat);
  end;

var
  ShadowColor, HighlightColor, SaveTextColor: DWORD;
begin
  DC := Canvas.Handle;
  if not ADrawDisabledShadow then
    Draw
  else begin
    ShadowColor := GetSysColor(COLOR_BTNSHADOW);
    HighlightColor := GetSysColor(COLOR_BTNHIGHLIGHT);
    OffsetRect(ARect, 1, 1);
    SaveTextColor := SetTextColor(DC, HighlightColor);
    Draw;
    OffsetRect(ARect, -1, -1);
    SetTextColor(DC, ShadowColor);
    Draw;
    SetTextColor(DC, SaveTextColor);
  end;
end;

procedure TTBItemViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsSelected, IsPushed, UseDisabledShadow: Boolean);
var
  ShowEnabled, HasArrow: Boolean;
  MenuCheckWidth, MenuCheckHeight: Integer;

  function GetDrawTextFlags: UINT;
  begin
    Result := 0;
    if not AreKeyboardCuesEnabled and (vsUseHiddenAccels in View.FStyle) and
       not(vsShowAccels in View.FState) then
      Result := DT_HIDEPREFIX;
  end;

  procedure DrawSubmenuArrow;
  var
    BR: TRect;
    Bmp: TBitmap;

    procedure DrawWithColor(AColor: TColor);
    const
      ROP_DSPDxax = $00E20746;
    var
      DC: HDC;
      SaveTextColor, SaveBkColor: TColorRef;
    begin
      Canvas.Brush.Color := AColor;
      DC := Canvas.Handle;
      SaveTextColor := SetTextColor(DC, clWhite);
      SaveBkColor := SetBkColor(DC, clBlack);
      BitBlt(DC, BR.Left, BR.Top, MenuCheckWidth, MenuCheckHeight,
        Bmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
      SetBkColor(DC, SaveBkColor);
      SetTextColor(DC, SaveTextColor);
      Canvas.Brush.Style := bsClear;
    end;

  begin
    Bmp := TBitmap.Create;
    try
      Bmp.Monochrome := True;
      Bmp.Width := MenuCheckWidth;
      Bmp.Height := MenuCheckHeight;
      BR := Rect(0, 0, MenuCheckWidth, MenuCheckHeight);
      DrawFrameControl(Bmp.Canvas.Handle, BR, DFC_MENU, DFCS_MENUARROW);
      OffsetRect(BR, ClientAreaRect.Right - MenuCheckWidth,
        ClientAreaRect.Top + ((ClientAreaRect.Bottom - ClientAreaRect.Top) - MenuCheckHeight) div 2);
      if not UseDisabledShadow then begin
        if ShowEnabled and (tbisCombo in Item.ItemStyle) and IsSelected then begin
          OffsetRect(BR, 1, 1);
          DrawWithColor(clBtnText);
        end
        else
          DrawWithColor(Canvas.Font.Color);
      end
      else begin
        OffsetRect(BR, 1, 1);
        DrawWithColor(clBtnHighlight);
        OffsetRect(BR, -1, -1);
        DrawWithColor(clBtnShadow);
      end;
    finally
      Bmp.Free;
    end;
  end;

  procedure DrawDropdownArrow(R: TRect; Rotated: Boolean);

    procedure DrawWithColor(AColor: TColor);
    var
      X, Y: Integer;
      P: array[0..2] of TPoint;
    begin
      X := (R.Left + R.Right) div 2;
      Y := (R.Top + R.Bottom) div 2;
      if not Rotated then begin
        Dec(Y);
        P[0].X := X-2;
        P[0].Y := Y;
        P[1].X := X+2;
        P[1].Y := Y;
        P[2].X := X;
        P[2].Y := Y+2;
      end
      else begin
        Dec(X);
        P[0].X := X;
        P[0].Y := Y+2;
        P[1].X := X;
        P[1].Y := Y-2;
        P[2].X := X-2;
        P[2].Y := Y;
      end;
      Canvas.Pen.Color := AColor;
      Canvas.Brush.Color := AColor;
      Canvas.Polygon(P);
    end;

  begin
    if not UseDisabledShadow then
      DrawWithColor(Canvas.Font.Color)
    else begin
      OffsetRect(R, 1, 1);
      DrawWithColor(clBtnHighlight);
      OffsetRect(R, -1, -1);
      DrawWithColor(clBtnShadow);
    end;
  end;

  function GetDitherBitmap: TBitmap;
  begin
    Result := AllocPatternBitmap(clBtnFace, clBtnHighlight);
    Result.HandleType := bmDDB;  { needed for Win95, or else brush is solid white }
  end;

const
  EdgeStyles: array[Boolean] of UINT = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  CheckMarkPoints: array[0..11] of TPoint = (
    { Black }
    (X: -2; Y: -2), (X: 0; Y:  0), (X:  4; Y: -4),
    (X:  4; Y: -3), (X: 0; Y:  1), (X: -2; Y: -1),
    (X: -2; Y: -2),
    { White }
    (X: -3; Y: -2), (X: -3; Y: -1), (X: 0; Y: 2),
    (X:  5; Y: -3), (X:  5; Y: -5));
var
  ToolbarStyle, ImageIsShown: Boolean;
  R, RC, RD: TRect;
  S: String;
  ImgList: TCustomImageList;
  I, X, Y: Integer;
  Points: array[0..11] of TPoint;
  DrawTextFlags: UINT;
  LeftMargin: Integer;
  TextMetrics: TTextMetric;
begin
  ToolbarStyle := IsToolbarStyle;
  ShowEnabled := Item.Enabled or View.Customizing;
  HasArrow := (tbisSubmenu in Item.ItemStyle) and
    ((tbisCombo in Item.ItemStyle) or (tboDropdownArrow in Item.EffectiveOptions));
  MenuCheckWidth := GetSystemMetricsForControl(View.FWindow, SM_CXMENUCHECK);
  MenuCheckHeight := GetSystemMetricsForControl(View.FWindow, SM_CYMENUCHECK);
  ImgList := GetImageList;
  ImageIsShown := ImageShown and Assigned(ImgList);
  LeftMargin := 0;
  if not ToolbarStyle then begin
    if Assigned(ImgList) then
      LeftMargin := MulDiv(ImgList.Width + 3, ClientAreaRect.Bottom, ImgList.Height + 3)
    else
      LeftMargin := ClientAreaRect.Bottom;
  end;

  { Border }
  RC := ClientAreaRect;
  if ToolbarStyle then begin
    if HasArrow then begin
      if tbisCombo in Item.ItemStyle then begin
        Dec(RC.Right, tbDropdownComboMargin);
        RD := RC;
        Dec(RC.Right, tbDropdownComboArrowWidth - tbDropdownComboMargin);
        RD.Left := RC.Right;
      end
      else begin
        if View.Orientation <> tbvoVertical then
          RD := Rect(RC.Right - tbDropdownArrowWidth - tbDropdownArrowMargin, 0,
            RC.Right - tbDropdownArrowMargin, RC.Bottom)
        else
          RD := Rect(0, RC.Bottom - tbDropdownArrowWidth - tbDropdownArrowMargin,
            RC.Right, RC.Bottom - tbDropdownArrowMargin);
      end;
    end
    else
      SetRectEmpty(RD);
    if (IsSelected and ShowEnabled) or Item.Checked or
       (csDesigning in Item.ComponentState) then begin
      if not(tbisCombo in Item.ItemStyle) then
        DrawEdge(Canvas.Handle, RC, EdgeStyles[IsPushed or Item.Checked], BF_RECT)
      else begin
        DrawEdge(Canvas.Handle, RC, EdgeStyles[(IsPushed and View.FCapture) or Item.Checked], BF_RECT);
        if (IsSelected and ShowEnabled) or
           (csDesigning in Item.ComponentState) then
          DrawEdge(Canvas.Handle, RD, EdgeStyles[IsPushed and not View.FCapture], BF_RECT);
      end;
    end;
    if HasArrow then begin
      if not(tbisCombo in Item.ItemStyle) and IsPushed then
        OffsetRect(RD, 1, 1);
      DrawDropdownArrow(RD, not(tbisCombo in Item.ItemStyle) and
        (View.Orientation = tbvoVertical));
    end;
    InflateRect(RC, -1, -1);
    if Item.Checked and not (IsSelected and ShowEnabled) then begin
      Canvas.Brush.Bitmap := GetDitherBitmap;
      Canvas.FillRect(RC);
      Canvas.Brush.Style := bsClear;
    end;
    InflateRect(RC, -1, -1);
    if Item.Checked or
       ((IsSelected and IsPushed) and
        (not(tbisCombo in Item.ItemStyle) or View.FCapture)) then
      OffsetRect(RC, 1, 1);
    if HasArrow and not(tbisCombo in Item.ItemStyle) then begin
      if View.Orientation <> tbvoVertical then
        Dec(RC.Right, tbDropdownArrowWidth)
      else
        Dec(RC.Bottom, tbDropdownArrowWidth);
    end;
  end
  else begin
    { On selected menu items, fill the background with the selected color.
      Note: This assumes the brush color was not changed from the initial
      value. }
    if IsSelected then begin
      R := RC;
      if ImageIsShown or Item.Checked then
        Inc(R.Left, LeftMargin + tbMenuImageTextSpace);
      if (tbisCombo in Item.ItemStyle) and IsSelected and ShowEnabled then
        Dec(R.Right, MenuCheckWidth);
      Canvas.FillRect(R);
    end;
  end;

  { Adjust brush & font }
  Canvas.Brush.Style := bsClear;
  if tboDefault in Item.EffectiveOptions then
    with Canvas.Font do Style := Style + [fsBold];
  GetTextMetrics(Canvas.Handle, TextMetrics);

  { Caption }
  if CaptionShown then begin
    S := GetCaptionText;
    R := RC;
    DrawTextFlags := GetDrawTextFlags;
    if ToolbarStyle then begin
      if ImageIsShown then begin
        if not IsRotated and not(tboImageAboveCaption in Item.EffectiveOptions) then
          Inc(R.Left, ImgList.Width + 1)
        else
          Inc(R.Top, ImgList.Height + 1);
      end;
      DrawItemCaption(Canvas, R, S, UseDisabledShadow,
        DT_SINGLELINE or DT_CENTER or DT_VCENTER or DrawTextFlags)
    end
    else begin
      Inc(R.Left, LeftMargin + tbMenuImageTextSpace + tbMenuLeftTextMargin);
      { Like standard menus, shift the text up one pixel if the text height
        is 4 pixels less than the total item height. This is done so underlined
        characters aren't displayed too low. }
      if (R.Bottom - R.Top) - (TextMetrics.tmHeight + TextMetrics.tmExternalLeading) = tbMenuVerticalMargin then
        Dec(R.Bottom);
      Inc(R.Top, TextMetrics.tmExternalLeading);
      DrawItemCaption(Canvas, R, S, UseDisabledShadow,
        DT_SINGLELINE or DT_LEFT or DT_VCENTER or DrawTextFlags);
    end;
  end;

  { Shortcut and/or submenu arrow (menus only) }
  if not ToolbarStyle then begin
    S := Item.GetShortCutText;
    if S <> '' then begin
      R := RC;
      R.Left := R.Right - (R.Bottom - R.Top) - GetTextWidth(Canvas.Handle, S, True);
      { Like standard menus, shift the text up one pixel if the text height
        is 4 pixels less than the total item height. This is done so underlined
        characters aren't displayed too low. }
      if (R.Bottom - R.Top) - (TextMetrics.tmHeight + TextMetrics.tmExternalLeading) = tbMenuVerticalMargin then
        Dec(R.Bottom);
      Inc(R.Top, TextMetrics.tmExternalLeading);
      DrawItemCaption(Canvas, R, S, UseDisabledShadow,
        DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_NOPREFIX);
    end;
    if tbisSubmenu in Item.ItemStyle then begin
      if tbisCombo in Item.ItemStyle then begin
        R := RC;
        R.Left := R.Right - MenuCheckWidth;
        if IsSelected and ShowEnabled then
          DrawEdge(Canvas.Handle, R, BDR_SUNKENOUTER, BF_RECT or BF_MIDDLE)
        else begin
          Dec(R.Left);
          if not IsSelected then
            DrawEdge(Canvas.Handle, R, EDGE_ETCHED, BF_LEFT)
          else
            DrawEdge(Canvas.Handle, R, BDR_SUNKENOUTER, BF_LEFT);
        end;
      end;
      DrawSubmenuArrow;
    end;
  end;

  { Image, or check box }
  if ImageIsShown or (not ToolbarStyle and Item.Checked) then begin
    R := RC;
    if ToolbarStyle then begin
      if not IsRotated and not(tboImageAboveCaption in Item.EffectiveOptions) then
        R.Right := R.Left + ImgList.Width + 2
      else
        R.Bottom := R.Top + ImgList.Height + 2;
    end
    else begin
      R.Right := R.Left + LeftMargin;
      if (IsSelected and ShowEnabled) or Item.Checked then
        DrawEdge(Canvas.Handle, R, EdgeStyles[Item.Checked], BF_RECT or BF_MIDDLE);
      if Item.Checked and not IsSelected then begin
        InflateRect(R, -1, -1);
        Canvas.Brush.Bitmap := GetDitherBitmap;
        Canvas.FillRect(R);
        Canvas.Brush.Style := bsClear;
        InflateRect(R, 1, 1);
      end;
      if Item.Checked then
        OffsetRect(R, 1, 1);
    end;
    if ImageIsShown then begin
      X := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
      Y := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
      if ImgList is TTBCustomImageList then
        TTBCustomImageList(ImgList).DrawState(Canvas, X, Y, Item.ImageIndex,
          ShowEnabled, IsSelected, Item.Checked)
      else
        ImgList.Draw(Canvas, X, Y, Item.ImageIndex, ShowEnabled);
    end
    else
      if not ToolbarStyle and Item.Checked then begin
        { Draw default check mark or radio button image when user hasn't
          specified their own }
        X := (R.Left + R.Right) div 2;
        Y := (R.Top + R.Bottom) div 2;
        if Item.RadioItem then begin
          Canvas.Pen.Color := clBtnText;
          Canvas.Brush.Color := clBtnText;
          Canvas.RoundRect(X-3, Y-3, X+2, Y+2, 2, 2);
          Canvas.Pen.Color := clBtnHighlight;
          Canvas.Brush.Style := bsClear;
          Canvas.RoundRect(X-4, Y-4, X+3, Y+3, 6, 6);
        end
        else begin
          Dec(X, 2);
          Inc(Y);
          System.Move(CheckMarkPoints, Points, 12 * SizeOf(TPoint));
          for I := Low(Points) to High(Points) do begin
            Inc(Points[I].X, X);
            Inc(Points[I].Y, Y);
          end;
          Canvas.Pen.Color := clBtnText;
          Polyline(Canvas.Handle, Points[0], 7);
          Canvas.Pen.Color := clBtnHighlight;
          Polyline(Canvas.Handle, Points[7], 5);
        end;
      end;
  end;
end;

procedure TTBItemViewer.GetCursor(const Pt: TPoint; var ACursor: HCURSOR);
begin
end;

function TTBItemViewer.GetIndex: Integer;
begin
  Result := View.IndexOf(Self);
end;

function TTBItemViewer.IsToolbarSize: Boolean;
begin
  Result := View.FIsToolbar or (tboToolbarSize in Item.FEffectiveOptions);
end;

function TTBItemViewer.IsToolbarStyle: Boolean;
begin
  Result := View.FIsToolbar or (tboToolbarStyle in Item.FEffectiveOptions);
end;

function TTBItemViewer.IsPtInButtonPart(X, Y: Integer): Boolean;
var
  W: Integer;
begin
  Result := not(tbisSubmenu in Item.ItemStyle);
  if tbisCombo in Item.ItemStyle then begin
    if IsToolbarStyle then
      W := tbDropdownComboArrowWidth
    else
      W := GetSystemMetricsForControl(View.FWindow, SM_CXMENUCHECK);
    Result := X < (BoundsRect.Right - BoundsRect.Left) - W;
  end;
end;

procedure TTBItemViewer.MouseDown(Shift: TShiftState; X, Y: Integer;
  var MouseDownOnMenu: Boolean);

  procedure HandleDefaultDoubleClick(const View: TTBView);
  { Looks for a tboDefault item in View and ends the modal loop if it finds
    one. }
  var
    I: Integer;
    Viewer: TTBItemViewer;
    Item: TTBCustomItem;
  begin
    for I := 0 to View.FViewerCount-1 do begin
      Viewer := View.FViewers[I];
      Item := Viewer.Item;
      if (Viewer.Show or Viewer.Clipped) and (tboDefault in Item.EffectiveOptions) and
         (tbisSelectable in Item.ItemStyle) and Item.Enabled and Item.Visible then begin
        Viewer.Execute(True);
        Break;
      end;
    end;
  end;

var
  WasAlreadyOpen: Boolean;
begin
  if not Item.Enabled then begin
    if (View.FParentView = nil) and not View.FIsPopup then
      View.EndModal;
    Exit;
  end;
  if IsPtInButtonPart(X, Y) then begin
    if IsToolbarStyle then begin
      View.CancelChildPopups;
      View.SetCapture;
      View.Invalidate(Self);
    end;
  end
  else begin
    WasAlreadyOpen := (View.FOpenViewer = Self);
    if View.OpenChildPopup(False) then begin
      if WasAlreadyOpen and ((View.FParentView = nil) and not View.FIsPopup) then
        MouseDownOnMenu := True;
      if (ssDouble in Shift) and not(tbisCombo in Item.ItemStyle) then
        HandleDefaultDoubleClick(View.FOpenViewerView);
    end;
  end;
end;

procedure TTBItemViewer.MouseMove(X, Y: Integer);
begin
end;

procedure TTBItemViewer.MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean);
var
  HadCapture, IsToolbarItem: Boolean;
begin
  HadCapture := View.FCapture;
  View.CancelCapture;
  IsToolbarItem := (View.FParentView = nil) and not View.FIsPopup;
  if not View.FMouseOverSelected or not Item.Enabled or
     (tbisClicksTransparent in Item.ItemStyle) then begin
    if IsToolbarItem then
      View.EndModal;
    Exit;
  end;
  if (tbisSubmenu in Item.ItemStyle) and not IsPtInButtonPart(X, Y) then begin
    if IsToolbarItem and MouseWasDownOnMenu then
      View.EndModal;
  end
  else begin
    { it's a 'normal' item }
    if not IsToolbarStyle or HadCapture then
      Execute(True);
  end;
end;

procedure TTBItemViewer.MouseWheel(WheelDelta, X, Y: Integer);
begin
end;

procedure TTBItemViewer.LosingCapture;
begin
  View.Invalidate(Self);
end;

procedure TTBItemViewer.Entering(OldSelected: TTBItemViewer);
begin
  if Assigned(Item.FOnSelect) then
    Item.FOnSelect(Item, Self, True);
end;

procedure TTBItemViewer.Leaving;
begin
  if Assigned(Item.FOnSelect) then
    Item.FOnSelect(Item, Self, False);
end;

procedure TTBItemViewer.KeyDown(var Key: Word; Shift: TShiftState);
begin
end;

function TTBItemViewer.ScreenToClient(const P: TPoint): TPoint;
begin
  Result := View.FWindow.ScreenToClient(P);
  Dec(Result.X, BoundsRect.Left);
  Dec(Result.Y, BoundsRect.Top);
end;

function TTBItemViewer.UsesSameWidth: Boolean;
{ If UsesSameWidth returns True, the item viewer's width will be expanded to
  match the widest item viewer on the same view whose UsesSameWidth method
  also returns True. }
begin
  Result := (tboImageAboveCaption in Item.FEffectiveOptions) and
    (tboSameWidth in Item.FEffectiveOptions) and IsToolbarSize;
end;

function TTBItemViewer.DoExecute: Boolean;
{ Low-level 'execute' handler. Returns True if the caller should call
  GivePriority on the viewer (normally, if the 'execute' operation was a
  success and the modal loop is ending). }
begin
  View.EndModalWithClick(Self);
  Result := True;
end;

procedure TTBItemViewer.Execute(AGivePriority: Boolean);
{ Calls DoExecute and, if applicable, View.GivePriority. Note that it is up to
  the caller to check the viewer's visibility and enabled state. }
begin
  if DoExecute and AGivePriority then
    View.GivePriority(Self);
end;

function TTBItemViewer.GetAccRole: Integer;
{ Returns the MSAA "role" of the viewer. }
const
  { Constants from OleAcc.h }
  ROLE_SYSTEM_CLIENT = $a;
  ROLE_SYSTEM_MENUITEM = $c;
  ROLE_SYSTEM_SEPARATOR = $15;
  ROLE_SYSTEM_PUSHBUTTON = $2b;
  ROLE_SYSTEM_BUTTONMENU = $39;
begin
  if Item is TTBControlItem then
    Result := ROLE_SYSTEM_CLIENT
  else if tbisSeparator in Item.ItemStyle then
    Result := ROLE_SYSTEM_SEPARATOR
  else if View.IsPopup or (vsMenuBar in View.Style) then
    Result := ROLE_SYSTEM_MENUITEM
  else if tbisSubmenu in Item.ItemStyle then
    Result := ROLE_SYSTEM_BUTTONMENU
  else
    Result := ROLE_SYSTEM_PUSHBUTTON;
end;

function TTBItemViewer.GetAccValue(var Value: WideString): Boolean;
{ Gets the MSAA "value" text of the viewer. Returns True if something was
  assigned to Value, or False if the viewer does not possess a "value". }
begin
  Result := False;
end;


{ TTBView }

constructor TTBView.CreateView(AOwner: TComponent; AParentView: TTBView;
  AParentItem: TTBCustomItem; AWindow: TWinControl;
  AIsToolbar, ACustomizing, AUsePriorityList: Boolean);
begin
  Create(AOwner);
  FBackgroundColor := clDefault;
  FCustomizing := ACustomizing;
  FIsPopup := not AIsToolbar;
  FIsToolbar := AIsToolbar;
  FNewViewersGetHighestPriority := True;
  FParentView := AParentView;
  FParentItem := AParentItem;
  if Assigned(FParentItem) then begin
    //FIsToolbar := FIsToolbar or FParentItem.FDisplayAsToolbar;
    FParentItem.RegisterNotification(LinkNotification);
    FParentItem.FreeNotification(Self);
  end;
  FUsePriorityList := AUsePriorityList;
  FWindow := AWindow;
  UpdateCurParentItem;
end;

destructor TTBView.Destroy;
begin
  CloseChildPopups;
  if Assigned(FAccObjectInstance) then begin
    FAccObjectInstance.ClientIsDestroying;
    { Get rid of our own reference to FAccObjectInstance. Normally the
      reference count will be now be zero and FAccObjectInstance will be
      freed, unless MSAA still holds a reference. }
    FAccObjectInstance._Release;
    FAccObjectInstance := nil;
  end;
  { If parent view is a toolbar, invalidate the open item so that it's
    redrawn back in the "up" position }
  if Assigned(ParentView) and ParentView.FIsToolbar then begin
    Include(ParentView.FState, vsNoAnimation);
    if Assigned(ParentView.FOpenViewer) then
      ParentView.Invalidate(ParentView.FOpenViewer);
  end;
  if Assigned(FCurParentItem) then
    FCurParentItem.UnregisterNotification(ItemNotification);
  if Assigned(FParentItem) then
    FParentItem.UnregisterNotification(LinkNotification);
  inherited;
  FPriorityList.Free;
  FreeViewers;
  { Now that we're destroyed, "focus" the parent view }
  if Assigned(FParentView) then
    FParentView.NotifyFocusEvent;
end;

function TTBView.GetAccObject: IDispatch;
begin
  if FAccObjectInstance = nil then begin
    FAccObjectInstance := TTBViewAccObject.Create(Self);
    { Strictly as an optimization, take a reference for ourself and keep it
      for the lifetime of the view. (Destroy calls _Release.) }
    FAccObjectInstance._AddRef;
  end;
  Result := FAccObjectInstance;
end;

function TTBView.HandleWMGetObject(var Message: TMessage): Boolean;
begin
  if (Message.LParam = Integer(OBJID_CLIENT)) then begin
    Message.Result := LresultFromObject(ITBAccessible, Message.WParam, GetAccObject);
    Result := True;
  end
  else
    Result := False;
end;

procedure TTBView.UpdateCurParentItem;
var
  Value: TTBCustomItem;
begin
  Value := ItemContainingItems(FParentItem);
  if FCurParentItem <> Value then begin
    CloseChildPopups;
    if Assigned(FCurParentItem) then
      FCurParentItem.UnregisterNotification(ItemNotification);
    FCurParentItem := Value;
    if Assigned(Value) then
      Value.RegisterNotification(ItemNotification);
    RecreateAllViewers;
    if Assigned(Value) and not(csDesigning in Value.ComponentState) then
      InitiateActions;
  end;
end;

procedure TTBView.InitiateActions;
var
  I: Integer;
begin
  { Use a 'while' instead of a 'for' since an InitiateAction implementation
    may add/delete items }
  I := 0;
  while I < FViewerCount do begin
    FViewers[I].Item.InitiateAction;
    Inc(I);
  end;
end;

procedure TTBView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FParentItem then begin
      FParentItem := nil;
      UpdateCurParentItem;
      if Assigned(FParentView) then
        FParentView.CloseChildPopups;
    end
    else if AComponent = FOpenViewerWindow then begin
      FOpenViewerWindow := nil;
      FOpenViewerView := nil;
      FOpenViewer := nil;
    end
    else if AComponent = FChevronParentView then
      FChevronParentView := nil;
  end
end;

function TTBView.ContainsView(AView: TTBView): Boolean;
begin
  while Assigned(AView) and (AView <> Self) do
    AView := AView.FParentView;
  Result := Assigned(AView);
end;

function TTBView.GetRootView: TTBView;
begin
  Result := Self;
  while Assigned(Result.FParentView) do
    Result := Result.FParentView;
end;

function TTBView.GetParentToolbarView: TTBView;
begin
  Result := Self;
  while Assigned(Result) and not Result.FIsToolbar do
    Result := Result.FParentView;
end;

procedure TTBView.FreeViewers;
var
  VI: PTBItemViewerArray;
  I, C: Integer;
begin
  if Assigned(FViewers) then begin
    VI := FViewers;
    C := FViewerCount;
    FViewers := nil;
    FViewerCount := 0;
    for I := C-1 downto 0 do
      FreeAndNil(VI[I]);
    FreeMem(VI);
  end;
end;

procedure TTBView.InvalidatePositions;
begin
  if FValidated then begin
    FValidated := False;
    if Assigned(FWindow) and FWindow.HandleAllocated then
      InvalidateRect(FWindow.Handle, nil, True);
  end;
end;

procedure TTBView.ValidatePositions;
begin
  if not FValidated then
    UpdatePositions;
end;

procedure TTBView.TryValidatePositions;
begin
  if (FUpdating = 0) and
     (not Assigned(FParentItem) or not(csLoading in FParentItem.ComponentState)) and
     (not Assigned(FParentItem.Owner) or not(csLoading in FParentItem.Owner.ComponentState)) then
    ValidatePositions;
end;

(*procedure TTBView.TryRevalidatePositions;
begin
  if FValidated then begin
    if FUpdating = 0 then begin
      FreePositions;
      UpdatePositions;
    end
    else
      InvalidatePositions;
  end;
end;*)

function TTBView.Find(Item: TTBCustomItem): TTBItemViewer;
var
  I: Integer;
begin
  for I := 0 to FViewerCount-1 do
    if FViewers[I].Item = Item then begin
      Result := FViewers[I];
      Exit;
    end;
  raise ETBItemError.Create(STBViewerNotFound);
end;

function TTBView.IndexOf(AViewer: TTBItemViewer): Integer;
var
  I: Integer;
begin
  if Assigned(AViewer) then
    for I := 0 to FViewerCount-1 do
      if FViewers[I] = AViewer then begin
        Result := I;
        Exit;
      end;
  Result := -1;
end;


procedure TTBView.DeletingViewer(Viewer: TTBItemViewer);
begin
  if FSelected = Viewer then
    FSelected := nil;
  if FOpenViewer = Viewer then
    CloseChildPopups;
end;

procedure TTBView.RecreateItemViewer(const I: Integer);
var
  OldViewer, NewViewer: TTBItemViewer;
  J: Integer;
begin
  OldViewer := FViewers[I];
  DeletingViewer(OldViewer);
  NewViewer := OldViewer.Item.GetItemViewerClass(Self).Create(Self,
    OldViewer.Item, OldViewer.FGroupLevel);
  FViewers[I] := NewViewer;
  if Assigned(FPriorityList) then begin
    J := FPriorityList.IndexOf(OldViewer);
    if J <> -1 then
      FPriorityList[J] := NewViewer;
  end;
  OldViewer.Free;
end;

function TTBView.InsertItemViewers(const NewIndex: Integer;
  const AItem: TTBCustomItem; const AGroupLevel: Integer;
  const AddToPriorityList, TopOfPriorityList: Boolean): Integer;
var
  NewViewer: TTBItemViewer;
  LinkItem: TTBCustomItem;
  I: Integer;
begin
  if AGroupLevel > MaxGroupLevel then begin
    Result := 0;
    Exit;
  end;

  NewViewer := AItem.GetItemViewerClass(Self).Create(Self, AItem,
    AGroupLevel);
  InsertIntoViewerArray(FViewers, FViewerCount, NewIndex,
    NewViewer);
  if AddToPriorityList and FUsePriorityList then begin
    if not TopOfPriorityList then
      AddToList(FPriorityList, NewViewer)
    else
      { When new items are inserted programmatically at run-time, place
        them at the top of FPriorityList }
      AddToFrontOfList(FPriorityList, NewViewer);
  end;
  Result := 1;

  { If a new group item is being inserted, insert all its child items too }
  if not FCustomizing and (tbisEmbeddedGroup in AItem.ItemStyle) then begin
    LinkItem := ItemContainingItems(AItem);
    for I := 0 to LinkItem.Count-1 do begin
      Inc(Result, InsertItemViewers(NewIndex + Result, LinkItem.FItems[I].Item,
        AGroupLevel + 1, AddToPriorityList, TopOfPriorityList));
    end;
  end;
end;

procedure TTBView.ItemNotification(Ancestor: TTBCustomItem; Relayed: Boolean;
  Action: TTBItemChangedAction; Index: Integer; Item: TTBCustomItem);

  procedure ItemInserted;
  var
    NewLevel, Start, InsertPoint, Last: Integer;
    GroupItem, NextItem: TTBCustomItem;
    Found, SearchAgain: Boolean;
  begin
    InvalidatePositions;
    NewLevel := 0;
    Start := 0;
    if Ancestor = FCurParentItem then
      InsertPoint := FViewerCount
    else begin
      { Ancestor <> FCurParentItem, so apparently an item has been inserted
        inside a group item }
      repeat
        Found := False;
        while Start < FViewerCount do begin
          GroupItem := FViewers[Start].Item;
          if (tbisEmbeddedGroup in GroupItem.ItemStyle) and (GroupItem = Ancestor) then begin
            NewLevel := FViewers[Start].FGroupLevel + 1;
            Inc(Start);
            Found := True;
            Break;
          end;
          Inc(Start);
        end;
        if not Found then
          { Couldn't find Ancestor; it shouldn't get here }
          Exit;
        InsertPoint := Start;
        SearchAgain := False;
        while (InsertPoint < FViewerCount) and
           (FViewers[InsertPoint].FGroupLevel >= NewLevel) do begin
          if (FViewers[InsertPoint].Item = Item) and
             (FViewers[InsertPoint].FGroupLevel = NewLevel) then begin
            { If the item we were going to insert already exists, then there
              must be multiple instances of the same group item. This can
              happen when are two group items on the same toolbar each
              linking to the same submenu item, with the submenu item
              containing a group item of its own, and an item is inserted
              inside that. }
            SearchAgain := True;
            Break;
          end;
          Inc(InsertPoint);
        end;
      until not SearchAgain;
    end;
    if InsertPoint = FViewerCount then begin
      { Don't add items after the chevron or MDI buttons item }
      Dec(InsertPoint, FInternalViewersAtEnd);
      if InsertPoint < 0 then
        InsertPoint := 0;  { just in case? }
    end;
    { If the new item wasn't placed at the end, adjust InsertPoint accordingly }
    if Index < Item.Parent.Count-1 then begin
      Last := InsertPoint;
      InsertPoint := Start;
      NextItem := Item.Parent.FItems[Index+1].Item;
      while (InsertPoint < Last) and
         ((FViewers[InsertPoint].Item <> NextItem) or
          (FViewers[InsertPoint].FGroupLevel <> NewLevel)) do
        Inc(InsertPoint);
    end;
    InsertItemViewers(InsertPoint, Item, NewLevel, True,
      not(csLoading in Item.ComponentState) and FNewViewersGetHighestPriority);
  end;

  procedure ItemDeleting;

    procedure DeleteItem(DeleteIndex: Integer);
    var
      Viewer: TTBItemViewer;
    begin
      Viewer := FViewers[DeleteIndex];
      DeletingViewer(Viewer);
      RemoveFromList(FPriorityList, Viewer);
      FreeAndNil(Viewer);
      DeleteFromViewerArray(FViewers, FViewerCount, DeleteIndex);
    end;

  var
    I: Integer;
    DeleteLevel: Integer;
  begin
    InvalidatePositions;
    I := 0;
    DeleteLevel := 0;
    while I < FViewerCount do begin
      if DeleteLevel > 0 then begin
        if FViewers[I].FGroupLevel >= DeleteLevel then begin
          DeleteItem(I);
          Continue;
        end
        else
          DeleteLevel := 0;
      end;
      if FViewers[I].Item = Item then begin
        { Delete the item, and any group item children afterward }
        DeleteLevel := FViewers[I].FGroupLevel + 1;
        DeleteItem(I);
        Continue;
      end;
      Inc(I);
    end;
  end;

var
  I: Integer;
begin
  case Action of
    tbicInserted: ItemInserted;
    tbicDeleting: ItemDeleting;
    tbicSubitemsChanged: begin
        { If Relayed=True, LinkSubitems must have changed on a child group
          item. Currently there isn't any optimized way of handling this
          situation; just recreate all viewers. }
        if Relayed then
          RecreateAllViewers;
      end;
    tbicSubitemsBeginUpdate: BeginUpdate;
    tbicSubitemsEndUpdate: EndUpdate;
    tbicInvalidate: begin
        for I := 0 to FViewerCount-1 do
          if FViewers[I].Item = Item then
            Invalidate(FViewers[I]);
      end;
    tbicInvalidateAndResize: InvalidatePositions;
    tbicRecreateItemViewers: begin
        InvalidatePositions;
        for I := 0 to FViewerCount-1 do
          if FViewers[I].Item = Item then
            RecreateItemViewer(I);
      end;
    tbicSubMenuImagesChanged: ImagesChanged;
  else
    { Prevent TryValidatePositions from being called below on Actions other than
      those listed above. Currently there are no other Actions, but for forward
      compatibility, we should ignore unknown Actions completely. }
    Exit;
  end;
  TryValidatePositions;
end;

procedure TTBView.LinkNotification(Ancestor: TTBCustomItem; Relayed: Boolean;
  Action: TTBItemChangedAction; Index: Integer; Item: TTBCustomItem);
{ This notification procedure watches for tbicSubitemsChanged notifications
  from FParentItem }
begin
  case Action of
    tbicSubitemsChanged: begin
        { LinkSubitems may have changed on FParentItem, e.g. on the root item
          of a toolbar, so see if FCurParentItem needs updating }
        UpdateCurParentItem;
      end;
    tbicSubMenuImagesChanged: begin
        { In case the images were inherited from the actual parent instead of
          the linked parent... }
        if FParentItem <> FCurParentItem then
          ImagesChanged;
      end;
  end;
end;

procedure TTBView.ImagesChanged;
begin
  InvalidatePositions;
  TryValidatePositions;
  if Assigned(FOpenViewerView) then
    FOpenViewerView.ImagesChanged;
end;

procedure TTBView.GivePriority(AViewer: TTBItemViewer);
{ Move item to top of priority list. Rearranges items if necessary. }
var
  I: Integer;
begin
  if Assigned(FChevronParentView) then begin
    I := AViewer.Index + FChevronParentView.FInternalViewersAtFront;
    if I < FChevronParentView.FViewerCount then  { range check just in case }
      FChevronParentView.GivePriority(FChevronParentView.FViewers[I]);
    Exit;
  end;
  if Assigned(FPriorityList) then begin
    I := FPriorityList.IndexOf(AViewer);
    if I <> -1 then begin
      FPriorityList.Move(I, 0);
      if not FValidated or AViewer.OffEdge then
        UpdatePositions;
    end;
  end;
  { Call GivePriority on parent view, so that if an item on a submenu is
    clicked, the parent item of the submenu gets priority. }
  if Assigned(FParentView) and Assigned(FParentView.FOpenViewer) then
    FParentView.GivePriority(FParentView.FOpenViewer);
end;

function TTBView.HighestPriorityViewer: TTBItemViewer;
{ Returns index of first visible, non-separator item at top of priority list,
  or -1 if there are no items found }
var
  I: Integer;
  J: TTBItemViewer;
begin
  ValidatePositions;
  Result := nil;
  if Assigned(FPriorityList) then begin
    for I := 0 to FPriorityList.Count-1 do begin
      J := FPriorityList[I];
      if J.Show and not(tbisSeparator in J.Item.ItemStyle) then begin
        Result := J;
        Break;
      end;
    end;
  end
  else begin
    for I := 0 to FViewerCount-1 do begin
      J := FViewers[I];
      if J.Show and not(tbisSeparator in J.Item.ItemStyle) then begin
        Result := J;
        Break;
      end;
    end;
  end;
end;

procedure TTBView.StartTimer(const ATimer: TTBViewTimerID;
  const Interval: Integer);
{ Starts a timer. Stops any previously set timer of the same ID first.
  Note: WM_TIMER messages generated by timers set by the method are handled
  in PopupMessageLoop. }
begin
  StopTimer(ATimer);
  if (FWindow is TTBPopupWindow) and FWindow.HandleAllocated then begin
    SetTimer(FWindow.Handle, ViewTimerBaseID + Ord(ATimer), Interval, nil);
    Include(FActiveTimers, ATimer);
  end;
end;

procedure TTBView.StopAllTimers;
var
  I: TTBViewTimerID;
begin
  for I := Low(I) to High(I) do
    StopTimer(I);
end;

procedure TTBView.StopTimer(const ATimer: TTBViewTimerID);
begin
  if ATimer in FActiveTimers then begin
    if (FWindow is TTBPopupWindow) and FWindow.HandleAllocated then
      KillTimer(FWindow.Handle, ViewTimerBaseID + Ord(ATimer));
    Exclude(FActiveTimers, ATimer);
  end;
end;

function TTBView.OpenChildPopup(const SelectFirstItem: Boolean): Boolean;
var
  Item: TTBCustomItem;
begin
  StopTimer(tiClose);
  StopTimer(tiOpen);
  if FSelected <> FOpenViewer then begin
    CloseChildPopups;
    if Assigned(FSelected) then begin
      Item := FSelected.Item;
      if Item.Enabled and (tbisSubmenu in Item.ItemStyle) then
        Item.CreatePopup(Self, FSelected, not FIsToolbar, SelectFirstItem,
          False, Point(0, 0), tbpaLeft);
    end;
  end;
  Result := Assigned(FOpenViewer);
end;

procedure TTBView.CloseChildPopups;
begin
  if Assigned(FOpenViewerView) then
    FOpenViewerView.CloseChildPopups;
  StopTimer(tiClose);
  FOpenViewerWindow.Free;
  FOpenViewerWindow := nil;
  FOpenViewerView := nil;
  FOpenViewer := nil;
end;

procedure TTBView.CancelChildPopups;
begin
  if FIsToolbar then
    Exclude(FState, vsDropDownMenus);
  {MP}
  if Assigned(FOpenViewerWindow) then
    FOpenViewerWindow.Cancel;
  CloseChildPopups;
end;

function TTBView.ViewerFromPoint(const P: TPoint): TTBItemViewer;
var
  I: Integer;
begin
  ValidatePositions;
  for I := 0 to FViewerCount-1 do begin
    if FViewers[I].Show and
       PtInRect(FViewers[I].BoundsRect, P) then begin
      Result := FViewers[I];
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TTBView.NotifyFocusEvent;
{ Notifies Active Accessibility of a change in "focus". Has no effect if the
  view or the root view lacks the vsModal state, or if the modal loop is
  ending (EndModal* was called). }
var
  I, ChildID, J: Integer;
begin
  { Note: We don't notify about windows not yet shown (e.g. a popup menu that
    is still initializing) because that would probably confuse screen readers.
    Also allocating a window handle at this point *might* not be a good idea. }
  if (vsModal in FState) and (vsModal in GetRootView.FState) and
     not IsModalEnding and
     FWindow.HandleAllocated and IsWindowVisible(FWindow.Handle) then begin
    if Assigned(FSelected) and FSelected.IsAccessible then
      I := IndexOf(FSelected)
    else
      I := -1;
    if (I < 0) and Assigned(FParentView) then begin
      { If we have no selected item, report the the selected item on the parent
        view as having the "focus".
        Note: With standard menus, when you go from having a selection to no
        selection on a submenu, it sends two focus events - first with the
        client window as having the focus, then with the parent item. I
        figure that's probably a bug, so I don't try to emulate that behavior
        here. }
      FParentView.NotifyFocusEvent;
    end
    else begin
      if I >= 0 then begin
        { Convert viewer index into a one-based child index.
          (TTBViewAccObject.get_accChild does the inverse.) }
        ChildID := 1;
        for J := 0 to I-1 do
          if FViewers[J].IsAccessible then
            Inc(ChildID);
      end
      else begin
        { If there is no (accessible) selection and no parent view, report
          the client window itself as being "focused". This is what happens
          when a standard context menu has no selection. }
        ChildID := CHILDID_SELF;
      end;
      NotifyWinEvent(EVENT_OBJECT_FOCUS, FWindow.Handle, OBJID_CLIENT, ChildID);
    end;
  end;
end;

procedure TTBView.SetSelected(Value: TTBItemViewer);
begin
  Select(Value, False);
end;

procedure TTBView.Select(Value: TTBItemViewer; ViaMouse: Boolean);
{ Sets the current selection.
  When the selection is changing it will also, if necessary, open/close child
  popups. How exactly this works depends on the setting of ViaMouse. If
  ViaMouse is True it will delay the opening/closing of popups using timers. }
var
  OldSelected: TTBItemViewer;
  NewMouseOverSelected: Boolean;
  P: TPoint;
begin
  OldSelected := FSelected;
  if Value <> OldSelected then begin
    { If there's a new selection and the parent item on the parent view
      isn't currently selected, select it. Also stop any timer running on
      the parent view. }
    if Assigned(Value) and Assigned(FParentView) and
       Assigned(FParentView.FOpenViewer) and
       (FParentView.FSelected <> FParentView.FOpenViewer) then begin
      FParentView.Selected := FParentView.FOpenViewer;
      FParentView.StopTimer(tiClose);
      FParentView.StopTimer(tiOpen);
    end;

    { Handle automatic closing of child popups }
    if vsModal in FState then begin
      { If the view is a toolbar, or if the new selection didn't come from
        the mouse, close child popups immediately }
      if FIsToolbar or not ViaMouse then begin
        { Always stop any close timer because CloseChildPopups may not be
          called below }
        StopTimer(tiClose);
        if Value <> FOpenViewer then
          { ^ But don't close if selection is returning to the open item.
            Needed for the "FParentView.Selected := FParentView.FOpenViewer"
            line above to work. }
          CloseChildPopups;
      end
      else begin
        { Otherwise, delay-close any child popup }
        if Assigned(FOpenViewerView) and not(tiClose in FActiveTimers) then
          StartTimer(tiClose, GetMenuShowDelay);
      end;
    end;

    CancelCapture;
    if Assigned(OldSelected) then
      OldSelected.Leaving;
    FSelected := Value;
    FSelectedViaMouse := ViaMouse;
  end;

  NewMouseOverSelected := False;
  if Assigned(Value) and Assigned(FWindow) then begin
    P := SmallPointToPoint(TSmallPoint(GetMessagePos()));
    if FindDragTarget(P, True) = FWindow then begin
      P := FWindow.ScreenToClient(P);
      NewMouseOverSelected := (ViewerFromPoint(P) = Value);
      if NewMouseOverSelected and FCapture and
         not Value.IsPtInButtonPart(P.X - Value.BoundsRect.Left,
         P.Y - Value.BoundsRect.Top) then
        NewMouseOverSelected := False;
    end;
  end;

  if Value <> OldSelected then begin
    FMouseOverSelected := NewMouseOverSelected;
    if Assigned(OldSelected) and (tbisRedrawOnSelChange in OldSelected.Item.ItemStyle) then
      Invalidate(OldSelected);
    if Assigned(Value) then begin
      if tbisRedrawOnSelChange in Value.Item.ItemStyle then
        Invalidate(Value);
      Value.Entering(OldSelected);
    end;
    NotifyFocusEvent;

    { Handle automatic opening of a child popup }
    if vsModal in FState then begin
      { If the view is a toolbar, immediately open any child popup }
      if FIsToolbar then begin
        if Assigned(Value) then begin
          if ViaMouse and Assigned(FParentView) then begin
            { On chevron popups, always drop down menus when mouse passes
              over them, like Office 2000 }
            Include(FState, vsDropDownMenus);
          end;
          if (vsDropDownMenus in FState) and
             (ViaMouse or not(tbisNoAutoOpen in Value.Item.ItemStyle)) then
            OpenChildPopup(not ViaMouse);
        end;
      end
      else begin
        { Otherwise, delay-open any child popup if the selection came from
          the mouse }
        StopTimer(tiOpen);
        if ViaMouse and Assigned(Value) and (tbisSubmenu in Value.Item.ItemStyle) then
          StartTimer(tiOpen, GetMenuShowDelay);
      end;
    end;
  end
  else if FMouseOverSelected <> NewMouseOverSelected then begin
    FMouseOverSelected := NewMouseOverSelected;
    if Assigned(Value) and FCapture and (tbisRedrawOnMouseOverChange in Value.Item.ItemStyle) then
      Invalidate(Value);
  end;
end;

procedure TTBView.UpdateSelection(const P: PPoint; const AllowNewSelection: Boolean);
{ Called in response to a mouse movement, this method updates the current
  selection, updates the vsMouseInWindow view state, and enables/disables
  scroll timers. }

  function IsPtInScrollArrow(ADownArrow: Boolean): Boolean;
  var
    P2: TPoint;
    R: TRect;
  begin
    Result := False;
    if (vsModal in FState) and (vsMouseInWindow in FState) and
       Assigned(P) then begin
      P2 := FWindow.ScreenToClient(P^);
      R := FWindow.ClientRect;
      if PtInRect(R, P2) then begin
        if ADownArrow then
          Result := FShowDownArrow and (P2.Y >= R.Bottom - tbMenuScrollArrowHeight)
        else
          Result := FShowUpArrow and (P2.Y < tbMenuScrollArrowHeight);
      end;
    end;
  end;

var
  NewSelected, ViewerAtPoint: TTBItemViewer;
  P2: TPoint;
  MouseWasInWindow: Boolean;
begin
  ValidatePositions;

  { If modal, default to keeping the existing selection }
  if vsModal in FState then
    NewSelected := FSelected
  else
    NewSelected := nil;

  { Is the mouse inside the window? }
  MouseWasInWindow := vsMouseInWindow in FState;
  if Assigned(P) and Assigned(FWindow) and (FindDragTarget(P^, True) = FWindow) then begin
    { If we're a popup window and the mouse is inside, default to no selection }
    if FIsPopup then
      NewSelected := nil;
    Include(FState, vsMouseInWindow);
    if AllowNewSelection or Assigned(FSelected) then begin
      P2 := FWindow.ScreenToClient(P^);
      ViewerAtPoint := ViewerFromPoint(P2);
      if Assigned(ViewerAtPoint) then
        NewSelected := ViewerAtPoint;
    end;
  end
  else
    Exclude(FState, vsMouseInWindow);

  { If FCapture is True, don't allow the selection to change }
  if FCapture and (NewSelected <> FSelected) then
    NewSelected := FSelected;

  { If we're a popup window and there is a selection... }
  if FIsPopup and Assigned(NewSelected) then begin
    { If the mouse just moved out of the window and no submenu was open,
      remove the highlight }
    if not FCapture and MouseWasInWindow and not(vsMouseInWindow in FState) and
       (not Assigned(FOpenViewerView) or not(tbisSubmenu in NewSelected.Item.ItemStyle)) then
      NewSelected := nil;
  end;

  { Now we set the new Selected value }
  Select(NewSelected, True);

  { Update scroll arrow timers }
  if IsPtInScrollArrow(False) then begin
    StopTimer(tiScrollDown);
    if not(tiScrollUp in FActiveTimers) then
      StartTimer(tiScrollUp, 100);
  end
  else if IsPtInScrollArrow(True) then begin
    StopTimer(tiScrollUp);
    if not(tiScrollDown in FActiveTimers) then
      StartTimer(tiScrollDown, 100);
  end
  else begin
    StopTimer(tiScrollUp);
    StopTimer(tiScrollDown);
  end;
end;

procedure TTBView.RecreateAllViewers;
var
  Item: TTBCustomItem;
  I: Integer;
begin
  { Since the FViewers list is being rebuilt, FOpenViewer and FSelected
    will no longer be valid, so ensure they're set to nil. }
  CloseChildPopups;
  Selected := nil;

  InvalidatePositions;

  FreeAndNil(FPriorityList);
  FreeViewers;
  FInternalViewersAtFront := 0;
  FInternalViewersAtEnd := 0;

  { MDI system menu item }
  Item := GetMDISystemMenuItem;
  if Assigned(Item) then
    Inc(FInternalViewersAtFront, InsertItemViewers(FViewerCount, Item, 0,
      False, False));

  { Items }
  if Assigned(FCurParentItem) then begin
    for I := 0 to FCurParentItem.Count-1 do
      InsertItemViewers(FViewerCount, FCurParentItem.FItems[I].Item, 0,
        True, False);
  end;

  { MDI buttons item }
  Item := GetMDIButtonsItem;
  if Assigned(Item) then begin
    for I := 0 to Item.Count-1 do
      Inc(FInternalViewersAtEnd, InsertItemViewers(FViewerCount,
        Item.FItems[I].Item, 0, False, False));
  end;

  { Chevron item }
  Item := GetChevronItem;
  if Assigned(Item) then
    Inc(FInternalViewersAtEnd, InsertItemViewers(FViewerCount, Item, 0,
      False, False));
end;

function TTBView.CalculatePositions(const CanMoveControls: Boolean;
  const AOrientation: TTBViewOrientation;
  AWrapOffset, AChevronOffset, AChevronSize: Integer;
  var ABaseSize, TotalSize: TPoint;
  var AWrappedLines: Integer): Boolean;
{ Returns True if the positions have changed }
type
  PTempPosition = ^TTempPosition;
  TTempPosition = record
    BoundsRect: TRect;
    Show, OffEdge, LineSep, Clipped, SameWidth: Boolean;
  end;
  PTempPositionArray = ^TTempPositionArray;
  TTempPositionArray = array[0..$7FFFFFFF div SizeOf(TTempPosition)-1] of TTempPosition;
var
  DC: HDC;
  LeftX, TopY, CurX, CurY, I: Integer;
  NewPositions: PTempPositionArray;
  GroupSplit, DidWrap: Boolean;
  LineStart, HighestHeightOnLine, HighestWidthOnLine: Integer;

  function GetSizeOfGroup(const StartingIndex: Integer): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := StartingIndex to FViewerCount-1 do begin
      if not NewPositions[I].Show then
        Continue;
      if tbisSeparator in FViewers[I].Item.ItemStyle then
        Break;
      with NewPositions[I] do begin
        if AOrientation <> tbvoVertical then
          Inc(Result, BoundsRect.Right)
        else
          Inc(Result, BoundsRect.Bottom);
      end;
    end;
  end;

  procedure Mirror;
  { Reverses the horizontal ordering (i.e. first item becomes last) }
  var
    I, NewRight: Integer;
  begin
    for I := 0 to FViewerCount-1 do
      with NewPositions[I] do
        if Show then begin
          NewRight := TotalSize.X - BoundsRect.Left;
          BoundsRect.Left := TotalSize.X - BoundsRect.Right;
          BoundsRect.Right := NewRight;
        end;
  end;

  procedure HandleMaxHeight;
  { Decreases, if necessary, the height of the view to FMaxHeight, and adjusts
    the visibility of the scroll arrows }
  var
    MaxOffset, I, MaxTop, MaxBottom: Integer;
  begin
    FShowUpArrow := False;
    FShowDownArrow := False;
    if (FMaxHeight > 0) and (TotalSize.Y > FMaxHeight) then begin
      MaxOffset := TotalSize.Y - FMaxHeight;
      if FScrollOffset > MaxOffset then
        FScrollOffset := MaxOffset;
      if FScrollOffset < 0 then
        FScrollOffset := 0;
      FShowUpArrow := (FScrollOffset > 0);
      FShowDownArrow := (FScrollOffset < MaxOffset);
      MaxTop := 0;
      if FShowUpArrow then
        MaxTop := tbMenuScrollArrowHeight;
      MaxBottom := FMaxHeight;
      if FShowDownArrow then
        Dec(MaxBottom, tbMenuScrollArrowHeight);
      for I := 0 to FViewerCount-1 do begin
        if not IsRectEmpty(NewPositions[I].BoundsRect) then begin
          OffsetRect(NewPositions[I].BoundsRect, 0, -FScrollOffset);
          if NewPositions[I].Show and
             ((NewPositions[I].BoundsRect.Top < MaxTop) or
              (NewPositions[I].BoundsRect.Bottom > MaxBottom)) then begin
            NewPositions[I].Show := False;
            NewPositions[I].Clipped := True;
          end;
        end;
      end;
      TotalSize.Y := FMaxHeight;
    end
    else
      FScrollOffset := 0;
  end;

  procedure FinalizeLine(const LineEnd: Integer; const LastLine: Boolean);
  var
    I, RightAlignStart: Integer;
    Item: TTBCustomItem;
    IsButton: Boolean;
    Pos: PTempPosition;
    Z: Integer;
  begin
    if LineStart <> -1 then begin
      if DidWrap and (FChevronParentView = nil) then begin
        { When wrapping on a docked toolbar, extend TotalSize.X/Y to
          AWrapOffset so that the toolbar always fills the whole row }
        if (AOrientation = tbvoHorizontal) and (TotalSize.X < AWrapOffset) then
          TotalSize.X := AWrapOffset
        else if (AOrientation = tbvoVertical) and (TotalSize.Y < AWrapOffset) then
          TotalSize.Y := AWrapOffset;
      end;
      RightAlignStart := -1;
      for I := LineStart to LineEnd do begin
        Pos := @NewPositions[I];
        if not Pos.Show then
          Continue;
        Item := FViewers[I].Item;
        if (RightAlignStart < 0) and (tbisRightAlign in Item.ItemStyle) then
          RightAlignStart := I;
        IsButton := FIsToolbar or (tboToolbarSize in Item.FEffectiveOptions);
        if FIsToolbar then begin
          if LastLine and not DidWrap and (AOrientation <> tbvoFloating) then begin
            { In case the toolbar is docked next to a taller/wider toolbar... }
            HighestWidthOnLine := TotalSize.X;
            HighestHeightOnLine := TotalSize.Y;
          end;
          { Make separators on toolbars as tall/wide as the tallest/widest item }
          if [tbisSeparator, tbisStretch] * Item.ItemStyle <> [] then begin
            if AOrientation <> tbvoVertical then
              Pos.BoundsRect.Bottom := Pos.BoundsRect.Top + HighestHeightOnLine
            else
              Pos.BoundsRect.Right := Pos.BoundsRect.Left + HighestWidthOnLine;
          end
          else begin
            { Center the item }
            if AOrientation <> tbvoVertical then begin
              Z := (HighestHeightOnLine - (Pos.BoundsRect.Bottom - Pos.BoundsRect.Top)) div 2;
              Inc(Pos.BoundsRect.Top, Z);
              Inc(Pos.BoundsRect.Bottom, Z);
            end
            else begin
              Z := (HighestWidthOnLine - (Pos.BoundsRect.Right - Pos.BoundsRect.Left)) div 2;
              Inc(Pos.BoundsRect.Left, Z);
              Inc(Pos.BoundsRect.Right, Z);
            end;
          end;
        end
        else begin
          { Make items in a menu as wide as the widest item }
          if not IsButton then begin
            with Pos.BoundsRect do Right := Left + HighestWidthOnLine;
          end;
        end;
      end;
      if RightAlignStart >= 0 then begin
        Z := 0;
        for I := LineEnd downto RightAlignStart do begin
          Pos := @NewPositions[I];
          if not Pos.Show then
            Continue;
          if AOrientation <> tbvoVertical then
            Z := Min(AWrapOffset, TotalSize.X) - Pos.BoundsRect.Right
          else
            Z := Min(AWrapOffset, TotalSize.Y) - Pos.BoundsRect.Bottom;
          Break;
        end;
        if Z > 0 then begin
          for I := RightAlignStart to LineEnd do begin
            Pos := @NewPositions[I];
            if not Pos.Show then
              Continue;
            if AOrientation <> tbvoVertical then begin
              Inc(Pos.BoundsRect.Left, Z);
              Inc(Pos.BoundsRect.Right, Z);
            end
            else begin
              Inc(Pos.BoundsRect.Top, Z);
              Inc(Pos.BoundsRect.Bottom, Z);
            end;
          end;
        end;
      end;
    end;
    LineStart := -1;
    HighestHeightOnLine := 0;
    HighestWidthOnLine := 0;
  end;

  procedure PositionItem(const CurIndex: Integer; var Pos: TTempPosition);
  var
    O, X, Y: Integer;
    IsLineSep, Vert: Boolean;
  begin
    if LineStart = -1 then begin
      LineStart := CurIndex;
      HighestHeightOnLine := 0;
      HighestWidthOnLine := 0;
    end;
    IsLineSep := False;
    Vert := (AOrientation = tbvoVertical);
    if not Vert then
      O := CurX
    else
      O := CurY;
    if (AWrapOffset > 0) and (O > 0) then begin
      if not Vert then
        Inc(O, Pos.BoundsRect.Right)
      else
        Inc(O, Pos.BoundsRect.Bottom);
      if (tbisSeparator in FViewers[CurIndex].Item.ItemStyle) and
         ((GroupSplit and not(tbisNoLineBreak in FViewers[CurIndex].Item.ItemStyle))
          or (O + GetSizeOfGroup(CurIndex+1) > AWrapOffset)) then begin
        DidWrap := True;
        Inc(AWrappedLines);
        if not Vert then begin
          CurX := 0;
          Inc(CurY, HighestHeightOnLine);
        end
        else begin
          CurY := 0;
          Inc(CurX, HighestWidthOnLine);
        end;
        FinalizeLine(CurIndex-1, False);
        LineStart := CurIndex+1;
        if not Vert then begin
          Pos.BoundsRect.Right := 0;
          Pos.BoundsRect.Bottom := tbLineSpacing;
        end
        else begin
          Pos.BoundsRect.Right := tbLineSpacing;
          Pos.BoundsRect.Bottom := 0;
        end;
        Pos.LineSep := True;
        IsLineSep := True;
      end
      else if O > AWrapOffset then begin
        { proceed to next row }
        DidWrap := True;
        Inc(AWrappedLines);
        if not Vert then begin
          CurX := LeftX;
          Inc(CurY, HighestHeightOnLine);
        end
        else begin
          CurY := TopY;
          Inc(CurX, HighestWidthOnLine);
        end;
        GroupSplit := True;
        FinalizeLine(CurIndex-1, False);
        LineStart := CurIndex;
      end;
    end;
    if Pos.BoundsRect.Bottom > HighestHeightOnLine then
      HighestHeightOnLine := Pos.BoundsRect.Bottom;
    if Pos.BoundsRect.Right > HighestWidthOnLine then
      HighestWidthOnLine := Pos.BoundsRect.Right;
    X := CurX;
    Y := CurY;
    if X < 0 then X := 0;
    if Y < 0 then Y := 0;
    OffsetRect(Pos.BoundsRect, X, Y);
    if IsLineSep then begin
      if not Vert then begin
        CurX := LeftX;
        Inc(CurY, tbLineSpacing);
      end
      else begin
        CurY := TopY;
        Inc(CurX, tbLineSpacing);
      end;
      GroupSplit := False;
    end;
  end;

var
  SaveOrientation: TTBViewOrientation;
  ChevronItem: TTBCustomItem;
  CalcCanvas: TCanvas;
  LastWasSep, LastWasButton, IsButton, IsControl: Boolean;
  Item: TTBCustomItem;
  Ctl: TControl;
  ChangedBold: Boolean;
  HighestSameWidthViewerWidth, Total, J, TotalVisibleItems: Integer;
  IsFirst: Boolean;
  Viewer: TTBItemViewer;
  UseChevron, NonControlsOffEdge, TempViewerCreated: Boolean;
  Margins: TRect;
label 1;
begin
  SaveOrientation := FOrientation;
  AWrappedLines := 1;
  ChevronItem := GetChevronItem;
  NewPositions := nil;
  DC := 0;
  CalcCanvas := nil;
  try
    FOrientation := AOrientation;

    CalcCanvas := TCanvas.Create;
    DC := GetDC(0);
    CalcCanvas.Handle := DC;
    CalcCanvas.Font.Assign(GetFont);

    NewPositions := AllocMem(FViewerCount * SizeOf(TTempPosition));

    { Figure out which items should be shown }
    LastWasSep := True;  { set to True initially so it won't show leading seps }
    for I := 0 to FViewerCount-1 do begin
      Item := FViewers[I].Item;
      IsControl := Item is TTBControlItem;
      with NewPositions[I] do begin
        { Show is initially False since AllocMem initializes to zero }
        if Item = ChevronItem then
          Continue;
        if Assigned(FChevronParentView) then begin
          if IsControl then
            Continue;
          FChevronParentView.ValidatePositions;
          J := I + FChevronParentView.FInternalViewersAtFront;
          if J < FChevronParentView.FViewerCount then
            { range check just in case }
            Viewer := FChevronParentView.FViewers[J]
          else
            Viewer := nil;
          if (Viewer = nil) or (not Viewer.OffEdge and not(tbisSeparator in Item.ItemStyle)) then
            Continue;
        end;
        if not IsControl then begin
          if not(tbisEmbeddedGroup in Item.ItemStyle) or FCustomizing then begin
            Show := Item.Visible;
            { Don't display two consecutive separators }
            if Show then begin
              if (tbisSeparator in Item.ItemStyle) and LastWasSep then
                Show := False;
              LastWasSep := tbisSeparator in Item.ItemStyle;
            end;
          end;
        end
        else begin
          { Controls can only be rendered on a single Parent, so only
            include the control if its parent is currently equal to
            FWindow }
          Ctl := TTBControlItem(Item).FControl;
          if Assigned(Ctl) and Assigned(FWindow) and (Ctl.Parent = FWindow) and
             (Ctl.Visible or (csDesigning in Ctl.ComponentState)) then begin
            Show := True;
            LastWasSep := False;
          end;
        end;
      end;
    end;

    { Hide any trailing separators, so that they aren't included in the
      base size }
    for I := FViewerCount-1 downto 0 do begin
      with NewPositions[I] do
        if Show then begin
          if not(tbisSeparator in FViewers[I].Item.ItemStyle) then
            Break;
          Show := False;
        end;
    end;

    { Calculate sizes of all the items }
    HighestSameWidthViewerWidth := 0;
    for I := 0 to FViewerCount-1 do begin
      Item := FViewers[I].Item;
      IsControl := Item is TTBControlItem;
      with NewPositions[I] do begin
        { BoundsRect is currently empty since AllocMem initializes to zero }
        if not Show then
          Continue;
        if not IsControl then begin
          ChangedBold := False;
          if tboDefault in Item.EffectiveOptions then
            with CalcCanvas.Font do
              if not(fsBold in Style) then begin
                ChangedBold := True;
                Style := Style + [fsBold];
              end;
          Viewer := FViewers[I];
          TempViewerCreated := False;
          if Item.NeedToRecreateViewer(Viewer) then begin
            if CanMoveControls then begin
              RecreateItemViewer(I);
              Viewer := FViewers[I];
            end
            else begin
              Viewer := Item.GetItemViewerClass(Self).Create(Self, Item, 0);
              TempViewerCreated := True;
            end;
          end;
          try
            Viewer.CalcSize(CalcCanvas, BoundsRect.Right, BoundsRect.Bottom);
            if Viewer.UsesSameWidth then begin
              SameWidth := True;
              if (BoundsRect.Right > HighestSameWidthViewerWidth) then
                HighestSameWidthViewerWidth := BoundsRect.Right;
            end;
          finally
            if TempViewerCreated then
              Viewer.Free;
          end;
          if ChangedBold then
            with CalcCanvas.Font do
              Style := Style - [fsBold];
        end
        else begin
          Ctl := TTBControlItem(Item).FControl;
          BoundsRect.Right := Ctl.Width;
          BoundsRect.Bottom := Ctl.Height;
        end;
      end;
    end;

    { Increase widths of SameWidth items if necessary. Also calculate
      ABaseSize.X (or Y). }
    ABaseSize.X := 0;
    ABaseSize.Y := 0;
    for I := 0 to FViewerCount-1 do begin
      with NewPositions[I] do begin
        if SameWidth and (BoundsRect.Right < HighestSameWidthViewerWidth) then
          BoundsRect.Right := HighestSameWidthViewerWidth;
        if AOrientation <> tbvoVertical then
          Inc(ABaseSize.X, BoundsRect.Right)
        else
          Inc(ABaseSize.Y, BoundsRect.Bottom);
      end;
    end;

    { Hide partially visible items, mark them as 'OffEdge' }
    if AOrientation <> tbvoVertical then
      Total := ABaseSize.X
    else
      Total := ABaseSize.Y;
    NonControlsOffEdge := False;
    UseChevron := Assigned(ChevronItem) and (AChevronOffset > 0) and
      (Total > AChevronOffset);
    if UseChevron then begin
      Dec(AChevronOffset, AChevronSize);
      while Total > AChevronOffset do begin
        { Count number of items. Stop loop if <= 1 }
        TotalVisibleItems := 0;
        for I := FViewerCount-1 downto 0 do begin
          if NewPositions[I].Show and not(tbisSeparator in FViewers[I].Item.ItemStyle) then
            Inc(TotalVisibleItems);
        end;
        if TotalVisibleItems <= 1 then
          Break;
        { Hide any trailing separators }
        for I := FViewerCount-1 downto 0 do begin
          with NewPositions[I] do
            if Show then begin
              if not(tbisSeparator in FViewers[I].Item.ItemStyle) then
                Break;
              Show := False;
              if AOrientation <> tbvoVertical then
                Dec(Total, BoundsRect.Right)
              else
                Dec(Total, BoundsRect.Bottom);
              goto 1;
            end;
        end;
        { Find an item to hide }
        if Assigned(FPriorityList) then
          I := FPriorityList.Count-1
        else
          I := FViewerCount-1;
        while I >= 0 do begin
          if Assigned(FPriorityList) then begin
            Viewer := FPriorityList[I];
            J := Viewer.Index;
          end
          else begin
            Viewer := FViewers[I];
            J := I;
          end;
          if NewPositions[J].Show and not(tbisSeparator in Viewer.Item.ItemStyle) then begin
            NewPositions[J].Show := False;
            NewPositions[J].OffEdge := True;
            if AOrientation <> tbvoVertical then
              Dec(Total, NewPositions[J].BoundsRect.Right)
            else
              Dec(Total, NewPositions[J].BoundsRect.Bottom);
            if not NonControlsOffEdge and not(Viewer.Item is TTBControlItem) then
              NonControlsOffEdge := True;
            goto 1;
          end;
          Dec(I);
        end;
        Break;  { prevent endless loop }
        1:
        { Don't show two consecutive separators }
        LastWasSep := True;  { set to True initially so it won't show leading seps }
        for J := 0 to FViewerCount-1 do begin
          Item := FViewers[J].Item;
          with NewPositions[J] do begin
            if Show then begin
              if (tbisSeparator in Item.ItemStyle) and LastWasSep then begin
                Show := False;
                if AOrientation <> tbvoVertical then
                  Dec(Total, BoundsRect.Right)
                else
                  Dec(Total, BoundsRect.Bottom);
              end;
              LastWasSep := tbisSeparator in Item.ItemStyle;
            end;
          end;
        end;
      end;
    end;

    { Hide any trailing separators after items were hidden }
    for I := FViewerCount-1 downto 0 do begin
      with NewPositions[I] do
        if Show then begin
          if not(tbisSeparator in FViewers[I].Item.ItemStyle) then
            Break;
          Show := False;
        end;
    end;

    { Set the ABaseSize.Y (or X) *after* items were hidden }
    for I := 0 to FViewerCount-1 do begin
      with NewPositions[I] do
        if Show then begin
          if AOrientation <> tbvoVertical then begin
            if BoundsRect.Bottom > ABaseSize.Y then
              ABaseSize.Y := BoundsRect.Bottom;
          end
          else begin
            if BoundsRect.Right > ABaseSize.X then
              ABaseSize.X := BoundsRect.Right;
          end;
        end;
    end;

    { On menus, set all non-separator items to be as tall as the tallest item }
    {if not FIsToolbar then begin
      J := 0;
      for I := 0 to FViewerCount-1 do begin
        Item := FViewers[I].Item;
        with NewPositions[I] do
          if Show and not(tbisSeparator in Item.ItemStyle) and
             not(tboToolbarSize in Item.FEffectiveOptions) and
             (BoundsRect.Bottom - BoundsRect.Top > J) then
            J := BoundsRect.Bottom - BoundsRect.Top;
      end;
      for I := 0 to FViewerCount-1 do begin
        Item := FViewers[I].Item;
        with NewPositions[I] do
          if Show and not(tbisSeparator in Item.ItemStyle) and
             not(tboToolbarSize in Item.FEffectiveOptions) then
            BoundsRect.Bottom := BoundsRect.Top + J;
      end;
    end;}

    { Calculate the position of the items }
    GetMargins(AOrientation, Margins);
    LeftX := Margins.Left;
    TopY := Margins.Top;
    if AWrapOffset > 0 then begin
      Dec(AWrapOffset, Margins.Right);
      if AWrapOffset < 1 then AWrapOffset := 1;
    end;
    CurX := LeftX;
    CurY := TopY;
    GroupSplit := False;
    DidWrap := False;
    LastWasButton := FIsToolbar;
    LineStart := -1;
    for I := 0 to FViewerCount-1 do begin
      Item := FViewers[I].Item;
      with NewPositions[I] do begin
        if not Show then
          Continue;
        IsButton := FIsToolbar or (tboToolbarSize in Item.FEffectiveOptions);
        if LastWasButton and not IsButton then begin
          { On a menu, if last item was a button and the current item isn't,
            proceed to next row }
          CurX := LeftX;
          CurY := TotalSize.Y;
        end;
        LastWasButton := IsButton;
        PositionItem(I, NewPositions[I]);
        if IsButton and (AOrientation <> tbvoVertical) then
          Inc(CurX, BoundsRect.Right - BoundsRect.Left)
        else
          Inc(CurY, BoundsRect.Bottom - BoundsRect.Top);
        if BoundsRect.Right > TotalSize.X then
          TotalSize.X := BoundsRect.Right;
        if BoundsRect.Bottom > TotalSize.Y then
          TotalSize.Y := BoundsRect.Bottom;
      end;
    end;
    if FViewerCount <> 0 then
      FinalizeLine(FViewerCount-1, True);
    Inc(TotalSize.X, Margins.Right);
    Inc(TotalSize.Y, Margins.Bottom);
    if AOrientation = tbvoVertical then
      Mirror;
    HandleMaxHeight;
    if CanMoveControls then begin
      for I := 0 to FViewerCount-1 do begin
        Item := FViewers[I].Item;
        if Item is TTBControlItem then begin
          if NewPositions[I].Show then begin
            Ctl := TTBControlItem(Item).FControl;
            if not EqualRect(NewPositions[I].BoundsRect, Ctl.BoundsRect) then
              Ctl.BoundsRect := NewPositions[I].BoundsRect;
          end
          else if NewPositions[I].OffEdge or NewPositions[I].Clipped then begin
            { Simulate hiding of OddEdge controls by literally moving them
              off the edge. Do the same for Clipped controls. }
            Ctl := TTBControlItem(Item).FControl;
            Ctl.SetBounds(FWindow.ClientWidth, FWindow.ClientHeight,
              Ctl.Width, Ctl.Height);
          end;
        end;
      end;
    end;
    { Set size of line separators }
    if FIsToolbar then
      for I := 0 to FViewerCount-1 do begin
        Item := FViewers[I].Item;
        with NewPositions[I] do
          if Show and (tbisSeparator in Item.ItemStyle) and
             LineSep then begin
            if AOrientation <> tbvoVertical then
              BoundsRect.Right := TotalSize.X
            else
              BoundsRect.Bottom := TotalSize.Y;
          end;
      end;

    { Position the chevron item }
    if UseChevron then begin
      if CanMoveControls then
        ChevronItem.Enabled := NonControlsOffEdge;
      NewPositions[FViewerCount-1].Show := True;
      I := AChevronOffset;
      if AOrientation <> tbvoVertical then begin
        if I < TotalSize.X then
          I := TotalSize.X;
        NewPositions[FViewerCount-1].BoundsRect := Bounds(I, 0,
          AChevronSize, TotalSize.Y);
      end
      else begin
        if I < TotalSize.Y then
          I := TotalSize.Y;
        NewPositions[FViewerCount-1].BoundsRect := Bounds(0, I,
          TotalSize.X, AChevronSize);
      end;
    end;

    { Commit changes }
    Result := False;
    if CanMoveControls then begin
      for I := 0 to FViewerCount-1 do begin
        if not Result and
           (not EqualRect(FViewers[I].BoundsRect, NewPositions[I].BoundsRect) or
            (FViewers[I].Show <> NewPositions[I].Show) or
            (tbisLineSep in FViewers[I].State <> NewPositions[I].LineSep)) then
          Result := True;
        FViewers[I].FBoundsRect := NewPositions[I].BoundsRect;
        FViewers[I].FShow := NewPositions[I].Show;
        FViewers[I].FOffEdge := NewPositions[I].OffEdge;
        FViewers[I].FClipped := NewPositions[I].Clipped;
        if NewPositions[I].LineSep then
          Include(FViewers[I].State, tbisLineSep)
        else
          Exclude(FViewers[I].State, tbisLineSep);
      end;
    end;
  finally
    FOrientation := SaveOrientation;
    if Assigned(CalcCanvas) then
      CalcCanvas.Handle := 0;
    if DC <> 0 then ReleaseDC(0, DC);
    CalcCanvas.Free;
    FreeMem(NewPositions);
  end;
  if (ABaseSize.X = 0) or (ABaseSize.Y = 0) then begin
    { If there are no visible items... }
    {}{scale this?}
    ABaseSize.X := 23;
    ABaseSize.Y := 22;
    if TotalSize.X < 23 then TotalSize.X := 23;
    if TotalSize.Y < 22 then TotalSize.Y := 22;
  end;
end;

procedure TTBView.DoUpdatePositions(var ASize: TPoint);
{ This is called by UpdatePositions }
var
  Bmp: TBitmap;
  CtlCanvas: TControlCanvas;
  WrappedLines: Integer;
begin
  { Don't call InvalidatePositions before CalculatePositions so that
    endless recursion doesn't happen if an item's CalcSize uses a method that
    calls ValidatePositions }
  if not CalculatePositions(True, FOrientation, FWrapOffset, FChevronOffset,
     FChevronSize, FBaseSize, ASize, WrappedLines) then begin
    { If the new positions are identical to the previous ones, continue using
      the previous ones, and don't redraw }
    FValidated := True;
    { Just because the positions are the same doesn't mean the size hasn't
      changed. (If a shrunken toolbar moves between docks, the positions of
      the non-OffEdge items may be the same on the new dock as on the old
      dock.) }
    AutoSize(ASize.X, ASize.Y);
  end
  else begin
    if not(csDesigning in ComponentState) then begin
      FValidated := True;
      { Need to call ValidateRect before AutoSize, otherwise Windows will
        erase the client area during a resize }
      if FWindow.HandleAllocated then
        ValidateRect(FWindow.Handle, nil);
      AutoSize(ASize.X, ASize.Y);
      if Assigned(FWindow) and FWindow.HandleAllocated and
         IsWindowVisible(FWindow.Handle) and
         (FWindow.ClientWidth > 0) and (FWindow.ClientHeight > 0) then begin
        CtlCanvas := nil;
        Bmp := TBitmap.Create;
        try
          CtlCanvas := TControlCanvas.Create;
          CtlCanvas.Control := FWindow;
          Bmp.Width := FWindow.ClientWidth;
          Bmp.Height := FWindow.ClientHeight;

          SendMessage(FWindow.Handle, WM_ERASEBKGND, WPARAM(Bmp.Canvas.Handle), 0);
          SendMessage(FWindow.Handle, WM_PAINT, WPARAM(Bmp.Canvas.Handle), 0);
          BitBlt(CtlCanvas.Handle, 0, 0, Bmp.Width, Bmp.Height,
            Bmp.Canvas.Handle, 0, 0, SRCCOPY);
          ValidateRect(FWindow.Handle, nil);
        finally
          CtlCanvas.Free;
          Bmp.Free;
        end;
      end;
    end
    else begin
      { Delphi's handling of canvases is different at design time -- child
        controls aren't clipped from a parent control's canvas, so the above
        offscreen rendering code doesn't work right at design-time }
      InvalidatePositions;
      FValidated := True;
      AutoSize(ASize.X, ASize.Y);
    end;
  end;
end;

function TTBView.UpdatePositions: TPoint;
{ Called whenever the size or orientation of a view changes. When items are
  added or removed from the view, InvalidatePositions must be called instead,
  otherwise the view may not be redrawn properly. }
begin
  Result.X := 0;
  Result.Y := 0;
  DoUpdatePositions(Result);
end;

procedure TTBView.AutoSize(AWidth, AHeight: Integer);
begin
end;

function TTBView.GetChevronItem: TTBCustomItem;
begin
  Result := nil;
end;

procedure TTBView.GetMargins(AOrientation: TTBViewOrientation;
  var Margins: TRect);
begin
  if AOrientation = tbvoFloating then begin
    Margins.Left := 4;
    Margins.Top := 2;
    Margins.Right := 4;
    Margins.Bottom := 1;
  end
  else begin
    Margins.Left := 0;
    Margins.Top := 0;
    Margins.Right := 0;
    Margins.Bottom := 0;
  end;
end;

function TTBView.GetMDIButtonsItem: TTBCustomItem;
begin
  Result := nil;
end;

function TTBView.GetMDISystemMenuItem: TTBCustomItem;
begin
  Result := nil;
end;

function TTBView.GetFont: TFont;
begin
  Result := GetToolbarFont(GetMonitorPixelsPerInch(GetMonitor));
  if not Assigned(Result) then
  begin
    { ToolbarFont is destroyed during unit finalization, but in rare cases
      this method may end up being called from ValidatePositions *after*
      unit finalization if Application.Run is never called; see the
      "EConvertError" newsgroup thread. We can't return nil because that would
      cause an exception in the calling function, so just return the window
      font. It's not the *right* font, but it shouldn't matter since the app
      is exiting anyway. }
    Result := TControlAccess(FWindow).Font;
  end;
end;

procedure TTBView.DrawItem(Viewer: TTBItemViewer; DrawTo: TCanvas;
  Offscreen: Boolean);
const
  COLOR_MENUHILIGHT = 29;
  clMenuHighlight = TColor(COLOR_MENUHILIGHT or $80000000);
var
  Bmp: TBitmap;
  DrawToDC, BmpDC: HDC;
  DrawCanvas: TCanvas;
  R1, R2, R3: TRect;
  IsOpen, IsSelected, IsPushed: Boolean;
  ToolbarStyle: Boolean;
  UseDisabledShadow: Boolean;
  SaveIndex, SaveIndex2: Integer;
  BkColor: TColor;
begin
  ValidatePositions;

  if tbisInvalidated in Viewer.State then begin
    Offscreen := True;
    Exclude(Viewer.State, tbisInvalidated);
  end;

  R1 := Viewer.BoundsRect;
  if not Viewer.Show or IsRectEmpty(R1) or (Viewer.Item is TTBControlItem) then
    Exit;
  R2 := R1;
  OffsetRect(R2, -R2.Left, -R2.Top);

  IsOpen := FOpenViewer = Viewer;
  IsSelected := (FSelected = Viewer);
  IsPushed := IsSelected and (IsOpen or (FMouseOverSelected and FCapture));
  ToolbarStyle := Viewer.IsToolbarStyle;

  DrawToDC := DrawTo.Handle;
  Bmp := nil;
  { Must deselect any currently selected handles before calling SaveDC, because
    if they are left selected and DeleteObject gets called on them after the
    SaveDC call, it will fail on Win9x/Me, and thus leak GDI resources. }
  DrawTo.Refresh;
  SaveIndex := SaveDC(DrawToDC);
  try
    IntersectClipRect(DrawToDC, R1.Left, R1.Top, R1.Right, R1.Bottom);
    GetClipBox(DrawToDC, R3);
    if IsRectEmpty(R3) then
      Exit;

    if not Offscreen then begin
      MoveWindowOrg(DrawToDC, R1.Left, R1.Top);
      { Tweak the brush origin so that the checked background drawn behind
        checked items always looks the same regardless of whether the item
        is positioned on an even or odd Left or Top coordinate. }
      SetBrushOrgEx(DrawToDC, R1.Left and 1, R1.Top and 1, nil);
      DrawCanvas := DrawTo;
    end
    else begin
      Bmp := TBitmap.Create;
      Bmp.Width := R2.Right;
      Bmp.Height := R2.Bottom;
      DrawCanvas := Bmp.Canvas;
      BmpDC := DrawCanvas.Handle;
      SaveIndex2 := SaveDC(BmpDC);
      SetWindowOrgEx(BmpDC, R1.Left, R1.Top, nil);
      FWindow.Perform(WM_ERASEBKGND, WPARAM(BmpDC), 0);
      RestoreDC(BmpDC, SaveIndex2);
    end;

    { Initialize brush }
    if not ToolbarStyle and IsSelected then begin
      {$IFNDEF TB2K_USE_STRICT_O2K_MENU_STYLE}
      if AreFlatMenusEnabled then
        { Windows XP uses a different fill color for selected menu items when
          flat menus are enabled }
        DrawCanvas.Brush.Color := clMenuHighlight
      else
      {$ENDIF}
        DrawCanvas.Brush.Color := clHighlight;
    end
    else
      DrawCanvas.Brush.Style := bsClear;

    { Initialize font }
    DrawCanvas.Font.Assign(GetFont);
    if Viewer.Item.Enabled then begin
      if not ToolbarStyle and IsSelected then
        DrawCanvas.Font.Color := clHighlightText
      else begin
        if ToolbarStyle then
          DrawCanvas.Font.Color := clBtnText
        else
          DrawCanvas.Font.Color := tbMenuTextColor;
      end;
      UseDisabledShadow := False;
    end
    else begin
      DrawCanvas.Font.Color := clGrayText;
      { Use the disabled shadow if either:
        1. The item is a toolbar-style item.
        2. The item is not selected, and the background color equals the
           button-face color.
        3. The gray-text color is the same as the background color.
           Note: Windows actually uses dithered text in this case. }
      BkColor := ColorToRGB(TControlAccess(FWindow).Color);
      UseDisabledShadow := ToolbarStyle or
        (not IsSelected and (BkColor = ColorToRGB(clBtnFace))) or
        (ColorToRGB(clGrayText) = BkColor);
    end;

    Viewer.Paint(DrawCanvas, R2, IsSelected, IsPushed, UseDisabledShadow);

    if Offscreen then
      BitBlt(DrawToDC, R1.Left, R1.Top, Bmp.Width, Bmp.Height, DrawCanvas.Handle,
        0, 0, SRCCOPY);
  finally
    DrawTo.Refresh;  { must do this before a RestoreDC }
    RestoreDC(DrawToDC, SaveIndex);
    Bmp.Free;
  end;
end;

procedure TTBView.DrawSubitems(ACanvas: TCanvas);
var
  I: Integer;
begin
  for I := 0 to FViewerCount-1 do begin
    if (vsDrawInOrder in FState) or (FViewers[I] <> FSelected) then
      DrawItem(FViewers[I], ACanvas, False);
  end;
  if not(vsDrawInOrder in FState) and Assigned(FSelected) then
    DrawItem(FSelected, ACanvas, False);

  Exclude(FState, vsDrawInOrder);
end;

procedure TTBView.Invalidate(AViewer: TTBItemViewer);
begin
  if not FValidated or not Assigned(FWindow) or not FWindow.HandleAllocated then
    Exit;
  if AViewer.Show and not IsRectEmpty(AViewer.BoundsRect) and
     not(AViewer.Item is TTBControlItem) then begin
    Include(AViewer.State, tbisInvalidated);
    InvalidateRect(FWindow.Handle, @AViewer.BoundsRect, False);
  end;
end;

procedure TTBView.SetAccelsVisibility(AShowAccels: Boolean);
var
  I: Integer;
  Viewer: TTBItemViewer;
begin
  { Always show accels when keyboard cues are enabled }
  AShowAccels := AShowAccels or not(vsUseHiddenAccels in FStyle) or
    AreKeyboardCuesEnabled;
  if AShowAccels <> (vsShowAccels in FState) then begin
    if AShowAccels then
      Include(FState, vsShowAccels)
    else
      Exclude(FState, vsShowAccels);
    if Assigned(FWindow) and FWindow.HandleAllocated and
       IsWindowVisible(FWindow.Handle) then
      { ^ the visibility check is just an optimization }
      for I := 0 to FViewerCount-1 do begin
        Viewer := FViewers[I];
        if Viewer.CaptionShown and
           (FindAccelChar(Viewer.GetCaptionText) <> #0) then
          Invalidate(Viewer);
      end;
  end;
end;

function TTBView.FirstSelectable: TTBItemViewer;
var
  FirstViewer: TTBItemViewer;
begin
  Result := NextSelectable(nil, True);
  if Assigned(Result) then begin
    FirstViewer := Result;
    while tbisDontSelectFirst in Result.Item.ItemStyle do begin
      Result := NextSelectable(Result, True);
      if Result = FirstViewer then
        { don't loop endlessly if all items have the tbisDontSelectFirst style }
        Break;
    end;
  end;
end;

function TTBView.NextSelectable(CurViewer: TTBItemViewer;
  GoForward: Boolean): TTBItemViewer;
var
  I, J: Integer;
begin
  ValidatePositions;
  Result := nil;
  if FViewerCount = 0 then Exit;
  J := -1;
  I := IndexOf(CurViewer);
  while True do begin
    if GoForward then begin
      Inc(I);
      if I >= FViewerCount then I := 0;
    end
    else begin
      Dec(I);
      if I < 0 then I := FViewerCount-1;
    end;
    if J = -1 then
      J := I
    else
      if I = J then
        Exit;
    if (FViewers[I].Show or FViewers[I].Clipped) and FViewers[I].Item.Visible and
       (tbisSelectable in FViewers[I].Item.ItemStyle) then
      Break;
  end;
  Result := FViewers[I];
end;

function TTBView.NextSelectableWithAccel(CurViewer: TTBItemViewer;
  Key: Char; RequirePrimaryAccel: Boolean; var IsOnlyItemWithAccel: Boolean): TTBItemViewer;

  function IsAccelItem(const Index: Integer;
    const Primary, EnabledItems: Boolean): Boolean;
  var
    S: String;
    LastAccel: Char;
    Viewer: TTBItemViewer;
    Item: TTBCustomItem;
  begin
    Result := False;
    Viewer := FViewers[Index];
    Item := Viewer.Item;
    if (Viewer.Show or Viewer.Clipped) and (tbisSelectable in Item.ItemStyle) and
       (Item.Enabled = EnabledItems) and
       Item.Visible and Viewer.CaptionShown then begin
      S := Viewer.GetCaptionText;
      if S <> '' then begin
        LastAccel := FindAccelChar(S);
        if Primary then begin
          if LastAccel <> #0 then
            Result := AnsiCompareText(LastAccel, Key) = 0;
        end
        else
          if (LastAccel = #0) and (Key <> ' ') then
            Result := AnsiCompareText(S[1], Key) = 0;
      end;
    end;
  end;

  function FindAccel(I: Integer;
    const Primary, EnabledItems: Boolean): Integer;
  var
    J: Integer;
  begin
    Result := -1;
    J := -1;
    while True do begin
      Inc(I);
      if I >= FViewerCount then I := 0;
      if J = -1 then
        J := I
      else
        if I = J then
          Break;
      if IsAccelItem(I, Primary, EnabledItems) then begin
        Result := I;
        Break;
      end;
    end;
  end;

var
  Start, I: Integer;
  Primary, EnabledItems: Boolean;
begin
  ValidatePositions;
  Result := nil;
  IsOnlyItemWithAccel := False;
  if FViewerCount = 0 then Exit;

  Start := IndexOf(CurViewer);
  for Primary := True downto False do
    if not RequirePrimaryAccel or Primary then
      for EnabledItems := True downto False do begin
        I := FindAccel(Start, Primary, EnabledItems);
        if I <> -1 then begin
          Result := FViewers[I];
          IsOnlyItemWithAccel := not EnabledItems or
            (FindAccel(I, Primary, EnabledItems) = I);
          Exit;
        end;
      end;
end;

procedure TTBView.EnterToolbarLoop(Options: TTBEnterToolbarLoopOptions);
var
  ModalHandler: TTBModalHandler;
  P: TPoint;
begin
  if vsModal in FState then Exit;
  ModalHandler := TTBModalHandler.Create(FWindow.Handle);
  try
    { remove all states except... }
    FState := FState * [vsShowAccels];
    try
      Include(FState, vsModal);
      { Must ensure that DoneAction is reset to tbdaNone *before* calling
        NotifyFocusEvent so that the IsModalEnding call it makes won't return
        True }
      FDoneActionData.DoneAction := tbdaNone;
      { Now that the vsModal state has been added, send an MSAA focus event }
      if Assigned(Selected) then
        NotifyFocusEvent;
      ModalHandler.Loop(Self, tbetMouseDown in Options,
        tbetExecuteSelected in Options, tbetFromMSAA in Options, False);
    finally
      { Remove vsModal state from the root view before any TTBView.Destroy
        methods get called (as a result of the CloseChildPopups call below),
        so that NotifyFocusEvent becomes a no-op }
      Exclude(FState, vsModal);
      StopAllTimers;
      CloseChildPopups;
      GetCursorPos(P);
      UpdateSelection(@P, True);
    end;
  finally
    ModalHandler.Free;
  end;
  SetAccelsVisibility(False);
  Selected := nil;
  // caused flicker: FWindow.Update;
  ProcessDoneAction(FDoneActionData, False);
end;

procedure TTBView.SetCustomizing(Value: Boolean);
begin
  if FCustomizing <> Value then begin
    FCustomizing := Value;
    RecreateAllViewers;
  end;
end;

procedure TTBView.BeginUpdate;
begin
  Inc(FUpdating);
end;

procedure TTBView.EndUpdate;
begin
  Dec(FUpdating);
  if FUpdating = 0 then
    TryValidatePositions;
end;

procedure TTBView.GetOffEdgeControlList(const List: TList);
var
  I: Integer;
  Item: TTBCustomItem;
begin
  for I := 0 to FViewerCount-1 do begin
    Item := FViewers[I].Item;
    if (Item is TTBControlItem) and FViewers[I].OffEdge and
       (TTBControlItem(Item).FControl is TWinControl) then
      List.Add(TTBControlItem(Item).FControl);
  end;
end;

procedure TTBView.SetCapture;
begin
  FCapture := True;
end;

procedure TTBView.CancelCapture;
begin
  if FCapture then begin
    FCapture := False;
    LastPos.X := Low(LastPos.X);
    if Assigned(FSelected) then
      FSelected.LosingCapture;
  end;
end;

procedure TTBView.KeyDown(var Key: Word; Shift: TShiftState);

  procedure SelNextItem(const ParentView: TTBView; const GoForward: Boolean);
  begin
    ParentView.Selected := ParentView.NextSelectable(ParentView.FSelected,
      GoForward);
    ParentView.ScrollSelectedIntoView;
  end;

  procedure HelpKey;
  var
    V: TTBView;
    ContextID: Integer;
    { MP }
    HelpKeyword: string;
  begin
    ContextID := 0;
    V := Self;
    while Assigned(V) do begin
      if Assigned(V.FSelected) then begin
        ContextID := V.FSelected.Item.HelpContext;
        if ContextID <> 0 then Break;
      end;
      V := V.FParentView;
    end;
    { MP }
    if ContextID <> 0 then
    begin
      EndModalWithHelp(ContextID);
      Exit;
    end;

    HelpKeyword := '';
    V := Self;
    while Assigned(V) do begin
      if Assigned(V.FSelected) then begin
        HelpKeyword := V.FSelected.Item.HelpKeyword;
        if HelpKeyword <> '' then Break;
      end;
      V := V.FParentView;
    end;
    if HelpKeyword <> '' then
      EndModalWithHelp(HelpKeyword);
    { /MP }
  end;

var
  ParentTBView: TTBView;
begin
  ParentTBView := GetParentToolbarView;
  case Key of
    VK_TAB: begin
        SelNextItem(Self, GetKeyState(VK_SHIFT) >= 0);
      end;
    VK_RETURN: begin
        ExecuteSelected(True);
      end;
    VK_MENU, VK_F10: begin
        EndModal;
      end;
    VK_ESCAPE: begin
        Key := 0;
        if FParentView = nil then
          EndModal
        else
          FParentView.CancelChildPopups;
      end;
    VK_LEFT, VK_RIGHT: begin
        if (Self = ParentTBView) and (Orientation = tbvoVertical) then
          OpenChildPopup(True)
        else if Key = VK_LEFT then begin
          if Assigned(ParentTBView) and (ParentTBView.Orientation <> tbvoVertical) then begin
            if (Self = ParentTBView) or
               (FParentView = ParentTBView) then
              SelNextItem(ParentTBView, False)
            else
              FParentView.CloseChildPopups;
          end
          else begin
            if Assigned(FParentView) then
              FParentView.CancelChildPopups;
          end;
        end
        else begin
          if ((Self = ParentTBView) or not OpenChildPopup(True)) and
             (Assigned(ParentTBView) and (ParentTBView.Orientation <> tbvoVertical)) then begin
            { If we're on ParentTBView, or if the selected item can't display
              a submenu, proceed to next item on ParentTBView }
            SelNextItem(ParentTBView, True);
          end;
        end;
      end;
    VK_UP, VK_DOWN: begin
        if (Self = ParentTBView) and (Orientation <> tbvoVertical) then
          OpenChildPopup(True)
        else
          SelNextItem(Self, Key = VK_DOWN);
      end;
    VK_HOME, VK_END: begin
        Selected := NextSelectable(nil, Key = VK_HOME);
        ScrollSelectedIntoView;
      end;
    VK_F1: HelpKey;
  else
    Exit;  { don't set Key to 0 for unprocessed keys }
  end;
  Key := 0;
end;

function TTBView.IsModalEnding: Boolean;
begin
  Result := (GetRootView.FDoneActionData.DoneAction <> tbdaNone);
end;

procedure TTBView.EndModal;
var
  RootView: TTBView;
begin
  RootView := GetRootView;
  RootView.FDoneActionData.DoneAction := tbdaCancel;
end;

procedure TTBView.EndModalWithClick(AViewer: TTBItemViewer);
var
  RootView: TTBView;
begin
  RootView := GetRootView;
  RootView.FDoneActionData.ClickItem := AViewer.Item;
  RootView.FDoneActionData.Sound := AViewer.FView.FIsPopup;
  RootView.FDoneActionData.DoneAction := tbdaClickItem;
end;

procedure TTBView.EndModalWithHelp(AContextID: Integer);
var
  RootView: TTBView;
begin
  RootView := GetRootView;
  RootView.FDoneActionData.ContextID := AContextID;
  RootView.FDoneActionData.DoneAction := tbdaHelpContext;
end;

{ MP }
procedure TTBView.EndModalWithHelp(HelpKeyword: string);
var
  RootView: TTBView;
begin
  RootView := GetRootView;
  RootView.FDoneActionData.HelpKeyword := ShortString(HelpKeyword);
  RootView.FDoneActionData.DoneAction := tbdaHelpKeyword;
end;
{ /MP }

procedure TTBView.EndModalWithSystemMenu(AWnd: HWND; AKey: Cardinal);
var
  RootView: TTBView;
begin
  RootView := GetRootView;
  RootView.FDoneActionData.Wnd := AWnd;
  RootView.FDoneActionData.Key := AKey;
  RootView.FDoneActionData.DoneAction := tbdaOpenSystemMenu;
end;

procedure TTBView.ExecuteSelected(AGivePriority: Boolean);
{ Normally called after an Enter or accelerator key press on the view, this
  method 'executes' or opens the selected item. It ends the modal loop, except
  when a submenu is opened. }
var
  Item: TTBCustomItem;
begin
  if Assigned(FSelected) and FSelected.Item.Enabled then begin
    Item := FSelected.Item;
    if (tbisCombo in Item.ItemStyle) or not OpenChildPopup(True) then begin
      if tbisSelectable in Item.ItemStyle then
        FSelected.Execute(AGivePriority)
      else
        EndModal;
    end
  end
  else
    EndModal;
  Exit; asm db 0,'Toolbar2000 (C) 1998-2005 Jordan Russell',0 end;
end;

procedure TTBView.Scroll(ADown: Boolean);
var
  CurPos, NewPos, I: Integer;
begin
  ValidatePositions;
  if ADown then begin
    NewPos := High(NewPos);
    CurPos := FMaxHeight - tbMenuScrollArrowHeight;
    for I := 0 to FViewerCount-1 do begin
      with FViewers[I] do
        if Clipped and not(tbisSeparator in Item.ItemStyle) and
          (BoundsRect.Bottom < NewPos) and (BoundsRect.Bottom > CurPos) then
          NewPos := BoundsRect.Bottom;
    end;
    if NewPos = High(NewPos) then
      Exit;
    Dec(NewPos, FMaxHeight - tbMenuScrollArrowHeight);
  end
  else begin
    NewPos := Low(NewPos);
    CurPos := tbMenuScrollArrowHeight;
    for I := 0 to FViewerCount-1 do begin
      with FViewers[I] do
        if Clipped and not(tbisSeparator in Item.ItemStyle) and
          (BoundsRect.Top > NewPos) and (BoundsRect.Top < CurPos) then
          NewPos := BoundsRect.Top;
    end;
    if NewPos = Low(NewPos) then
      Exit;
    Dec(NewPos, tbMenuScrollArrowHeight);
  end;
  Inc(FScrollOffset, NewPos);
  UpdatePositions;
end;

procedure TTBView.ScrollSelectedIntoView;
begin
  ValidatePositions;
  if (FSelected = nil) or not FSelected.Clipped then
    Exit;

  if FSelected.BoundsRect.Top < tbMenuScrollArrowHeight then begin
    Dec(FScrollOffset, tbMenuScrollArrowHeight - FSelected.BoundsRect.Top);
    UpdatePositions;
  end
  else if FSelected.BoundsRect.Bottom > FMaxHeight - tbMenuScrollArrowHeight then begin
    Dec(FScrollOffset, (FMaxHeight - tbMenuScrollArrowHeight) -
      FSelected.BoundsRect.Bottom);
    UpdatePositions;
  end;
end;

procedure TTBView.SetUsePriorityList(Value: Boolean);
begin
  if FUsePriorityList <> Value then begin
    FUsePriorityList := Value;
    RecreateAllViewers;
  end;
end;

function TTBView.GetCaptureWnd: HWND;
begin
  Result := GetRootView.FCaptureWnd;
end;

procedure TTBView.CancelMode;
var
  View: TTBView;
begin
  EndModal;

  { Hide all parent/child popup windows. Can't actually destroy them using
    CloseChildPopups because this method may be called while inside
    TTBEditItemViewer's message loop, and it could result in the active
    TTBEditItemViewer instance being destroyed (leading to an AV). }
  View := Self;
  while Assigned(View.FOpenViewerView) do
    View := View.FOpenViewerView;
  repeat
    View.StopAllTimers;
    if View.FWindow is TTBPopupWindow then
      View.FWindow.Visible := False;
    View := View.FParentView;
  until View = nil;

  { Note: This doesn't remove the selection from a top-level toolbar item.
    Unfortunately, we can't do 'Selected := nil' because it would destroy
    child popups and that must'nt happen for the reason stated above. }
end;

procedure TTBView.SetState(AState: TTBViewState);
begin
  FState := AState;
end;

function TTBView.GetMonitor: TMonitor;
begin
  if ParentView <> nil then
  begin
    Result := ParentView.GetMonitor;
  end
    else
  if not IsRectEmpty(FMonitorRect) then
  begin
    Result := Screen.MonitorFromRect(FMonitorRect);
  end
    else
  begin
    Result := GetMonitorFromControl(Window);
  end;
end;

{ TTBModalHandler }

const
  LSFW_LOCK = 1;
  LSFW_UNLOCK = 2;

var
  LockSetForegroundWindowInited: BOOL;
  LockSetForegroundWindow: function(uLockCode: UINT): BOOL; stdcall;

constructor TTBModalHandler.Create(AExistingWnd: HWND);
begin
  inherited Create;
  LastPos := SmallPointToPoint(TSmallPoint(GetMessagePos()));
  if AExistingWnd <> 0 then
    FWnd := AExistingWnd
  else begin
    FWnd := Classes.AllocateHWnd(WndProc);
    FCreatedWnd := True;
  end;
  { Like standard menus, don't allow other apps to steal the focus during
    our modal loop. This also prevents us from losing activation when
    "active window tracking" is enabled and the user moves the mouse over
    another application's window. }
  LockForegroundWindow;
  SetCapture(FWnd);
  SetCursor(LoadCursor(0, IDC_ARROW));
  NotifyWinEvent(EVENT_SYSTEM_MENUSTART, FWnd, OBJID_CLIENT, CHILDID_SELF);
  FInited := True;
end;

class procedure TTBModalHandler.DoLockForegroundWindow(LockCode: Cardinal);
begin
  if not LockSetForegroundWindowInited then begin
    LockSetForegroundWindow := GetProcAddress(GetModuleHandle(user32),
      'LockSetForegroundWindow');
    InterlockedExchange(Integer(LockSetForegroundWindowInited), Ord(True));
  end;
  { Should always, as supported since Windows 2000 }
  if Assigned(LockSetForegroundWindow) then
    LockSetForegroundWindow(LockCode);
end;

class procedure TTBModalHandler.LockForegroundWindow;
begin
  DoLockForegroundWindow(LSFW_LOCK);
end;

class procedure TTBModalHandler.UnlockForegroundWindow;
begin
  DoLockForegroundWindow(LSFW_UNLOCK);
end;

destructor TTBModalHandler.Destroy;
begin
  UnlockForegroundWindow;
  if FWnd <> 0 then begin
    if GetCapture = FWnd then
      ReleaseCapture;
    if FInited then
      NotifyWinEvent(EVENT_SYSTEM_MENUEND, FWnd, OBJID_CLIENT, CHILDID_SELF);
    if FCreatedWnd then
      Classes.DeallocateHWnd(FWnd);
  end;
  inherited;
end;

procedure TTBModalHandler.WndProc(var Msg: TMessage);
begin
  Msg.Result := DefWindowProc(FWnd, Msg.Msg, Msg.WParam, Msg.LParam);
  if (Msg.Msg = WM_CANCELMODE) and Assigned(FRootPopup) then begin
    try
      { We can receive a WM_CANCELMODE message during a modal loop if a dialog
        pops up. Respond by hiding menus to make it look like the modal loop
        has returned, even though it really hasn't yet.
        Note: Similar code in TTBCustomToolbar.WMCancelMode. }
      FRootPopup.View.CancelMode;
    except
      Application.HandleException(Self);
    end;
  end;
end;

procedure TTBModalHandler.Loop(const RootView: TTBView;
  const AMouseDown, AExecuteSelected, AFromMSAA, TrackRightButton: Boolean);
var
  OriginalActiveWindow: HWND;

  function GetActiveView: TTBView;
  begin
    Result := RootView;
    while Assigned(Result.FOpenViewerView) do
      Result := Result.FOpenViewerView;
  end;

  procedure UpdateAllSelections(const P: TPoint; const AllowNewSelection: Boolean);
  var
    View, CapView: TTBView;
  begin
    View := GetActiveView;

    CapView := View;
    while Assigned(CapView) and not CapView.FCapture do
      CapView := CapView.FParentView;

    while Assigned(View) do begin
      if (CapView = nil) or (View = CapView) then
        View.UpdateSelection(@P, AllowNewSelection);
      View := View.FParentView;
    end;
  end;

  function GetSelectedViewer(var AView: TTBView; var AViewer: TTBItemViewer): Boolean;
  { Returns True if AViewer <> nil. }
  var
    View: TTBView;
  begin
    AView := nil;
    AViewer := nil;
    { Look for a capture item first }
    View := RootView;
    repeat
      if View.FCapture then begin
        AView := View;
        AViewer := View.FSelected;
        Break;
      end;
      View := View.FOpenViewerView;
    until View = nil;
    if View = nil then begin
      View := RootView;
      repeat
        if Assigned(View.FSelected) and View.FMouseOverSelected then begin
          AView := View;
          AViewer := View.FSelected;
          Break;
        end;
        if vsMouseInWindow in View.FState then begin
          { ...there is no current selection, but the mouse is still in the
            window. This can happen if the mouse is over the non-client area
            of the toolbar or popup window, or in an area not containing an
            item. }
          AView := View;
          Break;
        end;
        View := View.FOpenViewerView;
      until View = nil;
    end;
    Result := Assigned(AViewer);
  end;

  function ContinueLoop: Boolean;
  begin
    { Don't continue if the mouse capture is lost, if a (modeless) top-level
      window is shown causing the active window to change, or if EndModal* was
      called. }
    Result := (GetCapture = FWnd) and (GetActiveWindow = OriginalActiveWindow)
      and not RootView.IsModalEnding;
  end;

  function SendKeyEvent(const View: TTBView; var Key: Word;
    const Shift: TShiftState): Boolean;
  begin
    Result := True;
    if Assigned(View.FSelected) then begin
      View.FSelected.KeyDown(Key, Shift);
      if RootView.IsModalEnding then
        Exit;
    end;
    if Key <> 0 then begin
      View.KeyDown(Key, Shift);
      if RootView.IsModalEnding then
        Exit;
    end;
    Result := False;
  end;

  procedure DoHintMouseMessage(const Ctl: TControl; const P: TPoint);
  var
    M: TWMMouseMove;
  begin
    M.Msg := WM_MOUSEMOVE;
    M.Keys := 0;
    M.Pos := PointToSmallPoint(P);
    Application.HintMouseMessage(Ctl, TMessage(M));
  end;

  procedure MouseMoved;
  var
    View: TTBView;
    Cursor: HCURSOR;
    Item: TTBCustomItem;
    P: TPoint;
    R: TRect;
  begin
    UpdateAllSelections(LastPos, True);
    View := GetActiveView;
    Cursor := 0;
    if Assigned(View.FSelected) and Assigned(View.FWindow) then begin
      Item := View.FSelected.Item;
      P := View.FWindow.ScreenToClient(LastPos);
      if ((vsAlwaysShowHints in View.FStyle) or
          (tboShowHint in Item.FEffectiveOptions)) and not View.FCapture then begin
        { Display popup hint for the item. Update is called
          first to minimize flicker caused by the hiding &
          showing of the hint window. }
        View.FWindow.Update;
        DoHintMouseMessage(View.FWindow, P);
      end
      else
        Application.CancelHint;
      R := View.FSelected.BoundsRect;
      Dec(P.X, R.Left);
      Dec(P.Y, R.Top);
      View.FSelected.GetCursor(P, Cursor);
    end
    else
      Application.CancelHint;
    if Cursor = 0 then
      Cursor := LoadCursor(0, IDC_ARROW);
    SetCursor(Cursor);
  end;

  procedure UpdateAppHint;
  var
    View: TTBView;
  begin
    View := RootView;
    while Assigned(View.FOpenViewerView) and Assigned(View.FOpenViewerView.FSelected) do
      View := View.FOpenViewerView;
    if Assigned(View.FSelected) then
      Application.Hint := GetLongHint(View.FSelected.Item.Hint)
    else
      Application.Hint := '';
  end;

  procedure HandleTimer(const View: TTBView; const ID: TTBViewTimerID);
  begin
    case ID of
      tiOpen: begin
          { Similar to standard menus, always close child popups, even if
            Selected = OpenViewer.
            Note: CloseChildPopups and OpenChildPopup will stop the tiClose
            and tiOpen timers respectively. }
          View.CloseChildPopups;
          View.OpenChildPopup(False);
        end;
      tiClose: begin
          { Note: CloseChildPopups stops the tiClose timer. }
          View.CloseChildPopups;
        end;
      tiScrollUp: begin
          if View.FShowUpArrow then
            View.Scroll(False)
          else
            View.StopTimer(tiScrollUp);
        end;
      tiScrollDown: begin
          if View.FShowDownArrow then
            View.Scroll(True)
          else
            View.StopTimer(tiScrollDown);
        end;
    end;
  end;

var
  MouseDownOnMenu: Boolean;
  Msg: TMsg;
  P: TPoint;
  Ctl: TControl;
  View: TTBView;
  IsOnlyItemWithAccel: Boolean;
  MouseIsDown: Boolean;
  Key: Word;
  Shift: TShiftState;
  Viewer: TTBItemViewer;
begin
  FillChar(RootView.FDoneActionData, SizeOf(RootView.FDoneActionData), 0);
  RootView.ValidatePositions;
  try
  try
    RootView.FCaptureWnd := FWnd;
    MouseDownOnMenu := False;
    if AMouseDown then begin
      P := RootView.FSelected.ScreenToClient(SmallPointToPoint(TSmallPoint(GetMessagePos())));
      RootView.FSelected.MouseDown([], P.X, P.Y, MouseDownOnMenu);
      if RootView.IsModalEnding then
        Exit;
      MouseDownOnMenu := False;  { never set MouseDownOnMenu to True on first click }
    end
    else if AExecuteSelected then begin
      RootView.ExecuteSelected(not AFromMSAA);
      if RootView.IsModalEnding then
        Exit;
    end;
    OriginalActiveWindow := GetActiveWindow;
    while ContinueLoop do begin
      { Examine the next message before popping it out of the queue }
      if not PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then begin
        WaitMessage;
        Continue;
      end;
      case Msg.message of
        WM_LBUTTONDOWN, WM_RBUTTONDOWN: begin
            P := SmallPointToPoint(TSmallPoint(Msg.lParam));
            Windows.ClientToScreen(Msg.hwnd, P);
            Ctl := FindDragTarget(P, True);
            { Was the mouse not clicked on a popup, or was it clicked on a
              popup that is not a child of RootView?
              (The latter can happen when in customization mode, for example,
              if the user right-clicks a popup menu being customized and
              the context menu is displayed.) }
            if not(Ctl is TTBPopupWindow) or
               not RootView.ContainsView(TTBPopupWindow(Ctl).View) then begin
              { If the root view is a popup, or if the root view is a toolbar
                and the user clicked outside the toolbar or in its non-client
                area (e.g. on its drag handle), exit }
              if RootView.FIsPopup or (Ctl <> RootView.FWindow) or
                 not PtInRect(RootView.FWindow.ClientRect, RootView.FWindow.ScreenToClient(P)) then
                Exit
              else
                if Msg.message = WM_LBUTTONDOWN then begin
                  { If the user clicked inside a toolbar on anything but an
                    item, exit }
                  UpdateAllSelections(P, True);
                  if (RootView.FSelected = nil) or not RootView.FMouseOverSelected or
                     (tbisClicksTransparent in RootView.FSelected.Item.ItemStyle) then
                    Exit;
                end;
            end;
        end;
      end;
      { Now pop the message out of the queue }
      if not PeekMessage(Msg, 0, Msg.message, Msg.message, PM_REMOVE or PM_NOYIELD) then
        Continue;
      case Msg.message of
        $4D:
          { This undocumented message is sent to the focused window when
            F1 is pressed. Windows handles it by sending a WM_HELP message
            to the same window. We don't want this to happen while a menu
            is up, so swallow the message. }
          ;
        WM_CONTEXTMENU:
          { Windows still sends WM_CONTEXTMENU messages for "context menu"
            keystrokes even if WM_KEYUP messages are never dispatched,
            so it must specifically ignore this message }
          ;
        WM_KEYFIRST..WM_KEYLAST: begin
            Application.CancelHint;
            MouseIsDown := (GetKeyState(VK_LBUTTON) < 0) or
              (TrackRightButton and (GetKeyState(VK_RBUTTON) < 0));
            case Msg.message of
              WM_KEYDOWN, WM_SYSKEYDOWN:
                begin
                  if Msg.wParam = VK_PROCESSKEY then
                    { Don't let IME process the key }
                    Msg.wParam := ImmGetVirtualKey(Msg.hwnd);
                  if not MouseIsDown or (Msg.wParam = VK_F1) then begin
                    Key := Word(Msg.wParam);
                    if SendKeyEvent(GetActiveView, Key,
                       KeyDataToShiftState(Msg.lParam)) then
                      Exit;
                    { If it's not handled by a KeyDown method, translate
                      it into a WM_*CHAR message }
                    if Key <> 0 then
                      TranslateMessage(Msg);
                  end;
                end;
              WM_CHAR, WM_SYSCHAR:
                if not MouseIsDown then begin
                  View := GetActiveView;
                  Viewer := View.NextSelectableWithAccel(View.FSelected,
                    Chr(Msg.WParam), False, IsOnlyItemWithAccel);
                  if Viewer = nil then begin
                    if (Msg.WParam in [VK_SPACE, Ord('-')]) and
                       not RootView.FIsPopup and (View = RootView) and
                       (GetActiveWindow <> 0) then begin
                      RootView.EndModalWithSystemMenu(GetActiveWindow,
                        Msg.WParam);
                      Exit;
                    end
                    else
                      MessageBeep(0);
                  end
                  else begin
                    View.Selected := Viewer;
                    View.ScrollSelectedIntoView;
                    if IsOnlyItemWithAccel then
                      View.ExecuteSelected(True);
                  end;
                end;
            end;
          end;
        WM_TIMER:
          begin
            Ctl := FindControl(Msg.hwnd);
            if Assigned(Ctl) and (Ctl is TTBPopupWindow) and
               (Msg.wParam >= ViewTimerBaseID + Ord(Low(TTBViewTimerID))) and
               (Msg.wParam <= ViewTimerBaseID + Ord(High(TTBViewTimerID))) then begin
              if Assigned(TTBPopupWindow(Ctl).FView) then
                HandleTimer(TTBPopupWindow(Ctl).FView,
                  TTBViewTimerID(WPARAM(Msg.wParam - ViewTimerBaseID)));
            end
            else
              DispatchMessage(Msg);
          end;
        $118: ;
            { ^ Like standard menus, don't dispatch WM_SYSTIMER messages
              (the internal Windows message used for things like caret
              blink and list box scrolling). }
        WM_MOUSEFIRST..WM_MOUSELAST:
          case Msg.message of
            WM_MOUSEMOVE: begin
                if (Msg.pt.X <> LastPos.X) or (Msg.pt.Y <> LastPos.Y) then begin
                  LastPos := Msg.pt;
                  MouseMoved;
                end;
                if GetSelectedViewer(View, Viewer) then begin
                  P := Viewer.ScreenToClient(Msg.pt);
                  Viewer.MouseMove(P.X, P.Y);
                end;
              end;
            WM_MOUSEWHEEL:
              if GetSelectedViewer(View, Viewer) then begin
                P := Viewer.ScreenToClient(Msg.pt);
                Viewer.MouseWheel(Smallint(LongRec(Msg.wParam).Hi), P.X, P.Y);
              end;
            WM_LBUTTONDOWN, WM_LBUTTONDBLCLK, WM_RBUTTONDOWN:
              if (Msg.message <> WM_RBUTTONDOWN) or TrackRightButton then begin
                Application.CancelHint;
                MouseDownOnMenu := False;
                Exclude(RootView.FState, vsIgnoreFirstMouseUp);
                UpdateAllSelections(Msg.pt, True);
                if GetSelectedViewer(View, Viewer) then begin
                  if Msg.message <> WM_LBUTTONDBLCLK then
                    Shift := []
                  else
                    Shift := [ssDouble];
                  P := Viewer.ScreenToClient(Msg.pt);
                  Viewer.MouseDown(Shift, P.X, P.Y, MouseDownOnMenu);
                  LastPos := SmallPointToPoint(TSmallPoint(GetMessagePos()));
                end;
              end;
            WM_LBUTTONUP, WM_RBUTTONUP:
              if (Msg.message = WM_LBUTTONUP) or TrackRightButton then begin
                UpdateAllSelections(Msg.pt, False);
                { ^ False is used so that when a popup menu is
                  displayed with the cursor currently inside it, the item
                  under the cursor won't be accidentally selected when the
                  user releases the button. The user must move the mouse at
                  at least one pixel (generating a WM_MOUSEMOVE message),
                  and then release the button. }
                if not GetSelectedViewer(View, Viewer) then begin
                  { Mouse was not released over any item. Cancel out of the
                    loop if it's outside all views, or is inside unused
                    space on a topmost toolbar }
                  if not Assigned(View) or
                     ((View = RootView) and RootView.FIsToolbar) then begin
                    if not(vsIgnoreFirstMouseUp in RootView.FState) then
                      Exit
                    else
                      Exclude(RootView.FState, vsIgnoreFirstMouseUp);
                  end;
                end
                else begin
                  P := Viewer.ScreenToClient(Msg.pt);
                  Viewer.MouseUp(P.X, P.Y, MouseDownOnMenu);
                end;
              end;
          end;
      else
        DispatchMessage(Msg);
      end;
      if not ContinueLoop then
      begin
        Exit;
      end;
      if LastPos.X = Low(LastPos.X) then begin
        LastPos := SmallPointToPoint(TSmallPoint(GetMessagePos()));
        MouseMoved;
      end;
      UpdateAppHint;
    end;
  finally
    RootView.CancelCapture;
  end;
  finally
    RootView.FCaptureWnd := 0;
    Application.Hint := '';
    { Make sure there are no outstanding WM_*CHAR messages }
    RemoveMessages(WM_CHAR, WM_DEADCHAR);
    RemoveMessages(WM_SYSCHAR, WM_SYSDEADCHAR);
    { Nor any outstanding 'send WM_HELP' messages caused by an earlier press
      of the F1 key }
    RemoveMessages($4D, $4D);
  end;
end;


{ TTBPopupView }

procedure TTBPopupView.AutoSize(AWidth, AHeight: Integer);
begin
  with TTBPopupWindow(FWindow) do
    with GetNCSize do
      SetBounds(Left, Top, AWidth + (X * 2),
        AHeight + (Y * 2));
end;

function TTBPopupView.GetMonitor: TMonitor;
begin
  Result := Screen.MonitorFromRect(FWindow.BoundsRect);
end;

function TTBPopupView.GetFont: TFont;
begin
  Result := (Owner as TTBPopupWindow).Font;
end;


{ TTBPopupWindow }

constructor TTBPopupWindow.CreatePopupWindow(AOwner: TComponent;
  const AParentView: TTBView; const AItem: TTBCustomItem;
  const ACustomizing: Boolean; const PopupPoint: TPoint);
begin
  inherited Create(AOwner);
  Visible := False;
  SetBounds(PopupPoint.X, PopupPoint.Y, 320, 240);
  ControlStyle := ControlStyle - [csCaptureMouse];
  ShowHint := True;
  Color := tbMenuBkColor;
  FView := GetViewClass.CreateView(Self, AParentView, AItem, Self, False,
    ACustomizing, False);
  Include(FView.FState, vsModal);

  { Inherit the font from the parent view, or use the system menu font if
    there is no parent view }
  if Assigned(AParentView) then
    Font.Assign(AParentView.GetFont)
  else
    Font.Assign(GetToolbarFont(GetMonitorPixelsPerInch(Screen.MonitorFromPoint(PopupPoint))));

  { Inherit the accelerator visibility state from the parent view. If there
    is no parent view (i.e. it's a standalone popup menu), then default to
    hiding accelerator keys, but change this in CreateWnd if the last input
    came from the keyboard. }
  if Assigned(AParentView) then begin
    if vsUseHiddenAccels in AParentView.FStyle then
      Include(FView.FStyle, vsUseHiddenAccels);
    if vsShowAccels in AParentView.FState then
      Include(FView.FState, vsShowAccels);
  end
  else
    Include(FView.FStyle, vsUseHiddenAccels);

  if Application.Handle <> 0 then
    { Use Application.Handle if possible so that the taskbar button for the app
      doesn't pop up when a TTBEditItem on a popup menu is focused }
    ParentWindow := Application.Handle
  else
    { When Application.Handle is zero, use GetDesktopWindow() as the parent
      window, not zero, otherwise UpdateControlState won't show the window }
    ParentWindow := GetDesktopWindow;
end;

destructor TTBPopupWindow.Destroy;
begin
  Destroying;
  { Ensure window handle is destroyed *before* FView is freed, since
    DestroyWindowHandle calls NotifyWinEvent which may result in
    FView.HandleWMObject being called }
  if HandleAllocated then
    DestroyWindowHandle;
  FreeAndNil(FView);
  inherited;
end;

{MP}
procedure TTBPopupWindow.Cancel;
begin
  { noop }
end;

procedure TTBPopupWindow.BeforeDestruction;
begin
  { The inherited BeforeDestruction method hides the form. We need to close
    any child popups first, so that pixels behind the popups are properly
    restored without generating a WM_PAINT message. }
  if Assigned(FView) then
    FView.CloseChildPopups;
  inherited;
end;

function TTBPopupWindow.GetNCSize: TPoint;
begin
  Result.X := PopupMenuWindowNCSize;
  Result.Y := PopupMenuWindowNCSize;
end;

function TTBPopupWindow.GetViewClass: TTBViewClass;
begin
  Result := TTBPopupView;
end;

procedure TTBPopupWindow.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  inherited;
  with Params do begin
    Style := (Style and not (WS_CHILD or WS_GROUP or WS_TABSTOP)) or WS_POPUP;
    ExStyle := ExStyle or WS_EX_TOPMOST or WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    WindowClass.Style := WindowClass.Style or CS_DROPSHADOW;
  end;
end;

procedure TTBPopupWindow.CreateWnd;
const
  WM_CHANGEUISTATE = $0127;
  WM_QUERYUISTATE  = $0129;
  UIS_INITIALIZE = 3;
  UISF_HIDEACCEL = $2;
var
  B: Boolean;
begin
  inherited;
  { On a top-level popup window, send WM_CHANGEUISTATE & WM_QUERYUISTATE
    messages to the window to see if the last input came from the keyboard
    and if the accelerator keys should be shown }
  if (FView.ParentView = nil) and not FAccelsVisibilitySet then begin
    FAccelsVisibilitySet := True;
    SendMessage(Handle, WM_CHANGEUISTATE, UIS_INITIALIZE, 0);
    B := (SendMessage(Handle, WM_QUERYUISTATE, 0, 0) and UISF_HIDEACCEL = 0);
    FView.SetAccelsVisibility(B);
  end;
end;

procedure TTBPopupWindow.DestroyWindowHandle;
begin
  { Before destroying the window handle, we must stop any animation, otherwise
    the animation thread will use an invalid handle }
  TBEndAnimation(WindowHandle);
  { Cleanly destroy any timers before the window handle is destroyed }
  if Assigned(FView) then
    FView.StopAllTimers;
  NotifyWinEvent(EVENT_SYSTEM_MENUPOPUPEND, WindowHandle, OBJID_CLIENT,
    CHILDID_SELF);
  inherited;
end;

procedure TTBPopupWindow.WMGetObject(var Message: TMessage);
begin
  if not FView.HandleWMGetObject(Message) then
    inherited;
end;

procedure TTBPopupWindow.CMShowingChanged(var Message: TMessage);
const
  ShowFlags: array[Boolean] of UINT = (
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_HIDEWINDOW,
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  SPI_GETMENUFADE = $1012;
var
  Animate: BOOL;
  Blend: Boolean;
begin
  { Must override TCustomForm/TForm's CM_SHOWINGCHANGED handler so that the
    form doesn't get activated when Visible is set to True. }

  { Handle animation. NOTE: I do not recommend trying to enable animation on
    Windows 95 and NT 4.0 because there's a difference in the way the
    SetWindowPos works on those versions. See the comment in the
    TBStartAnimation function of TB2Anim.pas. }
  {$IFNDEF TB2K_NO_ANIMATION}
  if ((FView.ParentView = nil) or not(vsNoAnimation in FView.FParentView.FState)) and
     Showing and (FView.Selected = nil) and not IsWindowVisible(WindowHandle) and
     SystemParametersInfo(SPI_GETMENUANIMATION, 0, @Animate, 0) and Animate then begin
    Blend := SystemParametersInfo(SPI_GETMENUFADE, 0, @Animate, 0) and Animate;
    if Blend or (FAnimationDirection <> []) then begin
      if SendMessage(WindowHandle, WM_TB2K_POPUPSHOWING, TPS_ANIMSTART, 0) = 0 then
      begin
        { Start animation only if WM_TB2K_POPUPSHOWING returns zero (or not handled) }
        TBStartAnimation(WindowHandle, Blend, FAnimationDirection);
        Exit;
      end;
    end;
  end;
  {$ENDIF}

  { No animation... }
  if not Showing then begin
    { Call TBEndAnimation to ensure WS_EX_LAYERED style is removed before
      hiding, otherwise windows under the popup window aren't repainted
      properly. }
    TBEndAnimation(WindowHandle);
  end;
  SetWindowPos(WindowHandle, 0, 0, 0, 0, 0, ShowFlags[Showing]);
  if Showing then SendNotifyMessage(WindowHandle, WM_TB2K_POPUPSHOWING, TPS_NOANIM, 0);
end;

procedure TTBPopupWindow.WMTB2kAnimationEnded(var Message: TMessage);
begin
  SendNotifyMessage(WindowHandle, WM_TB2K_POPUPSHOWING, TPS_ANIMFINISHED, 0);
end;

procedure TTBPopupWindow.WMTB2kStepAnimation(var Message: TMessage);
begin
  TBStepAnimation(Message);
end;

procedure TTBPopupWindow.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  { May be necessary in some cases... }
  TBEndAnimation(WindowHandle);
  inherited;
end;

procedure TTBPopupWindow.WMPaint(var Message: TWMPaint);
begin
  { Must abort animation when a WM_PAINT message is received }
  TBEndAnimation(WindowHandle);
  inherited;
end;

procedure TTBPopupWindow.Paint;
begin
  FView.DrawSubitems(Canvas);
  PaintScrollArrows;
end;

procedure TTBPopupWindow.PaintScrollArrows;

  procedure DrawArrow(const R: TRect; ADown: Boolean);
  var
    X, Y: Integer;
    P: array[0..2] of TPoint;
  begin
    X := (R.Left + R.Right) div 2;
    Y := (R.Top + R.Bottom) div 2;
    Dec(Y);
    P[0].X := X-3;
    P[0].Y := Y;
    P[1].X := X+3;
    P[1].Y := Y;
    P[2].X := X;
    P[2].Y := Y;
    if ADown then
      Inc(P[2].Y, 3)
    else begin
      Inc(P[0].Y, 3);
      Inc(P[1].Y, 3);
    end;
    Canvas.Pen.Color := tbMenuTextColor;
    Canvas.Brush.Color := tbMenuTextColor;
    Canvas.Polygon(P);
  end;

begin
  if FView.FShowUpArrow then
    DrawArrow(Rect(0, 0, ClientWidth, tbMenuScrollArrowHeight), False);
  if FView.FShowDownArrow then
    DrawArrow(Bounds(0, ClientHeight - tbMenuScrollArrowHeight,
      ClientWidth, tbMenuScrollArrowHeight), True);
end;

procedure TTBPopupWindow.WMClose(var Message: TWMClose);
begin
  { do nothing -- ignore Alt+F4 keypresses }
end;

procedure TTBPopupWindow.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  with GetNCSize do
    InflateRect(Message.CalcSize_Params^.rgrc[0], -X, -Y);
  inherited;
end;

procedure PopupWindowNCPaintProc(Control: TControl; Wnd: HWND; DC: HDC; AppData: Longint);
var
  R: TRect;
  {$IFNDEF TB2K_USE_STRICT_O2K_MENU_STYLE}
  Brush: HBRUSH;
  {$ENDIF}
begin
  GetWindowRect(Wnd, R);  OffsetRect(R, -R.Left, -R.Top);
  {$IFNDEF TB2K_USE_STRICT_O2K_MENU_STYLE}
  if not AreFlatMenusEnabled then begin
  {$ENDIF}
    DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_ADJUST);
    FrameRect(DC, R, GetSysColorBrush(COLOR_BTNFACE));
  {$IFNDEF TB2K_USE_STRICT_O2K_MENU_STYLE}
  end
  else begin
    FrameRect(DC, R, GetSysColorBrush(COLOR_BTNSHADOW));
    Brush := CreateSolidBrush(ColorToRGB(TTBPopupWindow(AppData).Color));
    InflateRect(R, -1, -1);
    FrameRect(DC, R, Brush);
    InflateRect(R, -1, -1);
    FrameRect(DC, R, Brush);
    DeleteObject(Brush);
  end;
  {$ENDIF}
end;

procedure TTBPopupWindow.WMNCPaint(var Message: TMessage);
var
  DC: HDC;
begin
  DC := GetWindowDC(Handle);
  try
    SelectNCUpdateRgn(Handle, DC, HRGN(Message.WParam));
    PopupWindowNCPaintProc(Self, Handle, DC, Longint(Self));
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TTBPopupWindow.WMPrint(var Message: TMessage);
begin
  HandleWMPrint(Self, Handle, Message, PopupWindowNCPaintProc, Longint(Self));
end;

procedure TTBPopupWindow.WMPrintClient(var Message: TMessage);
begin
  HandleWMPrintClient(Self, Message);
end;

procedure TTBPopupWindow.CMHintShow(var Message: TCMHintShow);
begin
  with Message.HintInfo^ do begin
    HintStr := '';
    if Assigned(FView.Selected) then begin
      CursorRect := FView.Selected.BoundsRect;
      HintStr := FView.FSelected.GetHintText;
    end;
  end;
end;

procedure TTBPopupWindow.CMHintShowPause(var Message: TMessage);
begin
  // Hint was not active previously
  if not Boolean(Message.WParam) then
  begin
    PInteger(Message.LParam)^ := PInteger(Message.LParam)^ * 2;
  end;
end;


{ TTBItemContainer }

constructor TTBItemContainer.Create(AOwner: TComponent);
begin
  inherited;
  FItem := TTBRootItem.Create(Self);
  FItem.ParentComponent := Self;
end;

destructor TTBItemContainer.Destroy;
begin
  FItem.Free;
  inherited;
end;

function TTBItemContainer.GetItems: TTBCustomItem;
begin
  Result := FItem;
end;

procedure TTBItemContainer.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  FItem.GetChildren(Proc, Root);
end;

function TTBItemContainer.GetImages: TCustomImageList;
begin
  Result := FItem.SubMenuImages;
end;

procedure TTBItemContainer.SetImages(Value: TCustomImageList);
begin
  FItem.SubMenuImages := Value;
end;


{ TTBPopupMenu }

constructor TTBPopupMenu.Create(AOwner: TComponent);
begin
  inherited;
  FItem := GetRootItemClass.Create(Self);
  FItem.ParentComponent := Self;
  FItem.OnClick := RootItemClick;
end;

destructor TTBPopupMenu.Destroy;
begin
  FItem.Free;
  inherited;
end;

function TTBPopupMenu.GetItems: TTBCustomItem;
begin
  Result := FItem;
end;

procedure TTBPopupMenu.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  FItem.GetChildren(Proc, Root);
end;

procedure TTBPopupMenu.SetChildOrder(Child: TComponent; Order: Integer);
begin
  FItem.SetChildOrder(Child, Order);
end;

function TTBPopupMenu.GetRootItemClass: TTBRootItemClass;
begin
  Result := TTBRootItem;
end;

function TTBPopupMenu.GetImages: TCustomImageList;
begin
  Result := FItem.SubMenuImages;
end;

function TTBPopupMenu.GetLinkSubitems: TTBCustomItem;
begin
  Result := FItem.LinkSubitems;
end;

function TTBPopupMenu.GetOptions: TTBItemOptions;
begin
  Result := FItem.Options;
end;

procedure TTBPopupMenu.SetImages(Value: TCustomImageList);
begin
  FItem.SubMenuImages := Value;
end;

procedure TTBPopupMenu.SetLinkSubitems(Value: TTBCustomItem);
begin
  FItem.LinkSubitems := Value;
end;

procedure TTBPopupMenu.SetOptions(Value: TTBItemOptions);
begin
  FItem.Options := Value;
end;

procedure TTBPopupMenu.RootItemClick(Sender: TObject);
begin
  if Sender = FItem then
    Sender := Self;
  DoPopup(Sender);
end;

procedure TTBPopupMenu.Popup(X, Y: Integer);
begin
  PopupEx(X, Y, False);
end;

function TTBPopupMenu.PopupEx(X, Y: Integer;
  ReturnClickedItemOnly: Boolean = False): TTBCustomItem;
begin
  SetPopupPoint(Point(X, Y));
  Result := FItem.Popup(X, Y, TrackButton = tbRightButton,
    TTBPopupAlignment(Alignment), ReturnClickedItemOnly);
end;

function TTBPopupMenu.IsShortCut(var Message: TWMKey): Boolean;
begin
  Result := FItem.IsShortCut(Message);
end;


{ TTBImageList }

constructor TTBCustomImageList.Create(AOwner: TComponent);
begin
  inherited;
  FCheckedImagesChangeLink := TChangeLink.Create;
  FCheckedImagesChangeLink.OnChange := ImageListChanged;
  FDisabledImagesChangeLink := TChangeLink.Create;
  FDisabledImagesChangeLink.OnChange := ImageListChanged;
  FHotImagesChangeLink := TChangeLink.Create;
  FHotImagesChangeLink.OnChange := ImageListChanged;
  FImagesBitmap := TBitmap.Create;
  FImagesBitmap.OnChange := ImagesBitmapChanged;
  FImagesBitmapMaskColor := clFuchsia;
end;

destructor TTBCustomImageList.Destroy;
begin
  FreeAndNil(FImagesBitmap);
  FreeAndNil(FHotImagesChangeLink);
  FreeAndNil(FDisabledImagesChangeLink);
  FreeAndNil(FCheckedImagesChangeLink);
  inherited;
end;

procedure TTBCustomImageList.ImagesBitmapChanged(Sender: TObject);
begin
  if not ImagesBitmap.Empty then begin
    Clear;
    AddMasked(ImagesBitmap, FImagesBitmapMaskColor);
  end;
end;

procedure TTBCustomImageList.ImageListChanged(Sender: TObject);
begin
  Change;
end;

procedure TTBCustomImageList.DefineProperties(Filer: TFiler);
type
  TProc = procedure(ASelf: TObject; Filer: TFiler);
begin
  if (Filer is TReader) or FImagesBitmap.Empty then
    inherited
  else
    { Bypass TCustomImageList.DefineProperties when we've got an ImageBitmap }
    TProc(@TComponentAccess.DefineProperties)(Self, Filer);
end;

procedure TTBCustomImageList.DrawState(Canvas: TCanvas; X, Y, Index: Integer;
  Enabled, Selected, Checked: Boolean);
begin
  if not Enabled and Assigned(DisabledImages) then
    DisabledImages.Draw(Canvas, X, Y, Index)
  else if Checked and Assigned(CheckedImages) then
    CheckedImages.Draw(Canvas, X, Y, Index, Enabled)
  else if Selected and Assigned(HotImages) then
    HotImages.Draw(Canvas, X, Y, Index, Enabled)
  else
    Draw(Canvas, X, Y, Index, Enabled);
end;

procedure TTBCustomImageList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = CheckedImages then CheckedImages := nil;
    if AComponent = DisabledImages then DisabledImages := nil;
    if AComponent = HotImages then HotImages := nil;
  end;
end;

procedure TTBCustomImageList.ChangeImages(var AImageList: TCustomImageList;
  Value: TCustomImageList; AChangeLink: TChangeLink);
begin
  if Value = Self then
    Value := nil;
  if AImageList <> Value then begin
    if Assigned(AImageList) then
      AImageList.UnregisterChanges(AChangeLink);
    AImageList := Value;
    if Assigned(Value) then begin
      Value.RegisterChanges(AChangeLink);
      Value.FreeNotification(Self);
    end;
    { Don't call Change while loading because it causes the Delphi IDE to
      think the form has been modified (?). Also, don't call Change while
      destroying since there's no reason to. }
    if not(csLoading in ComponentState) and
       not(csDestroying in ComponentState) then
      Change;
  end;
end;

procedure TTBCustomImageList.SetCheckedImages(Value: TCustomImageList);
begin
  ChangeImages(FCheckedImages, Value, FCheckedImagesChangeLink);
end;

procedure TTBCustomImageList.SetDisabledImages(Value: TCustomImageList);
begin
  ChangeImages(FDisabledImages, Value, FDisabledImagesChangeLink);
end;

procedure TTBCustomImageList.SetHotImages(Value: TCustomImageList);
begin
  ChangeImages(FHotImages, Value, FHotImagesChangeLink);
end;

procedure TTBCustomImageList.SetImagesBitmap(Value: TBitmap);
begin
  FImagesBitmap.Assign(Value);
end;

procedure TTBCustomImageList.SetImagesBitmapMaskColor(Value: TColor);
begin
  if FImagesBitmapMaskColor <> Value then begin
    FImagesBitmapMaskColor := Value;
    ImagesBitmapChanged(nil);
  end;
end;


{ TTBBaseAccObject }

{ According to the MSAA docs:
  "With Active Accessibility 2.0, servers can return E_NOTIMPL from IDispatch
  methods and Active Accessibility will implement the IAccessible interface
  for them."
  And there was much rejoicing. }

function TTBBaseAccObject.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TTBBaseAccObject.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TTBBaseAccObject.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TTBBaseAccObject.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;


{ Initialization & finalization }

var
  ToolbarFonts: TDictionary<Integer, TFont>;
  ToolbarFont: TFont;

function CreateToolbarFont(PixelsPerInch: Integer): HFONT;
var
  NonClientMetrics: TNonClientMetrics;
begin
  FillChar(NonClientMetrics, SizeOf(NonClientMetrics), 0);
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  Assert(HasSystemParametersInfoForPixelsPerInch);
  if SystemParametersInfoForPixelsPerInch(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0, PixelsPerInch) then
  begin
    Result := CreateFontIndirect(NonClientMetrics.lfMenuFont);
  end
    else
  begin
    Result := 0;
  end;
end;

function GetToolbarFont(Control: TControl; PixelsPerInch: Integer): TFont; overload;
var
  H: HFONT;
begin
  // Temporary redundant legacy fallback to limit impact of per-monitor DPI change
  if not HasSystemParametersInfoForPixelsPerInch then
  begin
    Result := ToolbarFont;
  end
    else
  begin
    // See the comment in TTBView.GetFont
    if not Assigned(ToolbarFonts) then
    begin
      Result := nil;
    end
      else
    begin
      if PixelsPerInch < 0 then
      begin
        PixelsPerInch := GetControlPixelsPerInch(Control);
      end;
      if not ToolbarFonts.TryGetValue(PixelsPerInch, Result) then
      begin
        H := CreateToolbarFont(PixelsPerInch);
        if H <> 0 then
        begin
          Result := TFont.Create;
          Result.Handle := H;
          ToolbarFonts.Add(PixelsPerInch, Result);
        end
          else
        begin
          Result := nil;
        end;
      end
    end;
  end;
end;

function GetToolbarFont(PixelsPerInch: Integer): TFont; overload;
begin
  Result := GetToolbarFont(nil, PixelsPerInch);
end;

function GetToolbarFont(Control: TControl): TFont; overload;
begin
  Result := GetToolbarFont(Control, -1);
end;

procedure TBInitToolbarSystemFont;
var
  NonClientMetrics: TNonClientMetrics;
  FontPair: TPair<Integer, TFont>;
begin
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
  begin
    ToolbarFont.Handle := CreateFontIndirect(NonClientMetrics.lfMenuFont);
  end;

  for FontPair in ToolbarFonts do
  begin
    FontPair.Value.Handle := CreateToolbarFont(FontPair.Key);
  end;
end;

procedure TBFinalizeToolbarSystemFont;
var
  Font: TFont;
begin
  for Font in ToolbarFonts.Values do
  begin
    Font.Free;
  end;
end;

initialization
  ToolbarFonts := TDictionary<Integer, TFont>.Create;
  ToolbarFont := TFont.Create;
  TBInitToolbarSystemFont;
finalization
  DestroyClickWnd;
  FreeAndNil(ToolbarFont);
  TBFinalizeToolbarSystemFont;
  FreeAndNil(ToolbarFonts);
end.
