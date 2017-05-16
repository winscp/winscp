{MP}
unit TBXExtItems;

// TBX Package
// Copyright 2001-2004 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// Id: TBXExtItems.pas 16 2004-05-26 02:02:55Z Alex@ZEISS

interface

{$I TB2Ver.inc}

uses
  Windows, Messages, Classes, SysUtils, Graphics, Controls, StdCtrls, ExtCtrls,
  TBX, TBXThemes, TB2Item, TB2Toolbar, TB2ExtItems, TBXLists{$IFNDEF MPEXCLUDE}, TB2MRU{$ENDIF};

const
  tcrNumericProperty = 3;
  tcrSpinButton = 4;
  tcrList = 5;

type
  TTBXEditItemViewer = class;
  TTBXEditChange = procedure(Sender: TObject; const Text: string) of object;

  { TTBXEditItem }
  { Extends standard TTBEditItem, providing additional features and some
    combo box functionality, which is used in descendants }

  TTBXEditItem = class(TTBEditItem)
  private
    FAlignment: TAlignment;
    FAutoCompleteCounter: Integer;
    FEditorFontSettings: TFontSettings;
    FFontSettings: TFontSettings;
    FIsChanging: Boolean;
    FLastEditChange: string;
    FPasswordChar: Char;
    FReadOnly: Boolean;
    FShowImage: Boolean;
    FOnChange: TTBXEditChange;
    procedure FontSettingsChanged(Sender: TObject);
    procedure SetAlignment(Value: TAlignment);
    procedure SetPasswordChar(Value: Char);
    procedure SetShowImage(const Value: Boolean);
    procedure SetFontSettings(Value: TFontSettings);
  protected
    function  DoAcceptText(var NewText: string): Boolean; override;
    function  DoAutoComplete(var AText: string): Boolean; virtual;
    procedure DoBeginEdit(Viewer: TTBEditItemViewer); override;
    procedure DoChange(const AText: string); virtual;
    procedure DoTextChanged(Reason: Integer); override;
    function  GetImageIndex: Integer; virtual;
    function  GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    procedure GetPopupPosition(ParentView: TTBView; PopupWindow: TTBPopupWindow; var PopupPositionRec: TTBPopupPositionRec); override;
    function  GetPopupWindowClass: TTBPopupWindowClass; override;
    procedure HandleEditChange(Edit: TEdit); virtual;
  public
    function StartEditing(AView: TTBView): Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property EditorFontSettings: TFontSettings read FEditorFontSettings write FEditorFontSettings;
    property ExtendedAccept;
    property FontSettings: TFontSettings read FFontSettings write SetFontSettings;
    property ImageIndex;
    property Images;
    property PasswordChar: Char read FPasswordChar write SetPasswordChar default #0;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property ShowImage: Boolean read FShowImage write SetShowImage default False;
    property OnChange: TTBXEditChange read FOnChange write FOnChange;
    property OnSelect;
  end;

  TTBXEditItemViewer = class(TTBEditItemViewer)
  private
    procedure EditChangeHandler(Sender: TObject);
    function  MeasureEditCaption: TSize;
    function  MeasureTextHeight: Integer;
    procedure HandleEditChange(Edit: TEdit);
  protected
    OldWndProc: TWndMethod;
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    function  DoExecute: Boolean; override;
    function  HandleEditMessage(var Message: TMessage): Boolean; virtual;
    function  GetAccRole: Integer; override;
    procedure GetItemInfo(const Canvas: TCanvas; out ItemInfo: TTBXItemInfo; IsHoverItem, IsPushed, UseMenuColor: Boolean); virtual;
    function  GetEditControlClass: TEditClass; override;
    procedure GetEditInfo(out EditInfo: TTBXEditInfo; const ItemInfo: TTBXItemInfo); virtual;
    function  GetIndentBefore: Integer; virtual;
    function  GetIndentAfter: Integer; virtual;
    procedure GetEditRect(var R: TRect); override;
    function  IsToolbarSize: Boolean; override;
    procedure NewEditWndProc(var Message: TMessage);
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect; IsHoverItem, IsPushed, UseDisabledShadow: Boolean); override;
    function  ShowImage: Boolean; virtual;
    {MP}
    function  StripTextHotkey: Boolean; virtual;
  public
    function  IsToolbarStyle: Boolean; override;
  end;

  {$IFNDEF MPEXCLUDE}
  { TTBXSpinEditItem }
  TTBXCustomSpinEditItem = class;

  TSEValueType = (evtInteger, evtFloat, evtHex);
  TDecimal = 0..10;
  TSEChangeEvent = procedure(Sender: TTBXCustomSpinEditItem; const AValue: Extended) of object;
  TSEConvertEvent = procedure(Sender: TTBXCustomSpinEditItem; const APrefix, APostfix: string; var AValue: Extended; var CanConvert: Boolean) of object;
  TSEStepEvent = procedure(Sender: TTBXCustomSpinEditItem; Step: Integer; const OldValue: Extended; var NewValue: Extended) of object;
  TSETextToValueEvent = procedure(Sender: TTBXCustomSpinEditItem; const AText: string; out AValue: Extended; var CanConvert: Boolean) of object;
  TSEValueToTextEvent = procedure(Sender: TTBXCustomSpinEditItem; const AValue: Extended; var Text: string) of object;

  TTBXCustomSpinEditItem = class(TTBXEditItem)
  private
    FDecimal: TDecimal;
    FLastGoodValue: Extended;
    FMaxValue: Extended;
    FMinValue: Extended;
    FIncrement: Extended;
    FSpaceBeforePostfix: Boolean;
    FSpaceAfterPrefix: Boolean;
    FPostfix: string;
    FPrefix: string;
    FSnap: Boolean;
    FValueType: TSEValueType;
    FOnConvert: TSEConvertEvent;
    FOnTextToValue: TSETextToValueEvent;
    FOnValueChange: TSEChangeEvent;
    FOnValueToText: TSEValueToTextEvent;
    FOnStep: TSEStepEvent;
    function  IsIncrementStored: Boolean;
    function  IsMinValueStored: Boolean;
    function  IsMaxValueStored: Boolean;
    function  IsValueStored: Boolean;
    function  GetValue: Extended;
    procedure SetValue(NewValue: Extended);
    procedure SetValueType(NewType: TSEValueType);
    procedure SetDecimal(NewDecimal: TDecimal);
    procedure SetIncrement(const NewIncrement: Extended);
    procedure SetPostfix(const NewPostfix: string);
    procedure SetPrefix(const NewPrefix: string);
    procedure SetSpaceAfterPrefix(UseSpace: Boolean);
    procedure SetSpaceBeforePostfix(UseSpace: Boolean);
    function  ValidateUnits(const S: string): Boolean;
    function  GetAsInteger: Integer;
    procedure SetAsInteger(AValue: Integer);
  protected
    function  CheckValue(const V: Extended): Extended;
    procedure ClickUp;
    procedure ClickDown;
    function  DoAcceptText(var NewText: string): Boolean; override;
    function  DoConvert(const APrefix, APostfix: string; var AValue: Extended): Boolean; virtual;
    procedure DoStep(Step: Integer; const OldValue: Extended; var NewValue: Extended); virtual;
    procedure DoTextChanged(Reason: Integer); override;
    function  DoTextToValue(const AText: string; out AValue: Extended): Boolean; virtual;
    procedure DoValueChange(const V: Extended); virtual;
    procedure DoValueToText(const NewValue: Extended; var NewText: string); virtual;
    function  GetAsText(AValue: Extended): string;
    function  GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    function  ParseValue(const S: string; out V: Extended): Boolean;
    procedure SetValueEx(NewValue: Extended; Reason: Integer);
    property Alignment default taRightJustify;
    property OnConvert: TSEConvertEvent read FOnConvert write FOnConvert;
    property OnStep: TSEStepEvent read FOnStep write FOnStep;
    property OnTextToValue: TSETextToValueEvent read FOnTextToValue write FOnTextToValue;
    property OnValueChange: TSEChangeEvent read FOnValueChange write FOnValueChange;
    property OnValueToText: TSEValueToTextEvent read FOnValueToText write FOnValueToText;
  public
    constructor Create(AOwner: TComponent); override;
    property ValueType: TSEValueType read FValueType write SetValueType default evtInteger;
    property AsInteger: Integer read GetAsInteger write SetAsInteger stored False;
    property Decimal: TDecimal read FDecimal write SetDecimal default 2;
    property Increment: Extended read FIncrement write SetIncrement stored IsIncrementStored;
    property MaxValue: Extended read FMaxValue write FMaxValue stored IsMaxValueStored;
    property MinValue: Extended read FMinValue write FMinValue stored IsMinValueStored;
    property Postfix: string read FPostfix write SetPostfix;
    property Prefix: string read FPrefix write SetPrefix;
    property Snap: Boolean read FSnap write FSnap default True;
    property SpaceAfterPrefix: Boolean read FSpaceAfterPrefix write SetSpaceAfterPrefix;
    property SpaceBeforePostfix: Boolean read FSpaceBeforePostfix write SetSpaceBeforePostfix;
    property Value: Extended read GetValue write SetValue stored IsValueStored;
  published
    property Text stored False;
  end;

  TTBXSpinEditItem = class(TTBXCustomSpinEditItem)
  published
    property ValueType;
    property Alignment;
    property Decimal;
    property Increment;
    property MaxValue;
    property MinValue;
    property Postfix;
    property Prefix;
    property Snap;
    property SpaceAfterPrefix;
    property SpaceBeforePostfix;
    property Value;
    property OnConvert;
    property OnStep;
    property OnTextToValue;
    property OnValueChange;
    property OnValueToText;
  end;

  TSEBtnState = (ebsNone, ebsUp, ebsDown);

  TTBXSpinEditViewer = class(TTBXEditItemViewer)
  private
    FBtnState: TSEBtnState;
    FBtnTimer: TTimer;
    procedure TimerHandler(Sender: TObject);
  protected
    function  GetIndentAfter: Integer; override;
    procedure GetEditInfo(out EditInfo: TTBXEditInfo; const ItemInfo: TTBXItemInfo); override;
    function  HandleEditMessage(var Message: TMessage): Boolean; override;
    procedure InvalidateButtons;
    function  IsPtInButtonPart(X, Y: Integer): Boolean; override;
    procedure LosingCapture; override;
    procedure MouseDown(Shift: TShiftState; X, Y: Integer; var MouseDownOnMenu: Boolean); override;
    procedure MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean); override;
  public
    destructor Destroy; override;
  end;
  {$ENDIF}

  { TTBXCustomDropDownItem }
  { An extended edit item tb2k with a button. The dropdown list support is
    implemented in descendants, such as TTBXComboBoxItem }

  TTBXCustomDropDownItem = class(TTBXEditItem)
  private
    FAlwaysSelectFirst: Boolean;
    FDropDownList: Boolean;
    {MP}
    FOnCancel: TNotifyEvent;
  protected
    function CreatePopup(const ParentView: TTBView; const ParentViewer: TTBItemViewer;
      const PositionAsSubmenu, SelectFirstItem, Customizing: Boolean;
      const APopupPoint: TPoint; const Alignment: TTBPopupAlignment): TTBPopupWindow; override;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    function GetPopupWindowClass: TTBPopupWindowClass; override;
    procedure DoCancel;
  public
    constructor Create(AOwner: TComponent); override;
    property AlwaysSelectFirst: Boolean read FAlwaysSelectFirst write FAlwaysSelectFirst default True;
    property DropDownList: Boolean read FDropDownList write FDropDownList default False;
    {MP}
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
  end;

  TTBXDropDownItem = class(TTBXCustomDropDownItem)
  published
    property AlwaysSelectFirst;
    property DropDownList;
    property LinkSubitems;
    property SubMenuImages;
  end;

  TTBXDropDownItemViewer = class(TTBXEditItemViewer)
  protected
    procedure GetCursor(const Pt: TPoint; var ACursor: HCURSOR); override;
    procedure GetEditInfo(out EditInfo: TTBXEditInfo; const ItemInfo: TTBXItemInfo); override;
    function  GetIndentAfter: Integer; override;
    function  HandleEditMessage(var Message: TMessage): Boolean; override;
    function  IsPtInButtonPart(X, Y: Integer): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

  { TTBXComboBoxItem }
  { A combination of dropdown combo with a stringlist subitem }
  TTBXComboBoxItem = class;
  TTBXCAdjustImageIndex = procedure(Sender: TTBXComboBoxItem; const AText: string;
    AIndex: Integer; var ImageIndex: Integer) of object;

  TTBXComboBoxItem = class(TTBXCustomDropDownItem)
  private
    FAutoComplete: Boolean;
    FList: TTBXStringList;
    FOnItemClick: TNotifyEvent;
    FOnAdjustImageIndex: TTBXCAdjustImageIndex;
    procedure AdjustImageIndexHandler(Sender: TTBXCustomList; AItemIndex: Integer; var ImageIndex: Integer);
    function GetItemIndex: Integer;
    function GetMaxVisibleItems: Integer;
    function GetMaxWidth: Integer;
    function GetMinWidth: Integer;
    function GetStrings: TStrings;
    function GetShowListImages: Boolean;
    function GetOnClearItem: TTBXLPaintEvent;
    function GetOnDrawItem: TTBXLPaintEvent;
    function GetOnMeasureHeight: TTBXLMeasureHeight;
    function GetOnMeasureWidth: TTBXLMeasureWidth;
    procedure ListChangeHandler(Sender: TObject);
    procedure ListClickHandler(Sender: TObject);
    procedure SetItemIndex(Value: Integer);
    procedure SetMaxVisibleItems(Value: Integer);
    procedure SetMaxWidth(Value: Integer);
    procedure SetMinWidth(Value: Integer);
    procedure SetOnClearItem(Value: TTBXLPaintEvent);
    procedure SetOnDrawItem(Value: TTBXLPaintEvent);
    procedure SetOnMeasureHeight(Value: TTBXLMeasureHeight);
    procedure SetOnMeasureWidth(Value: TTBXLMeasureWidth);
    procedure SetStrings(Value: TStrings);
    procedure SetShowListImages(Value: Boolean);
  protected
    CachedImageIndex: Integer;
    CacheValid: Boolean;
    IsChanging: Boolean;
    procedure AdjustImageIndex(const AText: string; AIndex: Integer; var ImageIndex: Integer); virtual;
    function  DoAutoComplete(var AText: string): Boolean; override;
    procedure DoListChange; virtual;
    procedure DoListClick; virtual;
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); override;
    function  GetImageIndex: Integer; override;
    function  GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    function  GetStringListClass: TTBXStringListClass; virtual;
    procedure HandleEditChange(Edit: TEdit); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex default -1;
    procedure ChangeScale(M, D: Integer); override;
  published
    property AutoComplete: Boolean read FAutoComplete write FAutoComplete default True;
    property DropDownList;
    property MaxListWidth: Integer read GetMaxWidth write SetMaxWidth default 0;
    property MaxVisibleItems: Integer read GetMaxVisibleItems write SetMaxVisibleItems default 8;
    property MinListWidth: Integer read GetMinWidth write SetMinWidth default 64;
    property ShowListImages: Boolean read GetShowListImages write SetShowListImages default False;
    property Strings: TStrings read GetStrings write SetStrings;
    property SubMenuImages;
    property OnChange;
    property OnAdjustImageIndex: TTBXCAdjustImageIndex read FOnAdjustImageIndex write FOnAdjustImageIndex;
    property OnClearItem: TTBXLPaintEvent read GetOnClearItem write SetOnClearItem;
    property OnDrawItem: TTBXLPaintEvent read GetOnDrawItem write SetOnDrawItem;
    property OnItemClick: TNotifyEvent read FOnItemClick write FOnItemClick;
    property OnMeasureHeight: TTBXLMeasureHeight read GetOnMeasureHeight write SetOnMeasureHeight;
    property OnMeasureWidth: TTBXLMeasureWidth read GetOnMeasureWidth write SetOnMeasureWidth;
    property OnPopup;
    {MP}
    property OnCancel;
  end;

  TTBXComboBoxItemViewer = class(TTBXDropDownItemViewer)
  protected
    function HandleEditMessage(var Message: TMessage): Boolean; override;
    {MP}
    function StripTextHotkey: Boolean; override;
  end;

  { TTBXLabelItem }

  TTBXLabelOrientation = (tbxoAuto, tbxoHorizontal, tbxoVertical);
  TNonNegativeInt = 0..MaxInt;

  TTBXLabelItem = class(TTBCustomItem)
  private
    FCaption: TCaption;
    FFontSettings: TFontSettings;
    FMargin: Integer;
    FShowAccelChar: Boolean;
    FOrientation: TTBXLabelOrientation;
    {MP}
    FFixedSize: Integer;
    FSectionHeader: Boolean;
    FOnAdjustFont: TAdjustFontEvent;
    procedure FontSettingsChanged(Sender: TObject);
    procedure SetMargin(Value: Integer);
    procedure SetOrientation(Value: TTBXLabelOrientation);
    procedure SetCaption(const Value: TCaption);
    procedure SetFontSettings(Value: TFontSettings);
    procedure SetShowAccelChar(Value: Boolean);
    {MP}
    procedure SetFixedSize(Value: Integer);
    procedure SetSectionHeader(Value: Boolean);
  protected
    function GetItemViewerClass (AView: TTBView): TTBItemViewerClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateCaption(const Value: TCaption);
  published
    property Caption: TCaption read FCaption write SetCaption;
    property Enabled;
    property FontSettings: TFontSettings read FFontSettings write SetFontSettings;
    property Margin: Integer read FMargin write SetMargin default 0;
    property Orientation: TTBXLabelOrientation read FOrientation write SetOrientation default tbxoAuto;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    {MP}
    property FixedSize: Integer read FFixedSize write SetFixedSize default 0;
    property SectionHeader: Boolean read FSectionHeader write SetSectionHeader default False;
    property Visible;
    property OnAdjustFont: TAdjustFontEvent read FOnAdjustFont write FOnAdjustFont;
  end;

  TTBXLabelItemViewer = class(TTBItemViewer)
  protected
    function  GetCaptionText: string; override;
    function  GetIsHoriz: Boolean; virtual;
    procedure DoAdjustFont(AFont: TFont; StateFlags: Integer); virtual;
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect;
      IsHoverItem, IsPushed, UseDisabledShadow: Boolean); override;
    function  IsToolbarSize: Boolean; override;
  public
    function  IsToolbarStyle: Boolean; override;
  end;

  { TTBXColorItem }

  TTBXColorItem = class(TTBXCustomItem)
  private
    FColor: TColor;
    procedure SetColor(Value: TColor);
  protected
    function GetItemViewerClass (AView: TTBView): TTBItemViewerClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Action;
    property AutoCheck;
    property Caption;
    property Checked;
    property Color: TColor read FColor write SetColor default clWhite;
    property DisplayMode;
    property Enabled;
    property FontSettings;
    property GroupIndex;
    property HelpContext;
    { MP }
    property HelpKeyword;
    property Hint;
    property InheritOptions;
    property MaskOptions;
    property MinHeight;
    property MinWidth;
    property Options;
    { MP }
    property RadioItem;
    property ShortCut;
    property Visible;
    property OnAdjustFont;
    property OnClick;
  end;

  TTBXColorItemViewer = class(TTBXItemViewer)
  protected
    procedure DoPaintCaption(Canvas: TCanvas; const ClientAreaRect: TRect;
      var CaptionRect: TRect; IsTextRotated: Boolean; var PaintDefault: Boolean); override;
    function GetImageShown: Boolean; override;
    function GetImageSize: TSize; override;
    procedure DrawItemImage(Canvas: TCanvas; ARect: TRect; ItemInfo: TTBXItemInfo); override;
  public
    constructor Create(AView: TTBView; AItem: TTBCustomItem; AGroupLevel: Integer); override;
  end;

  {$IFNDEF MPEXCLUDE}
  { TTBXMRUList }

  TTBXMRUList = class(TTBMRUList)
  private
    FKeyShift: Integer;
    procedure SetKeyShift(Value: Integer);
  protected
    function GetFirstKey: Integer; override;
    function GetItemClass: TTBCustomItemClass; override;
    procedure SetItemCaptions; override;
  published
    property KeyShift: Integer read FKeyShift write SetKeyShift default 0;
  end;

  { TTBXMRUListItem }

  TTBXMRUListItem = class(TTBXCustomItem)
  private
    FMRUList: TTBMRUList;
    procedure SetMRUList(Value: TTBMRUList);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MRUList: TTBMRUList read FMRUList write SetMRUList;
  end;
  {$ENDIF}

implementation

uses TBXUtils, TB2Common, TB2Consts, TypInfo, Math, ImgList, {MP}Menus, Forms, PasTools;

const
  { Repeat intervals for spin edit items }
  SE_FIRSTINTERVAL = 400;
  SE_INTERVAL = 100;

type
  TTBViewAccess = class(TTBView);
  TTBItemAccess = class(TTBCustomItem);
  {$IFNDEF MPEXCLUDE}
  TTBMRUListAccess = class(TTBMRUList);
  {$ENDIF}
  TCustomEditAccess = class(TCustomEdit);
  TFontSettingsAccess = class(TFontSettings);

{ Misc. functions }

function StartsText(const ASubText, AText: string): Boolean;
var
  P: PChar;
  L, L2: Integer;
begin
  P := PChar(AText);
  L := Length(ASubText);
  L2 := Length(AText);
  if L > L2 then Result := False
  else Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
    P, L, PChar(ASubText), L) = 2;
end;

//============================================================================//

{ TTBXEdit }

type
  TTBXEdit = class(TEdit)
  private
    FAlignment: TAlignment;
    procedure SetAlignment(Value: TAlignment);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    property Alignment: TAlignment read FAlignment write SetAlignment;
  end;

procedure TTBXEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Cardinal = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or Alignments[FAlignment];
end;

procedure TTBXEdit.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;


//============================================================================//

{ TTBXEditItem }

constructor TTBXEditItem.Create(AOwner: TComponent);
begin
  inherited;
  FEditorFontSettings := TFontSettings.Create;
  FFontSettings := TFontSettings.Create;
  TFontSettingsAccess(FEditorFontSettings).OnChange := FontSettingsChanged;
  TFontSettingsAccess(FFontSettings).OnChange := FontSettingsChanged;
end;

destructor TTBXEditItem.Destroy;
begin
  FFontSettings.Free;
  FEditorFontSettings.Free;
  inherited;
end;

function TTBXEditItem.DoAcceptText(var NewText: string): Boolean;
begin
  Result := inherited DoAcceptText(NewText);
//  if not Result then DoChange(Text);
end;

function TTBXEditItem.DoAutoComplete(var AText: string): Boolean;
begin
  Result := False;
end;

procedure TTBXEditItem.DoBeginEdit(Viewer: TTBEditItemViewer);
begin
  with Viewer do
  begin
    TTBXEdit(EditControl).Alignment := Alignment;
    EditControl.PasswordChar := PasswordChar;
    EditControl.SelectAll;
    EditControl.ReadOnly := ReadOnly;
    EditorFontSettings.Apply(EditControl.Font);
    FAutoCompleteCounter := 0;
    inherited;
    if Viewer is TTBXEditItemViewer then
    begin
      EditControl.OnChange := TTBXEditItemViewer(Viewer).EditChangeHandler;
      TTBXEditItemViewer(Viewer).OldWndProc := EditControl.WindowProc;
      EditControl.WindowProc := TTBXEditItemViewer(Viewer).NewEditWndProc;
    end;
  end;
end;

procedure TTBXEditItem.DoChange(const AText: string);
begin
  if Assigned(FOnChange) then FOnChange(Self, AText);
end;

procedure TTBXEditItem.DoTextChanged(Reason: Integer);
begin
  if not ((Reason = tcrEditControl) and (Text = FLastEditChange)) then
    DoChange(Text);
end;

procedure TTBXEditItem.FontSettingsChanged(Sender: TObject);
begin
  Change(True);
end;

function TTBXEditItem.GetImageIndex: Integer;
begin
  Result := ImageIndex;
end;

function TTBXEditItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  if not (tboUseEditWhenVertical in EditOptions) and
     (AView.Orientation = tbvoVertical) then
    Result := TTBXItemViewer
  else
    Result := TTBXEditItemViewer;
end;

procedure TTBXEditItem.GetPopupPosition(ParentView: TTBView;
  PopupWindow: TTBPopupWindow; var PopupPositionRec: TTBPopupPositionRec);
var
  VT: Integer;
begin
  inherited;
  VT := GetWinViewType(PopupWindow);
  PopupPositionRec.PlaySound := not (VT and PVT_LISTBOX = PVT_LISTBOX);
end;

function TTBXEditItem.GetPopupWindowClass: TTBPopupWindowClass;
begin
  Result := TTBXPopupWindow;
end;

procedure TTBXEditItem.HandleEditChange(Edit: TEdit);
var
  S, S2: string;
begin
  if not FIsChanging then
  begin
    FIsChanging := True;
    try
      S := Edit.Text;
      S2 := S;
      if (Length(S) > 0) and (FAutoCompleteCounter > 0) and DoAutoComplete(S2) then
      begin
        Edit.Text := S2;
        Edit.SelStart := Length(S);
        Edit.SelLength := Length(S2) - Length(S);
        S := S2;
      end;
      if AnsiCompareText(S, FLastEditChange) <> 0 then
      begin
        DoChange(S); // note, Edit.Text may be different from Self.Text
        FLastEditChange := S;
      end;
    finally
      FIsChanging := False;
    end;
  end;
end;

procedure TTBXEditItem.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    Change(True);
  end;
end;

procedure TTBXEditItem.SetFontSettings(Value: TFontSettings);
begin
  FFontSettings.Assign(Value);
end;

procedure TTBXEditItem.SetPasswordChar(Value: Char);
begin
  if Value <> FPasswordChar then
  begin
    FPasswordChar := Value;
    Change(True);
  end;
end;

procedure TTBXEditItem.SetShowImage(const Value: Boolean);
begin
  FShowImage := Value;
  Change(True);
end;

function TTBXEditItem.StartEditing(AView: TTBView): Boolean;
var
  V: TTBItemViewer;
  SaveText: string;
begin
  Result := False;
  V := AView.Find(Self);
  if V is TTBXEditItemViewer then
  begin
    SaveText := Text;
    TTBXEditItemViewer(V).DoExecute;
    Result := Text <> SaveText;
  end;
end;


//============================================================================//

{ TTBXEditItemViewer }

procedure TTBXEditItemViewer.CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer);
var
  W, B: Integer;
  EditBoxHeight: Integer;
  EditCaptionSize: TSize;
begin
  if Self.Item is TTBXEditItem then with CurrentTheme do
  begin
    B := GetIntegerMetrics(Self, TMI_EDIT_FRAMEWIDTH);

    AWidth := TTBXEditItem(Item).EditWidth;
    if not IsToolbarStyle then
    begin
      EditCaptionSize := MeasureEditCaption;
      W := EditCaptionSize.CX;
      if W > 0 then
      begin
        Inc(W,
            GetIntegerMetrics(Self, TMI_MENU_LCAPTIONMARGIN) + GetIntegerMetrics(Self, TMI_MENU_RCAPTIONMARGIN) +
              GetIntegerMetrics(Self, TMI_MENU_IMGTEXTSPACE));
      end;
      Inc(AWidth,
          GetPopupMargin(Self) + GetIntegerMetrics(Self, TMI_MENU_IMGTEXTSPACE) + W +
            GetIntegerMetrics(Self, TMI_EDIT_MENURIGHTINDENT));
    end
    else
    begin
      EditCaptionSize.CX := 0;
      EditCaptionSize.CY := 0;
    end;

    EditBoxHeight := MeasureTextHeight + 1;
    Inc(EditBoxHeight, GetIntegerMetrics(Self, TMI_EDIT_TEXTMARGINVERT) * 2 + B * 2);
    AHeight := Max(EditBoxHeight, EditCaptionSize.CY);
    if not IsToolbarStyle then AHeight := AHeight;
    if EditHeightEven then AHeight := (AHeight + 1) and not $01
    else AHeight := AHeight or $01;
  end
  else inherited;
end;

procedure TTBXEditItemViewer.EditChangeHandler(Sender: TObject);
begin
  HandleEditChange((Sender as TEdit));
end;

procedure TTBXEditItemViewer.HandleEditChange(Edit: TEdit);
begin
  TTBXEditItem(Item).HandleEditChange(Edit);
end;

procedure TTBXEditItemViewer.GetEditInfo(out EditInfo: TTBXEditInfo; const ItemInfo: TTBXItemInfo);
begin
  FillChar(EditInfo, SizeOf(EditInfo), 0);
  EditInfo.LeftBtnWidth := GetIndentBefore;
  EditInfo.RightBtnWidth := GetIndentAfter;
end;

function TTBXEditItemViewer.GetAccRole: Integer;
const
  ROLE_SYSTEM_SPINBUTTON = $34;
  ROLE_SYSTEM_COMBOBOX = $2E;
begin
  Result := inherited GetAccRole;
  {$IFNDEF MPEXCLUDE}
  if Self is TTBXSpinEditViewer then Result := ROLE_SYSTEM_SPINBUTTON
  else {$ENDIF} if Self is TTBXDropDownItemViewer then Result := ROLE_SYSTEM_COMBOBOX;
end;

procedure TTBXEditItemViewer.GetItemInfo(const Canvas: TCanvas; out ItemInfo: TTBXItemInfo; IsHoverItem, IsPushed, UseMenuColor: Boolean);
const
  CToolbarStyle: array [Boolean] of Integer = (0, IO_TOOLBARSTYLE);
  CDesigning: array [Boolean] of Integer = (0, IO_DESIGNING);
var
  Item: TTBXEditItem;
begin
  Item := TTBXEditItem(Self.Item);

  FillChar(ItemInfo, SizeOf(TTBXItemInfo), 0);
  ItemInfo.Control := View.Window;
  ItemInfo.ViewType := GetViewType(View);
  ItemInfo.ItemOptions := CToolbarStyle[IsToolbarStyle]
    or CDesigning[csDesigning in Item.ComponentState];
  ItemInfo.Enabled := Item.Enabled or View.Customizing;
  ItemInfo.Pushed := IsPushed;
  ItemInfo.Selected := Item.Checked;
  if IsHoverItem then
  begin
    if not ItemInfo.Enabled and not View.MouseOverSelected then
      ItemInfo.HoverKind := hkKeyboardHover
    else
      if ItemInfo.Enabled then ItemInfo.HoverKind := hkMouseHover;
  end
  else ItemInfo.HoverKind := hkNone;
  if not IsToolbarStyle then ItemInfo.PopupMargin := GetPopupMargin(Self);
end;

procedure TTBXEditItemViewer.GetEditRect(var R: TRect);
const
  TB2K_EDIT_BORDER = 3;
var
  W, B: Integer;
begin
  if Item is TTBXEditItem then with CurrentTheme do
  begin
    R := BoundsRect;
    if not IsToolbarStyle then
    begin
      W := MeasureEditCaption.CX;
      if W > 0 then
      begin
        Inc(W,
            GetIntegerMetrics(Self, TMI_MENU_LCAPTIONMARGIN) + GetIntegerMetrics(Self, TMI_MENU_RCAPTIONMARGIN) +
              GetIntegerMetrics(Self, TMI_MENU_IMGTEXTSPACE));
      end;
      Inc(R.Left, GetPopupMargin(Self) + GetIntegerMetrics(Self, TMI_MENU_IMGTEXTSPACE) + W);
      Dec(R.Right, GetIntegerMetrics(Self, TMI_EDIT_MENURIGHTINDENT));
    end;

    B := GetIntegerMetrics(Self, TMI_EDIT_FRAMEWIDTH) - TB2K_EDIT_BORDER;
    InflateRect(
      R, -B - GetIntegerMetrics(Self, TMI_EDIT_TEXTMARGINHORZ) , -B - GetIntegerMetrics(Self, TMI_EDIT_TEXTMARGINVERT));
    Inc(R.Left, GetIndentBefore);
    Dec(R.Right, GetIndentAfter);
  end
  else inherited;
end;

function TTBXEditItemViewer.GetIndentAfter: Integer;
begin
  Result := 0;
end;

function TTBXEditItemViewer.GetIndentBefore: Integer;
var
  ImgList: TCustomImageList;
begin
  if ShowImage then
  begin
    ImgList := GetImageList;
    if ImgList <> nil then Result := ImgList.Width + 2
    else Result := 0;
  end
  else Result := 0;
end;

function TTBXEditItemViewer.HandleEditMessage(var Message: TMessage): Boolean;
const
  CharKeys = [VK_SPACE, $30..$5A, VK_NUMPAD0..VK_DIVIDE, $BA..$F5];
begin
  if Message.Msg = WM_KEYDOWN then
  begin
    if Message.WParam in CharKeys then Inc(TTBXEditItem(Item).FAutoCompleteCounter)
  end
  else if Message.Msg = WM_KEYUP then
  begin
    if Message.WParam in CharKeys then Dec(TTBXEditItem(Item).FAutoCompleteCounter);
  end;
  Result := False;
end;

procedure TTBXEditItemViewer.NewEditWndProc(var Message: TMessage);
begin
  if Assigned(OldWndProc) and not HandleEditMessage(Message) then OldWndProc(Message);
end;

{MP}
function TTBXEditItemViewer.StripTextHotkey: Boolean;
begin
  Result := False;
end;

procedure TTBXEditItemViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsHoverItem, IsPushed, UseDisabledShadow: Boolean);
const
  FillColors: array [Boolean] of Integer = (COLOR_BTNFACE, COLOR_WINDOW);
  TextColors: array [Boolean] of Integer = (COLOR_GRAYTEXT, COLOR_WINDOWTEXT);
  Alignments: array [TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  DC: HDC;
  Item: TTBXEditItem;
  S: string;
  R, R2: TRect;
  M, W: Integer;
  ItemInfo: TTBXItemInfo;
  EditInfo: TTBXEditInfo;
  ImgList: TCustomImageList;
  ImgIndex: Integer;
  Fnt, OldFnt: HFont;
  C, OldColor: TColor;
begin
  DC := Canvas.Handle;
  Item := TTBXEditItem(Self.Item);
  GetItemInfo(Canvas, ItemInfo, IsHoverItem, IsPushed, UseDisabledShadow);
  GetEditInfo(EditInfo, ItemInfo);
  R := ClientAreaRect;

  if not IsToolbarStyle then with CurrentTheme do
  begin
    S := Item.EditCaption;

    if Length(S) > 0 then
    begin
      { measure EditCaption }
      Fnt := TTBXEditItem(Item).FontSettings.CreateTransformedFont(TTBViewAccess(View).GetFont.Handle, C);
      OldFnt := SelectObject(DC, Fnt);
      W :=
        GetTextWidth(DC, S, True) +
        GetIntegerMetrics(Self, TMI_MENU_IMGTEXTSPACE) +
        GetIntegerMetrics(Self, TMI_MENU_LCAPTIONMARGIN) +
        GetIntegerMetrics(Self, TMI_MENU_RCAPTIONMARGIN);
      SelectObject(DC, OldFnt);
    end
    else
    begin
      Fnt := 0; // to suppress compiler warning
      W := 0;
    end;

    M := GetPopupMargin(Self);
    if not EditMenuFullSelect then R.Right := M + W
    else Dec(R.Right, GetIntegerMetrics(Self, TMI_EDIT_MENURIGHTINDENT));
    PaintMenuItemFrame(Canvas, R, ItemInfo);
    Inc(R.Left, M + GetIntegerMetrics(Self, TMI_MENU_IMGTEXTSPACE));
    R.Right := ClientAreaRect.Right - GetIntegerMetrics(Self, TMI_EDIT_MENURIGHTINDENT);

    if Length(S) > 0 then
    begin
      Inc(R.Left, GetIntegerMetrics(Self, TMI_MENU_LCAPTIONMARGIN));
      C := ColorToRGB(GetItemTextColor(ItemInfo));
      OldFnt := SelectObject(DC, Fnt);
      OldColor := SetTextColor(DC, C);
      PaintCaption(Canvas, R, ItemInfo, S, DT_SINGLELINE or DT_LEFT or DT_VCENTER, False);
      SetTextColor(DC, OldColor);
      W := GetTextWidth(DC, S, True);
      SelectObject(DC, OldFnt);
      DeleteObject(Fnt);
      Inc(R.Left,
          W + GetIntegerMetrics(Self, TMI_MENU_RCAPTIONMARGIN) + GetIntegerMetrics(Self, TMI_MENU_IMGTEXTSPACE));
    end;
  end;

  CurrentTheme.PaintEditFrame(View.GetMonitor, Canvas, R, ItemInfo, EditInfo);
  W := CurrentTheme.GetIntegerMetrics(Self, TMI_EDIT_FRAMEWIDTH);
  InflateRect(
    R, -W - CurrentTheme.GetIntegerMetrics(Self, TMI_EDIT_TEXTMARGINHORZ),
    -W - CurrentTheme.GetIntegerMetrics(Self, TMI_EDIT_TEXTMARGINVERT));

  if ShowImage then
  begin
    ImgList := GetImageList;
    if ImgList <> nil then
    begin
      R2.Left := R.Left;
      R2.Right := R.Left + ImgList.Width;
      R2.Top := (R.Top + R.Bottom + 1 - ImgList.Height) div 2;
      R2.Bottom := R2.Top + ImgList.Height;
      ImgIndex := TTBXEditItem(Item).GetImageIndex;
      if Item.Enabled then ImgList.Draw(Canvas, R.Left, R2.Top, ImgIndex)
      else DrawTBXImage(Canvas, R2, ImgList, ImgIndex, ISF_DISABLED);
    end;
  end;
  Inc(R.Left, EditInfo.LeftBtnWidth);
  Dec(R.Right, EditInfo.RightBtnWidth + 1);

  if Item.Text <> '' then
  begin
    S := Item.Text;
    if StripTextHotkey then S := StripHotkey(S);
    if TTBXEditItem(Item).PasswordChar <> #0 then S := StringOfChar(TTBXEditItem(Item).PasswordChar, Length(S));
    Fnt := Item.EditorFontSettings.CreateTransformedFont(TTBViewAccess(View).GetFont.Handle, C);
    OldFnt := SelectObject(DC, Fnt);
    SetBkMode(DC, TRANSPARENT);
    SetBkColor(DC, GetSysColor(FillColors[Item.Enabled]));
    SetTextColor(DC, GetSysColor(TextColors[Item.Enabled]));
    // WinSCP: Align edit text with toolbar labels
    InflateRect(R, 0, -1);
    DrawText(DC, PChar(S), Length(S), R, DT_SINGLELINE or DT_NOPREFIX or Alignments[Item.Alignment]);
    SelectObject(DC, OldFnt);
    DeleteObject(Fnt);
  end;

{  if not IsToolbarStyle then
  begin
    R := ClientAreaRect;
    Self.GetEditRect(R);
    OffsetRect(R, -BoundsRect.Left, -BoundsRect.Top);
    Canvas.FrameRect(R);
  end;  }
end;

function TTBXEditItemViewer.GetEditControlClass: TEditClass;
begin
  Result := TTBXEdit;
end;

function TTBXEditItemViewer.ShowImage: Boolean;
begin
  Result := TTBXEditItem(Item).ShowImage;
end;

function TTBXEditItemViewer.IsToolbarSize: Boolean;
begin
  Result := inherited IsToolbarSize;
  Result := Result or ((GetViewType(View) and PVT_TOOLBOX) = PVT_TOOLBOX);
end;

function TTBXEditItemViewer.IsToolbarStyle: Boolean;
begin
  Result := inherited IsToolbarStyle;
  Result := Result or ((GetViewType(View) and PVT_TOOLBOX) = PVT_TOOLBOX);
end;

function TTBXEditItemViewer.MeasureEditCaption: TSize;
var
  DC: HDC;
  Fnt, OldFnt: HFont;
  DummyColor: TColor;
  TextMetric: TTextMetric;
  S: string;
begin
  Result.cx := 0;
  Result.cy := 0;
  if Item is TTBXEditItem then
  begin
    S := StripAccelChars(TTBXEditItem(Item).EditCaption);
    if Length(S) > 0 then
    begin
      DummyColor := clWhite;
      DC := GetDC(0);
      Fnt := TTBXEditItem(Item).FontSettings.CreateTransformedFont(TTBViewAccess(View).GetFont.Handle, DummyColor);
      OldFnt := SelectObject(DC, Fnt);
      GetTextExtentPoint32(DC, PChar(S), Length(S), Result);
      GetTextMetrics(DC, TextMetric);
      Inc(Result.cy, TextMetric.tmExternalLeading);
      SelectObject(DC, OldFnt);
      DeleteObject(Fnt);
      ReleaseDC(0, DC);
    end;
  end;
end;

function TTBXEditItemViewer.MeasureTextHeight: Integer;
var
  DC: HDC;
  Fnt, OldFnt: HFont;
  DummyColor: TColor;
  TextMetric: TTextMetric;
begin
  Result := 0;
  if Item is TTBXEditItem then
  begin
    DummyColor := clWhite;
    DC := GetDC(0);
    Fnt := TTBXEditItem(Item).EditorFontSettings.CreateTransformedFont(TTBViewAccess(View).GetFont.Handle, DummyColor);
    OldFnt := SelectObject(DC, Fnt);
    Result := GetTextHeight(DC);
    GetTextMetrics(DC, TextMetric);
    Inc(Result, TextMetric.tmExternalLeading);
    SelectObject(DC, OldFnt);
    DeleteObject(Fnt);
    ReleaseDC(0, DC);
  end;
end;

function TTBXEditItemViewer.DoExecute: Boolean;
begin
  if Item is TTBXEditItem then
  begin
    TTBXEditItem(Item).FLastEditChange := '';
    Result := inherited DoExecute;
    with TTBXEditItem(Item) do
    begin
      if FLastEditChange <> Text then DoChange(Text);
      FLastEditChange := '';
    end;
  end
  else Result := inherited DoExecute;
end;


//============================================================================//

{MP}
type
  TTBXDropDownWindow = class(TTBXPopupWindow)
  protected
    procedure Cancel; override;
  public
    Owner: TTBXCustomDropDownItem;
  end;

procedure TTBXDropDownWindow.Cancel;
begin
  inherited;
  Owner.DoCancel;
end;
{/MP}

//============================================================================//

{ TTBXCustomDropDownItem }

constructor TTBXCustomDropDownItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle + [tbisCombo, tbisSubmenu, tbisSubitemsEditable] - [tbisDontSelectFirst];
  FAlwaysSelectFirst := True;
end;

function TTBXCustomDropDownItem.CreatePopup(const ParentView: TTBView;
  const ParentViewer: TTBItemViewer; const PositionAsSubmenu,
  SelectFirstItem, Customizing: Boolean; const APopupPoint: TPoint;
  const Alignment: TTBPopupAlignment): TTBPopupWindow;
var
  SelectFirst: Boolean;
begin
  if AlwaysSelectFirst then SelectFirst := True
  else SelectFirst := SelectFirstItem;
  Result := inherited CreatePopup(ParentView, ParentViewer, PositionAsSubmenu,
    SelectFirst, Customizing, APopupPoint, Alignment);
  {MP}
  (Result as TTBXDropDownWindow).Owner := Self;
end;

{MP}
procedure TTBXCustomDropDownItem.DoCancel;
begin
  if Assigned(OnCancel) then
    OnCancel(Self);
end;

function TTBXCustomDropDownItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  if not (tboUseEditWhenVertical in EditOptions) and (AView.Orientation = tbvoVertical) then
    Result := TTBXItemViewer
  else
    Result := TTBXDropDownItemViewer;
end;

{MP}
function TTBXCustomDropDownItem.GetPopupWindowClass: TTBPopupWindowClass;
begin
  Result := TTBXDropDownWindow;
end;

//----------------------------------------------------------------------------//

{ TTBXDropDownItemViewer }

procedure TTBXDropDownItemViewer.GetCursor(const Pt: TPoint; var ACursor: HCURSOR);
begin
  if not TTBXCustomDropDownItem(Item).DropDownList then inherited;
end;

procedure TTBXDropDownItemViewer.GetEditInfo(out EditInfo: TTBXEditInfo; const ItemInfo: TTBXItemInfo);
const
  CDisabled: array [Boolean] of Integer = (EBDS_DISABLED, 0);
  CHot: array [Boolean] of Integer = (0, EBDS_HOT);
  CPressed: array [Boolean] of Integer = (0, EBDS_PRESSED);
begin
  inherited GetEditInfo(EditInfo, ItemInfo);
  EditInfo.RightBtnInfo.ButtonType := EBT_DROPDOWN;
  EditInfo.RightBtnInfo.ButtonState := CDisabled[ItemInfo.Enabled] or
    CHot[ItemInfo.HoverKind = hkMouseHover] or CPressed[ItemInfo.Pushed];
end;

function TTBXDropDownItemViewer.GetIndentAfter: Integer;
begin
  if IsToolbarStyle then Result := CurrentTheme.GetIntegerMetrics(Self, TMI_EDIT_BTNWIDTH)
  else Result := GetSystemMetricsForControl(View.Window, SM_CXMENUCHECK) + 2;
end;

function TTBXDropDownItemViewer.HandleEditMessage(var Message: TMessage): Boolean;
begin
  if Message.Msg = WM_KEYDOWN then
  begin
    if TWMKeyDown(Message).CharCode = VK_F4 then
    begin
      if (View.OpenViewer = Self) // WasAlreadyOpen
        then View.CloseChildPopups
        else View.OpenChildPopup(True);
      Result := True;
      Exit;
    end;
  end;

  Result := inherited HandleEditMessage(Message);
end;

function TTBXDropDownItemViewer.IsPtInButtonPart(X, Y: Integer): Boolean;
begin
  Result := not (tbisSubmenu in TTBXCustomDropDownItem(Item).ItemStyle);
  if TTBXCustomDropDownItem(Item).DropDownList then Result := False
  else if (tbisCombo in TTBXCustomDropDownItem(Item).ItemStyle) then
    Result := X < (BoundsRect.Right - BoundsRect.Left) - GetIndentAfter;
end;

procedure TTBXDropDownItemViewer.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not TTBXCustomDropDownItem(Item).DropDownList then inherited;
end;

//============================================================================//

{ TTBXComboBoxItem }

procedure TTBXComboBoxItem.AdjustImageIndex(const AText: string;
  AIndex: Integer; var ImageIndex: Integer);
begin
  if Assigned(FOnAdjustImageIndex) then FOnAdjustImageIndex(Self, AText, AIndex, ImageIndex);
end;

procedure TTBXComboBoxItem.AdjustImageIndexHandler(Sender: TTBXCustomList;
  AItemIndex: Integer; var ImageIndex: Integer);
begin
  AdjustImageIndex(FList.Strings[AItemIndex], AItemIndex, ImageIndex);
end;

constructor TTBXComboBoxItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle - [tbisSubItemsEditable];
  FAutoComplete := True;
  FList := GetStringListClass.Create(Self);
  FList.OnChange := ListChangeHandler;
  FList.OnClick := ListClickHandler;
  FList.OnAdjustImageIndex := AdjustImageIndexHandler;
  MinListWidth := 64;
end;

function TTBXComboBoxItem.DoAutoComplete(var AText: string): Boolean;
var
  I: Integer;
  S, R: string;
  TemplateL, MinL, L: Integer;
begin
  Result := False;
  if Length(AText) > 0 then
  begin
    { choose the shortest matching string from items }
    TemplateL := Length(AText);
    MinL := MaxInt;
    SetLength(R, 0);
    for I := 0 to FList.Strings.Count - 1 do
    begin
      S := FList.Strings[I];
      L := Length(S);
      if (L >= TemplateL) and (L < MinL) and StartsText(AText, S) then
      begin
        R := S;
        MinL := L;
        if MinL = TemplateL then Break;
      end;
    end;
    Result := Length(R) > 0;
    if Result then AText := R;
  end;
end;

procedure TTBXComboBoxItem.DoListChange;
begin
  { Update text in edit item. This will call OnChange automatically }
  if (FList.ItemIndex >= 0) and (FList.ItemIndex < FList.Strings.Count) then
  begin
    IsChanging := True;
    try
      if Text <> FList.Strings[Flist.ItemIndex] then
      begin
        SetTextEx(FList.Strings[FList.ItemIndex], tcrList);
      end;
    finally
      IsChanging := False;
    end;
  end;
end;

procedure TTBXComboBoxItem.DoListClick;
begin
  if Assigned(FOnItemClick) then FOnItemClick(Self);
end;

procedure TTBXComboBoxItem.DoPopup(Sender: TTBCustomItem; FromLink: Boolean);
begin
  inherited;
  FList.ItemIndex := FList.Strings.IndexOf(Text);
end;

function TTBXComboBoxItem.GetImageIndex: Integer;
begin
  if not CacheValid then
  begin
    CachedImageIndex := ImageIndex;
    if ItemIndex >= 0 then CachedImageIndex := ItemIndex;
    AdjustImageIndex(Text, -1, CachedImageIndex);
    CacheValid := True;
  end;
  Result := CachedImageIndex;
end;

function TTBXComboBoxItem.GetItemIndex: Integer;
begin
  Result := FList.ItemIndex;
end;

function TTBXComboBoxItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  if not (tboUseEditWhenVertical in EditOptions) and
     (AView.Orientation = tbvoVertical) then
    Result := TTBXItemViewer
  else
    Result := TTBXComboBoxItemViewer;
end;

function TTBXComboBoxItem.GetMaxVisibleItems: Integer;
begin
  Result := FList.MaxVisibleItems;
end;

function TTBXComboBoxItem.GetMaxWidth: Integer;
begin
  Result := FList.MaxWidth;
end;

function TTBXComboBoxItem.GetMinWidth: Integer;
begin
  Result := FList.MinWidth;
end;

function TTBXComboBoxItem.GetOnClearItem: TTBXLPaintEvent;
begin
  Result := FList.OnClearItem;
end;

function TTBXComboBoxItem.GetOnDrawItem: TTBXLPaintEvent;
begin
  Result := FList.OnDrawItem;
end;

function TTBXComboBoxItem.GetOnMeasureHeight: TTBXLMeasureHeight;
begin
  Result := FList.OnMeasureHeight;
end;

function TTBXComboBoxItem.GetOnMeasureWidth: TTBXLMeasureWidth;
begin
  Result := FList.OnMeasureWidth;
end;

function TTBXComboBoxItem.GetShowListImages: Boolean;
begin
  Result := FList.ShowImages;
end;

function TTBXComboBoxItem.GetStringListClass: TTBXStringListClass;
begin
  Result := TTBXStringList;
end;

function TTBXComboBoxItem.GetStrings: TStrings;
begin
  Result := FList.Strings;
end;

procedure TTBXComboBoxItem.HandleEditChange(Edit: TEdit);
begin
  CacheValid := False;
  inherited;
end;

procedure TTBXComboBoxItem.ListChangeHandler(Sender: TObject);
begin
  CacheValid := False;
  DoListChange;
end;

procedure TTBXComboBoxItem.ListClickHandler(Sender: TObject);
begin
  CacheValid := False;
  DoListClick;
end;

procedure TTBXComboBoxItem.Loaded;
begin
  inherited;
  if FList.Strings.IndexOf(Text) >= 0 then
  begin
    IsChanging := True;
    try
      FList.ItemIndex := FList.Strings.IndexOf(Text);
    finally
      IsChanging := False;
    end;
  end;
  { MP Do not re-add on re-load (locale change) }
  if not Assigned(FList.Parent) then
    if not (csDesigning in ComponentState) then Add(FList);
end;

procedure TTBXComboBoxItem.SetItemIndex(Value: Integer);
begin
  FList.ItemIndex := Value;
end;

procedure TTBXComboBoxItem.SetMaxVisibleItems(Value: Integer);
begin
  FList.MaxVisibleItems := Value;
end;

procedure TTBXComboBoxItem.SetMaxWidth(Value: Integer);
begin
  FList.MaxWidth := Value;
end;

procedure TTBXComboBoxItem.SetMinWidth(Value: Integer);
begin
  FList.MinWidth := Value;
end;

procedure TTBXComboBoxItem.SetOnClearItem(Value: TTBXLPaintEvent);
begin
  FList.OnClearItem := Value;
end;

procedure TTBXComboBoxItem.SetOnDrawItem(Value: TTBXLPaintEvent);
begin
  FList.OnDrawItem := Value;
end;

procedure TTBXComboBoxItem.SetOnMeasureHeight(Value: TTBXLMeasureHeight);
begin
  FList.OnMeasureHeight := Value;
end;

procedure TTBXComboBoxItem.SetOnMeasureWidth(Value: TTBXLMeasureWidth);
begin
  FList.OnMeasureWidth := Value;
end;

procedure TTBXComboBoxItem.SetShowListImages(Value: Boolean);
begin
  FList.ShowImages := Value;
end;

procedure TTBXComboBoxItem.SetStrings(Value: TStrings);
begin
  FList.Strings := Value;
end;

procedure TTBXComboBoxItem.ChangeScale(M, D: Integer);
begin
  inherited;
  MinListWidth := MulDiv(MinListWidth, M, D);
  MaxListWidth := MulDiv(MaxListWidth, M, D);
end;


//============================================================================//

{ TTBXComboBoxItemViewer }

function TTBXComboBoxItemViewer.HandleEditMessage(var Message: TMessage): Boolean;
begin
  if (Message.Msg = WM_KEYDOWN) then with TTBXComboBoxItem(Item) do
  begin
    case Message.wParam of
      VK_UP:
        begin
          if ItemIndex > 0 then
            ItemIndex := ItemIndex- 1;
          EditControl.Text := Text;
          EditControl.SelectAll;
          Result := True;
        end;

      VK_DOWN:
        begin
          if ItemIndex < Strings.Count- 1 then
            ItemIndex := ItemIndex+ 1;
          EditControl.Text := Text;
          EditControl.SelectAll;
          Result := True;
        end;
    else
      Result := inherited HandleEditMessage(Message);
    end
  end
  else Result := inherited HandleEditMessage(Message);
end;

{MP}
function TTBXComboBoxItemViewer.StripTextHotkey: Boolean;
begin
  Result := TTBXComboBoxItem(Item).DropDownList;
end;

//============================================================================//

{ TTBXLabelItem }

constructor TTBXLabelItem.Create(AOwner: TComponent);
begin
  inherited;
  FFontSettings := TFontSettings.Create;
  TFontSettingsAccess(FFontSettings).OnChange := FontSettingsChanged;
  FShowAccelChar := True;
  ItemStyle := ItemStyle - [tbisSelectable] + [tbisClicksTransparent, tbisStretch];
  FSectionHeader := False;
end;

destructor TTBXLabelItem.Destroy;
begin
  FFontSettings.Free;
  inherited;
end;

procedure TTBXLabelItem.FontSettingsChanged(Sender: TObject);
begin
  Change(True);
end;

function TTBXLabelItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TTBXLabelItemViewer;
end;

procedure TTBXLabelItem.SetCaption(const Value: TCaption);
begin
  FCaption := Value;
  Change(True);
end;

procedure TTBXLabelItem.SetFontSettings(Value: TFontSettings);
begin
  FFontSettings := Value;
end;

{procedure TTBXLabelItem.SetFontSize(Value: TTBXFontSize);
begin
  FFontSize := Value;
  Change(True);
end;  }

procedure TTBXLabelItem.SetMargin(Value: Integer);
begin
  FMargin := Value;
  Change(True);
end;

procedure TTBXLabelItem.SetOrientation(Value: TTBXLabelOrientation);
begin
  FOrientation := Value;
  Change(True);
end;

procedure TTBXLabelItem.SetShowAccelChar(Value: Boolean);
begin
  FShowAccelChar := Value;
  Change(True);
end;

{MP}
procedure TTBXLabelItem.SetFixedSize(Value: Integer);
begin
  FFixedSize := Value;
  Change(True);
end;

procedure TTBXLabelItem.SetSectionHeader(Value: Boolean);
begin
  FSectionHeader := Value;
  Change(True);
end;

procedure TTBXLabelItem.UpdateCaption(const Value: TCaption);
begin
  FCaption := Value;
  Change(False);
end;


//============================================================================//

{ TTBXLabelItemViewer }

procedure TTBXLabelItemViewer.CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer);
var
  DC: HDC;
  S: string;
  TextMetrics: TTextMetric;
  RotatedFont, SaveFont: HFont;
  Margins: TTBXMargins;
  ImgList: TCustomImageList;
  ImgHeight: Integer;
begin
  Canvas.Font := TTBViewAccess(View).GetFont;
  DoAdjustFont(Canvas.Font, 0);
  S := GetCaptionText;
  if Length(S) = 0 then S := '0';
  DC := Canvas.Handle;
  if IsToolbarStyle then
  begin
    AWidth := TTBXLabelItem(Item).Margin;
    AHeight := AWidth;
    if Length(S) > 0 then
    begin
      if GetIsHoriz then
      begin
        GetTextMetrics(DC, TextMetrics);
        Inc(AHeight, TextMetrics.tmHeight);
        Inc(AWidth, GetTextWidth(DC, S, TTBXLabelItem(Item).ShowAccelChar));
      end
      else
      begin
        RotatedFont := CreateRotatedFont(DC);
        SaveFont := SelectObject(DC, RotatedFont);
        GetTextMetrics(DC, TextMetrics);
        Inc(AWidth, TextMetrics.tmHeight);
        Inc(AHeight, GetTextWidth(DC, S, TTBXLabelItem(Item).ShowAccelChar));
        SelectObject(DC, SaveFont);
        DeleteObject(RotatedFont);
      end;
    end;

    {MP}
    with TTBXLabelItem(Item) do
      if FFixedSize > 0 then
        if GetIsHoriz then
          AWidth := FFixedSize
        else
          AHeight := FFixedSize
  end
  else
  begin
    if Length(S) > 0 then
    begin
      GetTextMetrics(DC, TextMetrics);
      AHeight := TextMetrics.tmHeight;
      AWidth := GetTextWidth(DC, S, TTBXLabelItem(Item).ShowAccelChar);
    end;

    {MP}
    with TTBXLabelItem(Item) do
    begin
      if FFixedSize > 0 then
        AWidth := FFixedSize;

      if SectionHeader then
      begin
        // the same as regular menu item
        CurrentTheme.GetMargins(MID_MENUITEM, Margins);

        Inc(AWidth, Margins.LeftWidth + Margins.RightWidth);
        Inc(AWidth,
          GetPopupMargin(Self) + CurrentTheme.GetIntegerMetrics(Self, TMI_MENU_IMGTEXTSPACE) +
          CurrentTheme.GetIntegerMetrics(Self, TMI_MENU_LCAPTIONMARGIN) +
          CurrentTheme.GetIntegerMetrics(Self, TMI_MENU_RCAPTIONMARGIN));
        // + make sure it's always bit indented compared to menu items
        Inc(AWidth, 2 * 8);

        ImgHeight := 16;
        ImgList := GetImageList;
        if ImgList <> nil then ImgHeight := ImgList.Height;
        if AHeight < ImgHeight then AHeight := ImgHeight;
        Inc(AHeight, Margins.TopHeight + Margins.BottomHeight);

        Inc(AWidth, AHeight); { Note: maybe this should be controlled by the theme }
      end;
    end;
  end;

  if AWidth < 6 then AWidth := 6;
  if AHeight < 6 then AHeight := 6;
  with TTBXLabelItem(Item) do
  begin
    Inc(AWidth, Margin shl 1 + 1);
    Inc(AHeight, Margin shl 1 + 1);
  end;
end;

procedure TTBXLabelItemViewer.DoAdjustFont(AFont: TFont; StateFlags: Integer);
begin
  if Item is TTBXLabelItem then
    with TTBXLabelItem(Item) do
    begin
      FontSettings.Apply(AFont);
      if Assigned(FOnAdjustFont) then FOnAdjustFont(Item, Self, AFont, StateFlags);
    end;
end;

function TTBXLabelItemViewer.GetCaptionText: string;
var
  P: Integer;
begin
  Result := TTBXLabelItem(Item).Caption;
  P := Pos(#9, Result);
  if P <> 0 then SetLength(Result, P - 1);
end;

function TTBXLabelItemViewer.GetIsHoriz: Boolean;
begin
  with TTBXLabelItem(Item) do
   case Orientation of
     tbxoHorizontal: Result := True;
     tbxoVertical: Result := False;
   else // tbxoAuto
     Result := View.Orientation <> tbvoVertical;
   end;
end;

function TTBXLabelItemViewer.IsToolbarSize: Boolean;
begin
  Result := inherited IsToolbarSize;
  Result := Result or ((GetViewType(View) and PVT_TOOLBOX) = PVT_TOOLBOX);
end;

function TTBXLabelItemViewer.IsToolbarStyle: Boolean;
begin
  Result := inherited IsToolbarStyle;
  Result := Result or ((GetViewType(View) and PVT_TOOLBOX) = PVT_TOOLBOX);
end;

procedure TTBXLabelItemViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsHoverItem, IsPushed, UseDisabledShadow: Boolean);
const
  CEnabledStates: array [Boolean] of Integer = (ISF_DISABLED, 0);
  CDesigning: array [Boolean] of Integer = (0, IO_DESIGNING);
  CPrefixes: array [Boolean] of Integer = (DT_NOPREFIX, 0);
var
  Fmt: Cardinal;
  ItemInfo: TTBXItemInfo;
  R: TRect;
begin
  FillChar(ItemInfo, SizeOf(ItemInfo), 0);
  ItemInfo.Control := View.Window;
  ItemInfo.ViewType := GetViewType(View);
  ItemInfo.ItemOptions := IO_TOOLBARSTYLE or CDesigning[csDesigning in Item.ComponentState];
  ItemInfo.Enabled := Item.Enabled or View.Customizing;
  ItemInfo.Pushed := False;
  ItemInfo.Selected := False;
  ItemInfo.ImageShown := False;
  ItemInfo.ImageWidth := 0;
  ItemInfo.ImageHeight := 0;
  ItemInfo.HoverKind := hkNone;
  ItemInfo.IsPopupParent := False;
  ItemInfo.IsVertical := not GetIsHoriz;

  Canvas.Font := TTBViewAccess(View).GetFont;
  Canvas.Font.Color := CurrentTheme.GetItemTextColor(ItemInfo);
  DoAdjustFont(Canvas.Font, CEnabledStates[ItemInfo.Enabled]);
  Fmt := DT_SINGLELINE or DT_CENTER or DT_VCENTER or CPrefixes[TTBXLabelItem(Item).ShowAccelChar];
  R := ClientAreaRect;
  if TTBXLabelItem(Item).SectionHeader and (not IsToolbarStyle) then
  begin
    ItemInfo.PopupMargin := GetPopupMargin(Self);
    CurrentTheme.PaintMenuItem(Canvas, R, ItemInfo);
    Inc(R.Left, ItemInfo.PopupMargin + CurrentTheme.GetIntegerMetrics(Self, TMI_MENU_LCAPTIONMARGIN) - 1);
    Canvas.Brush.Color := CurrentTheme.GetViewColor(VT_SECTIONHEADER);
    Canvas.FillRect(R);
    Assert(not ItemInfo.IsVertical);
    Windows.DrawText(Canvas.Handle, PChar(GetCaptionText), Length(GetCaptionText), R, Fmt)
  end
    else
  begin
    Canvas.Brush.Style := bsClear;
    CurrentTheme.PaintCaption(Canvas, R, ItemInfo, GetCaptionText, Fmt, ItemInfo.IsVertical);
  end;

  Canvas.Brush.Style := bsSolid;
end;


//============================================================================//

{ TTBXColorItem }

constructor TTBXColorItem.Create(AOwner: TComponent);
begin
  inherited;
  FColor := clWhite;
end;

function TTBXColorItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TTBXColorItemViewer;
end;

procedure TTBXColorItem.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Change(False);
  end;
end;


//============================================================================//

{ TTBXColorItemViewer }

procedure TTBXColorItemViewer.DrawItemImage(Canvas: TCanvas; ARect: TRect; ItemInfo: TTBXItemInfo);
begin
  with ItemInfo, Canvas do
  begin
    if TTBXColorItem(Item).Color <> clNone then
    begin
      if ((ItemOptions and IO_TOOLBARSTYLE) = 0) then InflateRect(ARect, -2, -2);

      if Enabled then
      begin
        Brush.Color := clBtnShadow;
        FrameRect(ARect);
        InflateRect(ARect, -1, -1);
        Brush.Color := TTBXColorItem(Item).Color;
        FillRect(ARect);
      end
        else
      begin
        Inc(ARect.Right);
        Inc(ARect.Bottom);
        DrawEdge(Handle, ARect, BDR_SUNKENOUTER or BDR_RAISEDINNER, BF_RECT);
      end;
    end;
  end;
end;

procedure TTBXColorItemViewer.DoPaintCaption(Canvas: TCanvas;
  const ClientAreaRect: TRect; var CaptionRect: TRect;
  IsTextRotated: Boolean; var PaintDefault: Boolean);
begin
  if (GetViewType(View) and PVT_TOOLBOX) = PVT_TOOLBOX then
  begin
    { Center Caption }
    OffsetRect(CaptionRect, -CaptionRect.Left, 0);
    OffsetRect(CaptionRect, (ClientAreaRect.Right - CaptionRect.Right) div 2, 0);
  end;
end;

function TTBXColorItemViewer.GetImageSize: TSize;
var
  ImgList: TCustomImageList;
  Size: Integer;
begin
  ImgList := GetImageList;
  if ImgList <> nil then
  begin
    Result.CX := ImgList.Width;
    Result.CY := ImgList.Height;
    if IsToolbarStyle then
    begin
      // we want to get 12x12 with 16x16 images,
      // to match the imagelist-less branch below
      Result.CX := MulDiv(Result.CX, 12, 16);
      Result.CY := MulDiv(Result.CY, 12, 16);
    end;
  end
    else
  begin
    // we do not want to get here
    Assert(False);
    if IsToolbarStyle then
    begin
      Size := 12;
    end
    else
    begin
      Size := 16;
    end;
    // do not have a canvas here to scale by text height
    Size := ScaleByPixelsPerInch(Size, View.GetMonitor);
    Result.CX := Size;
    Result.CY := Size;
  end;
end;

function TTBXColorItemViewer.GetImageShown: Boolean;
begin
  Result := ((Item.DisplayMode in [nbdmDefault, nbdmImageAndText]) or
    (IsToolbarStyle and (Item.DisplayMode = nbdmTextOnlyInMenus)));
end;

constructor TTBXColorItemViewer.Create(AView: TTBView; AItem: TTBCustomItem; AGroupLevel: Integer);
begin
  inherited;
  Wide := False;
end;

//============================================================================//

{$IFNDEF MPEXCLUDE}

{ TTBXMRUList }

function TTBXMRUList.GetFirstKey:Integer;
begin
  Result := FKeyShift;
end;

function TTBXMRUList.GetItemClass: TTBCustomItemClass;
begin
  Result := TTBXCustomItem;
end;

procedure TTBXMRUList.SetItemCaptions;
var
  I: Integer;
begin
  inherited;
  if Container is TTBXCustomItem then
    for I := 0 to Items.Count - 1 do
      TTBXCustomItem(Items[I]).FontSettings := TTBXCustomItem(Container).FontSettings;
end;

procedure TTBXMRUList.SetKeyShift(Value: Integer);
begin
  if Value < 0 then Value := 0;
  FKeyShift := Value;
  SetItemCaptions;
end;


//============================================================================//

{ TTBXMRUListItem }

constructor TTBXMRUListItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle + [tbisEmbeddedGroup];
  Caption := STBMRUListItemDefCaption[1] + 'TBX ' +
    Copy(STBMRUListItemDefCaption, 2, Length(STBMRUListItemDefCaption) - 1);
end;

procedure TTBXMRUListItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FMRUList) and (Operation = opRemove) then MRUList := nil;
end;

procedure TTBXMRUListItem.SetMRUList(Value: TTBMRUList);
begin
  if FMRUList <> Value then
  begin
    FMRUList := Value;
    if Assigned(Value) then
    begin
      Value.FreeNotification(Self);
      LinkSubitems := TTBMRUListAccess(Value).Container;
    end
    else LinkSubitems := nil;
  end;
end;

{ TTBXCustomSpinEditItem }

function TTBXCustomSpinEditItem.CheckValue(const V: Extended): Extended;
begin
  Result := V;
  if FMaxValue <> FMinValue then
  begin
    if V < FMinValue then Result := FMinValue
    else if V > FMaxValue then Result := FMaxValue;
  end;
end;

procedure TTBXCustomSpinEditItem.ClickDown;
var
  OldValue, NewValue: Extended;
begin
  OldValue := GetValue;
  if Snap then
    NewValue := Ceil(OldValue / Increment - 1 - Increment * 0.0001) * Increment
  else
    NewValue := OldValue - FIncrement;
  DoStep(-1, OldValue, NewValue);
  SetValueEx(NewValue, tcrSpinButton);
end;

procedure TTBXCustomSpinEditItem.ClickUp;
var
  OldValue, NewValue: Extended;
begin
  OldValue := GetValue;
  if Snap then
    NewValue := Floor(OldValue / Increment + 1 + Increment * 0.0001) * Increment
  else
    NewValue := OldValue + FIncrement;
  DoStep(+1, OldValue, NewValue);
  SetValueEx(NewValue, tcrSpinButton);
end;

constructor TTBXCustomSpinEditItem.Create(AOwner: TComponent);
begin
  inherited;
  FAlignment := taRightJustify;
  FDecimal := 2;
  FIncrement := 1;
  FSnap := True;
  Text := '0';
end;

function TTBXCustomSpinEditItem.DoAcceptText(var NewText: string): Boolean;
var
  V: Extended;
begin
  if ParseValue(NewText, V) then
  begin
    NewText := GetAsText(V);
    Result := True;
  end
  else Result := False;
end;

function TTBXCustomSpinEditItem.DoConvert(const APrefix, APostfix: string; var AValue: Extended): Boolean;
begin
  Result := True;
  if Assigned(FOnConvert) then FOnConvert(Self, APrefix, APostfix, AValue, Result);
end;

procedure TTBXCustomSpinEditItem.DoStep(Step: Integer; const OldValue: Extended; var NewValue: Extended);
begin
  if Assigned(FOnStep) then FOnStep(Self, Step, OldValue, NewValue);
end;

procedure TTBXCustomSpinEditItem.DoTextChanged(Reason: Integer);
begin
  if Reason = tcrEditControl then
    SetValueEx(GetValue, tcrNumericProperty);
end;

function TTBXCustomSpinEditItem.DoTextToValue(const AText: string; out AValue: Extended): Boolean;
begin
  Result := False;
  if Assigned(FOnTextToValue) then FOnTextToValue(Self, AText, AValue, Result);
end;

procedure TTBXCustomSpinEditItem.DoValueChange(const V: Extended);
begin
  if Assigned(FOnValueChange) then FOnValueChange(Self, V);
end;

procedure TTBXCustomSpinEditItem.DoValueToText(const NewValue: Extended; var NewText: string);
begin
  if Assigned(FOnValueToText) then FOnValueToText(Self, NewValue, NewText);
end;

function TTBXCustomSpinEditItem.GetAsInteger: Integer;
begin
  Result := Round(Value);
end;

function TTBXCustomSpinEditItem.GetAsText(AValue: Extended): string;
begin
  AValue := CheckValue(AValue);
  if ValueType = evtFloat then Result := FloatToStrF(AValue, ffFixed, 15, FDecimal)
  else if ValueType = evtHex then Result := IntToHex(Round(AValue), 1)
  else Result := IntToStr(Round(AValue));

  if Length(Prefix) > 0 then
  begin
    if SpaceAfterPrefix then Result := ' ' + Result;
    Result := Prefix + Result;
  end;
  if Length(Postfix) > 0 then
  begin
    if SpaceBeforePostfix then Result := Result + ' ';
    Result := Result + Postfix;
  end;
  DoValueToText(AValue, Result);
end;

function TTBXCustomSpinEditItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  if not (tboUseEditWhenVertical in EditOptions) and
     (AView.Orientation = tbvoVertical) then
    Result := TTBXItemViewer
  else
    Result := TTBXSpinEditViewer;
end;

function TTBXCustomSpinEditItem.GetValue: Extended;
begin
  if not ParseValue(Text, Result) then
    Result := FLastGoodValue;
end;

function TTBXCustomSpinEditItem.IsIncrementStored: Boolean;
begin
  Result := FIncrement <> 1;
end;

function TTBXCustomSpinEditItem.IsMaxValueStored: Boolean;
begin
  Result := FMaxValue <> 0;
end;

function TTBXCustomSpinEditItem.IsMinValueStored: Boolean;
begin
  Result := FMinValue <> 0;
end;

function TTBXCustomSpinEditItem.IsValueStored: Boolean;
begin
  Result := GetValue <> 0;
end;

function TTBXCustomSpinEditItem.ParseValue(const S: string; out V: Extended): Boolean;
const
  CWhiteSpace = [' ', #9];
  CDigits = ['0'..'9'];
  CHexDigits = CDigits + ['A'..'F'];
  CInvalidUnitChars = [#0..#31, ' ', '*', '+', ',', '-', '.', '/', '0'..'9', '^'];
  CInvalidHexUnitChars = CInvalidUnitChars + ['A'..'F'];
var
  P: PChar;
  Sign1: Integer;
  Value1: Extended;
  Value2: Extended;
  Operator: Char;
  PrefixString, PostfixString: string;

  procedure SkipWhiteSpace;
  begin
    while P^ in CWhiteSpace do Inc(P);
  end;

  function GetInt: Integer;
  begin
    Result := 0;
    while P^ in CDigits do
    begin
      Result := Result * 10 + (Integer(P^) - Integer('0'));
      Inc(P);
    end;
  end;

  function GetInt2: Extended;
  begin
    Result := 0;
    while P^ in CDigits do
    begin
      Result := Result * 10 + (Integer(P^) - Integer('0'));
      Inc(P);
    end;
  end;

  function GetNumber(out PrefixString, PostfixString: string; out R: Extended): Boolean;
  var
    PStart: PChar;
    Tmp: Integer;
    ExponentSign, IR: Integer;
    Count1, Count2: Integer;
    E: Extended;
  begin
    R := 0;
    Result := False;

    { Read prefix }
    PStart := P;
    if ValueType <> evtHex then while not (P^ in CInvalidUnitChars) do Inc(P)
    else while not (P^ in CInvalidHexUnitChars) do Inc(P);
    SetString(PrefixString, PStart, P - PStart);
    SkipWhiteSpace;

    { Read value }
    if ValueType in [evtFloat, evtInteger] then
    begin
      if (ValueType = evtInteger) and not (P^ in CDigits) then Exit;

      { get the integer part }
      PStart := P;
      R := GetInt2;
      Count1 := P - PStart;

      if (ValueType = evtFloat) and (P^ = DecimalSeparator) then
      begin
        Inc(P);
        PStart := P;
        E := GetInt2;
        R := R + E / IntPower(10, P - PStart);
        Count2 := P - PStart;
      end
      else Count2 := 0;

      if (Count1 = 0) and (Count2 = 0) then Exit; // '.' (or ',') is not a number

      if (ValueType = evtFloat) and (P^ in ['e', 'E']) and (PChar(P + 1)^ in ['+', '-', '0'..'9']) then
      begin
        Inc(P);
        ExponentSign := 1;
        if P^ = '-' then
        begin
          ExponentSign := -1;
          Inc(P);
        end
        else if P^ = '+' then Inc(P);
        if not (P^ in CDigits) then Exit;
        Tmp := GetInt;
        if Tmp >= 5000 then Exit;
        R := R * IntPower(10, Tmp * ExponentSign);
      end;
    end
    else { evtHex }
    begin
      IR := 0;
      if not (P^ in CHexDigits) then Exit;
      while P^ in CHexDigits do
      begin
        IR := IR shl 4;
        if P^ in CDigits then Inc(IR, Integer(P^) - Integer('0'))
        else if P^ in ['a'..'f'] then Inc(IR, Integer(P^) - Integer('a') + 10)
        else Inc(IR, Integer(P^) - Integer('A') + 10);
        Inc(P);
      end;
      R := IR;
    end;
    SkipWhiteSpace;

    { Read postfix }
    PStart := P;
    if ValueType <> evtHex then while not (P^ in CInvalidUnitChars) do Inc(P)
    else while not (P^ in CInvalidHexUnitChars) do Inc(P);
    SetString(PostfixString, PStart, P - PStart);
    SkipWhiteSpace;

    Result := True;
  end;

begin
  V := 0;

  { Try text-to-value conversion for predefined "constants" }
  Result := DoTextToValue(S, V);
  if Result then Exit;

  { Parse the string for values and expressions }
  if Length(S) = 0 then Exit;
  P := PChar(S);
  SkipWhiteSpace;

  { Read the sign }
  Sign1 := 1;
  if P^ = '-' then
  begin
    Sign1 := -1;
    Inc(P);
    SkipWhiteSpace;
  end
  else if P^ = '+' then
  begin
    Inc(P);
    SkipWhiteSpace;
  end;

  { Read value }
  if not GetNumber(PrefixString, PostfixString, Value1) then Exit;
  if not DoConvert(PrefixString, PostfixString, Value1) then Exit;
  Value1 := Value1 * Sign1;
  V := Value1;

  { Read operator }
  if P^ in ['*', '+', '-', '/'] then
  begin
    Operator := P^;
    Inc(P);
    SkipWhiteSpace;
    if not GetNumber(PrefixString, PostfixString, Value2) then Exit;
    if not DoConvert(PrefixString, PostfixString, Value2) then Exit;
    case Operator of
    '*': V := V * Value2;
    '+': V := V + Value2;
    '-': V := V - Value2;
    '/': if Value2 <> 0 then V := V / Value2 else Exit;
    end;
  end;

  if P^ = #0 then Result := True;
end;

procedure TTBXCustomSpinEditItem.SetAsInteger(AValue: Integer);
begin
  Value := AValue;
end;

procedure TTBXCustomSpinEditItem.SetDecimal(NewDecimal: TDecimal);
begin
  if NewDecimal <> FDecimal then
  begin
    FDecimal := NewDecimal;
    SetValueEx(GetValue, tcrNumericProperty);
  end;
end;

procedure TTBXCustomSpinEditItem.SetIncrement(const NewIncrement: Extended);
begin
  if NewIncrement <= 0 then
    raise EPropertyError.Create('Increment should be a positive value');
  FIncrement := NewIncrement;
end;

procedure TTBXCustomSpinEditItem.SetPostfix(const NewPostfix: string);
begin
  if not ValidateUnits(NewPostfix) then
    raise EPropertyError.Create('Invalid postfix');
  FPostfix := NewPostfix;
  SetValueEx(GetValue, tcrNumericProperty);
end;

procedure TTBXCustomSpinEditItem.SetPrefix(const NewPrefix: string);
begin
  if not ValidateUnits(NewPrefix) then
    raise EPropertyError.Create('Invalid prefix');
  FPrefix := NewPrefix;
  SetValueEx(GetValue, tcrNumericProperty);
end;

procedure TTBXCustomSpinEditItem.SetSpaceAfterPrefix(UseSpace: Boolean);
begin
  FSpaceAfterPrefix := UseSpace;
  SetValueEx(GetValue, tcrNumericProperty);
end;

procedure TTBXCustomSpinEditItem.SetSpaceBeforePostfix(UseSpace: Boolean);
begin
  FSpaceBeforePostfix := UseSpace;
  SetValueEx(GetValue, tcrNumericProperty);
end;

procedure TTBXCustomSpinEditItem.SetValue(NewValue: Extended);
begin
  SetTextEx(GetAsText(NewValue), tcrNumericProperty);
  if FLastGoodValue <> NewValue then
  begin
    FLastGoodValue := NewValue;
    DoValueChange(NewValue);
  end;
end;

procedure TTBXCustomSpinEditItem.SetValueEx(NewValue: Extended; Reason: Integer);
begin
  SetTextEx(GetAsText(NewValue), Reason);
  if FLastGoodValue <> NewValue then
  begin
    FLastGoodValue := NewValue;
    DoValueChange(NewValue);
  end;
end;

procedure TTBXCustomSpinEditItem.SetValueType(NewType: TSEValueType);
var
  V: Extended;
begin
  if NewType <> FValueType then
  begin
    V := GetValue;
    FValueType := NewType;
    SetValueEx(V, tcrNumericProperty);
    if NewType in [evtInteger, evtHex] then FIncrement := Max(Round(FIncrement), 1);
  end;
end;

function TTBXCustomSpinEditItem.ValidateUnits(const S: string): Boolean;
const
  InvalidChars = [#0..#31, ' ', '*', '+', ',', '-', '.', '/', '0'..'9', '^'];
var
  I: Integer;
begin
  Result := False;
  if Length(S) > 0 then
    for I := 1 to Length(S) do
     if S[I] in InvalidChars then Exit;
  Result := True;
end;

{ TTBXSpinEditViewer }

destructor TTBXSpinEditViewer.Destroy;
begin
  FBtnTimer.Free;
  inherited;
end;

procedure TTBXSpinEditViewer.GetEditInfo(out EditInfo: TTBXEditInfo; const ItemInfo: TTBXItemInfo);
const
  CDisabled: array [Boolean] of Integer = (EBSS_DISABLED, 0);
  CHot: array [Boolean] of Integer = (0, EBSS_HOT);
  CUpDnState: array [TSEBtnState] of Integer = (0, EBSS_UP, EBSS_DOWN);
begin
  inherited GetEditInfo(EditInfo, ItemInfo);
  EditInfo.RightBtnInfo.ButtonType := EBT_SPIN;
  EditInfo.RightBtnInfo.ButtonState := CDisabled[ItemInfo.Enabled] or
    CHot[ItemInfo.HoverKind = hkMouseHover] or CUpDnState[FBtnState];
end;

function TTBXSpinEditViewer.GetIndentAfter: Integer;
begin
  if IsToolbarStyle then Result := CurrentTheme.GetIntegerMetrics(Self, TMI_EDIT_BTNWIDTH)  + 2
  else Result := GetSystemMetricsForControl(View.FWindow, SM_CXMENUCHECK) + 2;
end;

function TTBXSpinEditViewer.HandleEditMessage(var Message: TMessage): Boolean;
var
  Item: TTBXCustomSpinEditItem;

  function Val: Extended;
  begin
    if not Item.ParseValue(EditControl.Text, Result) then Result := Item.FLastGoodValue;
  end;

begin
  Item := TTBXCustomSpinEditItem(Self.Item);
  if Message.Msg = WM_CHAR then
    case TWMChar(Message).CharCode of
      VK_TAB:
        begin
          Item.Value := Val;
          EditControl.Text := Item.Text;
        end;
      VK_RETURN:
        begin
          Item.Value := Val;
          EditControl.Text := Item.Text;
        end;
      VK_ESCAPE:
        begin
//          Item.Value := Item.GetValue;
        end;
    end
  else if Message.Msg = WM_KEYDOWN then
    case TWMKeyDown(Message).CharCode of
      VK_UP:
        begin
          Item.ClickUp;
          EditControl.Text := Item.Text;
          EditControl.SelectAll;
          Result := True;
          Exit;
        end;
      VK_DOWN:
        begin
          Item.ClickDown;
          EditControl.Text := Item.Text;
          EditControl.SelectAll;
          Result := True;
          Exit;
        end;
    end;

  Result := inherited HandleEditMessage(Message);
end;

procedure TTBXSpinEditViewer.InvalidateButtons;
var
  R: TRect;
begin
  with TTBXSpinEditItem(Item) do
    if Show and not IsRectEmpty(BoundsRect) then
      begin
        R := BoundsRect;
        R.Left := R.Right - GetIndentAfter;
        InvalidateRect(View.Window.Handle, @R, False);
        Include(State, tbisInvalidated);
      end;
end;

function TTBXSpinEditViewer.IsPtInButtonPart(X, Y: Integer): Boolean;
begin
  Result := X <= (BoundsRect.Right - BoundsRect.Left) - GetIndentAfter;
end;

procedure TTBXSpinEditViewer.LosingCapture;
begin
  FBtnTimer.Free;
  FBtnTimer := nil;
  inherited;
end;

procedure TTBXSpinEditViewer.MouseDown(Shift: TShiftState; X, Y: Integer; var MouseDownOnMenu: Boolean);
begin
  if not Item.Enabled then Exit;
  FBtnState := ebsNone;
  if X >= BoundsRect.Right - BoundsRect.Left - GetIndentAfter then
  begin
    if Y < (BoundsRect.Bottom - BoundsRect.Top) div 2 then
    begin
      FBtnState := ebsUp;
      TTBXSpinEditItem(Item).ClickUp;
    end
    else
    begin
      FBtnState := ebsDown;
      TTBXSpinEditItem(Item).ClickDown;
    end;

    if FBtnTimer = nil then
    begin
      FBtnTimer := TTimer.Create(nil);
      FBtnTimer.OnTimer := TimerHandler;
    end;
    FBtnTimer.Interval := SE_FIRSTINTERVAL;
    FBtnTimer.Enabled := True;
  end;

  if FBtnState <> ebsNone then
  begin
    InvalidateButtons;
    inherited;
    View.SetCapture;
  end
  else inherited;
end;

procedure TTBXSpinEditViewer.MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean);
begin
  if FBtnState <> ebsNone then
  begin
    FBtnState := ebsNone;
    FBtnTimer.Free;
    FBtnTimer := nil;
    InvalidateButtons;
  end;
  inherited;
end;

procedure TTBXSpinEditViewer.TimerHandler(Sender: TObject);
begin
  FBtnTimer.Interval := SE_INTERVAL;
  if FBtnState = ebsUp then TTBXSpinEditItem(Item).ClickUp
  else if FBtnState = ebsDown then TTBXSpinEditItem(Item).ClickDown
  else
  begin
    FBtnTimer.Free;
    FBtnTimer := nil;
  end;
end;

{$ENDIF}

end.
