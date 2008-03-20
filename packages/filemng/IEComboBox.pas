unit IEComboBox;

{==================================================================

 Components TIECustomCombobox  /  Version 1.4  / January 2000
            TIEComboBox
            TIEDriveBox
 ==================================================================


    Description:
    ============
    TIECustomComboBox is a combobox with variable width of the dropdown list and
    provides the additional event OnCloseUp.

    TIEComboBox publishes the properties of the class TIECustomComboBox including
    the property Align wich might be was forgotten by Borland.

    TIEDriveComboBox realizes a selection control for the aviable drives of
    the system with icons. The drive icons are taken from the system image
    list.


    Author:
    =======
    (c) Ingo Eckel 1999
    Sodener Weg 38
    65812 Bad Soden
    Germany

    For detailed documentation and history see the documentation in TIEDriveComboBox.htm.

    V1.3:
    - Property DisplayStyle changed.


{==================================================================}

{Required compiler options:}
{$A+,B-,X+,H+,P+}

interface

uses
  StdCtrls, Controls, Messages, Types, Classes, Graphics;

const
  IconWidth = 16;

type
  TIECloseUpEvent = procedure (Sender: TObject; Canceled: Boolean) of object;

// =======================================================================
// Class TIECustomComboBox
// =======================================================================
  TIECustomComboBox = class(TCustomComboBox)
  private
    FDropDownFixedWidth: Integer;
    FOnCloseUp: TIECloseUpEvent;
    FCanceled: Boolean;
    FUseSystemImageList: Boolean;
    FSystemImageList: TImageList;

    function GetTopIndex: Integer;
    procedure SetTopIndex(Value: Integer);
    procedure SetUseSystemImageList(Value: Boolean);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;

  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    function GetItemImage(Index: Integer): Integer; virtual;
    function GetItemIndent(Index: Integer): Integer; virtual;
    function GetItemText(Index: Integer): string;
    function GetItemTextEx(Index: Integer; ForList: Boolean): string; virtual;
    function ImageList: TImageList; virtual;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure DoCloseUp(Canceled: Boolean); virtual;
    procedure DropDown; override;
    function GetMaxItemWidth: Integer;
    procedure ResetItemHeight;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTextWidth(Str: string): Integer;
    procedure DoPreloadImages;

    property ItemImage[Index: Integer]: Integer read GetItemImage;
    property ItemIndent[Index: Integer]: Integer read GetItemIndent;
    property ItemText[Index: Integer]: string read GetItemText;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    property UseSystemImageList: Boolean read FUseSystemImageList write SetUseSystemImageList;
    property DropDownFixedWidth: Integer read FDropDownFixedWidth write FDropDownFixedWidth default 0;
    property OnCloseUp: TIECloseUpEvent read FOnCloseUp write FOnCloseUp;
  published
  end;


// =======================================================================
// Class TIEComboBox
// =======================================================================
  TIEComboBox = class(TIECustomComboBox)
  published
    property DropDownFixedWidth;
    property OnCloseUp;

    property Style; {Must be published before Items}
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property Items;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDock;
    property OnStartDrag;
  end;

function  GetItemHeight(Font: TFont): Integer;

procedure Register;

implementation

uses
  SysUtils, Forms, Dialogs, Imglist, ShellAPI, CommCtrl, Math, Windows;

procedure Register;
begin
  RegisterComponents('DriveDir', [TIEComboBox]);
end;

// =======================================================================
// Class TIECustomComboBox
// =======================================================================

constructor TIECustomComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanceled := True;
  FUseSystemImageList := False;
  FSystemImageList := nil;
end; {TIECustomComboBox.Create}

destructor TIECustomComboBox.Destroy;
begin
  FreeAndNil(FSystemImageList);
  inherited;
end;

procedure TIECustomComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;

  ResetItemHeight;
  RecreateWnd;
end; {CMFontChanged}

procedure TIECustomComboBox.DoCloseUp(Canceled: Boolean);
begin
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self, Canceled);
end; { DoCloseUp }

procedure TIECustomComboBox.DropDown;
var
  ItemWidth: Integer;
begin
  {Check to see if DropDownFixed Width > 0. Then just set the
   width of the list box. Otherwise, loop through the items
   and set the width of the list box to 8 pixels > than the
   widest string to buffer the right side. Anything less than
   8 for some reason touches the end of the item on high-res
   monitor settings.}
  if (FDropDownFixedWidth > 0) then
      Self.Perform(CB_SETDROPPEDWIDTH, FDropDownFixedWidth, 0)
    else
  begin
    ItemWidth := GetMaxItemWidth + 8;
    if Items.Count > DropDowncount then
      Inc(ItemWidth, 16);
    Self.Perform(CB_SETDROPPEDWIDTH, ItemWidth, 0);
  end;

  inherited DropDown;
end; {TIECustomComboBox.DropDown}

function TIECustomComboBox.GetTextWidth(Str: string): Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Size: TSize;
begin
  DC := GetDC(0);
  try
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextExtentPoint32(DC, PChar(Str), Length(Str), Size);
    Result := Size.Cx;
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
end; {TIECustomComboBox.GetTextWidth}

function TIECustomComboBox.GetMaxItemWidth: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Size: TSize;
  Index: Integer;
begin
  Result := 0;
  DC := GetDC(0);
  try
    SaveFont := SelectObject(DC, Font.Handle);
    for Index := 0 to Items.Count - 1 do
    begin
      GetTextExtentPoint32(DC, PChar(ItemText[Index]), Length(ItemText[Index]), Size);
      if (ImageList <> nil) and (ItemImage[Index] >= 0) then
        Inc(Size.Cx, IconWidth + 6);
      Inc(Size.Cx, ItemIndent[Index]);
      if Size.Cx > Result then Result := Size.Cx;
    end;
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
end; {TIECustomComboBox.GetMaxItemWidth}

function TIECustomComboBox.GetTopIndex: Integer;
begin
  Result := Perform(CB_GETTOPINDEX, 0, 0);
end; {TIECustomComboBox.GetTopIndex}

{$HINTS OFF}
procedure TIECustomComboBox.DoPreloadImages;
var
  Index, Dummy: Integer;
begin
  for Index := 0 to Items.Count-1 do
    Dummy := ItemImage[Index];
end;
{$HINTS ON}

procedure TIECustomComboBox.ResetItemHeight;
var
  AHeight: Integer;
Begin
  AHeight := Max(GetItemHeight(Font), 10) + 2;
  inherited ItemHeight := AHeight;
  if HandleAllocated then
  begin
    {Set height of list items:}
    SendMessage(Handle, CB_SETITEMHEIGHT,  0, Max(AHeight, 12));
    {Set height of selection field:}
    SendMessage(Handle, CB_SETITEMHEIGHT, -1, AHeight);
    {Set height of delphi-control:}
    Height := AHeight;
  end;
end;

procedure TIECustomComboBox.SetTopIndex(Value: Integer);
begin
  if Value <> TopIndex then
    Perform(CB_SETTOPINDEX, Value, 0);
end; {TIECustomComboBox.SetTopIndex}

procedure TIECustomComboBox.CNCommand(var Message: TWMCommand);
begin
  inherited;

  case Message.NotifyCode of
    CBN_CLOSEUP:
      DoCloseUp(FCanceled);
    CBN_SELENDCANCEL:
      FCanceled := True;
    CBN_SELENDOK:
      FCanceled := False;
  end;
end; {TIECustomComboBox.CNCommand}

function GetItemHeight(Font: TFont): Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
  Result := Metrics.tmHeight;
end; {GetItemHeight}

procedure TIECustomComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Text: string;
  Image: Integer;
  InList: Boolean;
begin
  inherited;

  if Style = csOwnerDrawFixed then
  with Canvas do
  begin
    FillRect(Rect);
    Pen.Color := clWindowText;
    // Rect.Top = 3 when we draw selected item in component rect (not in dropdown)
    InList := (Rect.Top <> 3);
    Text := GetItemTextEx(Index, InList);

    if InList then Rect.Left := Rect.Left + ItemIndent[Index];

    if ImageList <> nil then
    begin
      Image := ItemImage[Index];
      if Image >= 0 then
      begin
        ImageList.Draw(Canvas, Rect.Left + 2, Rect.Top, Image);
        Rect.Left := Rect.Left + IconWidth + 6;
      end
        else Rect.Left := Rect.Left + 2;
    end
      else Rect.Left := Rect.Left + 2;

    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect,
      DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX));
  end;
end;

function TIECustomComboBox.GetItemImage(Index: Integer): Integer;
begin
  Result := -1;
end;

function TIECustomComboBox.GetItemIndent(Index: Integer): Integer;
begin
  Result := 0;
end;

function TIECustomComboBox.GetItemText(Index: Integer): string;
begin
  Result := GetItemTextEx(Index, True);
end;

function TIECustomComboBox.GetItemTextEx(Index: Integer; ForList: Boolean): string;
begin
  Result := Items[Index];
end;

function TIECustomComboBox.ImageList: TImageList;
var
  ImageListHandle: HImageList;
  FileInfo: TSHFileInfo;
begin
  if FUseSystemImageList then
  begin
    if not Assigned(FSystemImageList) then
    begin
      FSystemImageList := TImageList.Create(Self);
      ImageListHandle := SHGetFileInfo('', 0, FileInfo, SizeOf(FileInfo),
        SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
      if ImageListHandle <> 0 then
        with FSystemImageList do
        begin
          ShareImages  := True;
          Handle := ImageListHandle;
          DrawingStyle := dsTransparent;
        end;
    end;
    Result := FSystemImageList;
  end
    else Result := nil;
end;

procedure TIECustomComboBox.SetUseSystemImageList(Value: Boolean);
begin
  if FUseSystemImageList <> Value then
  begin
    if not FUseSystemImageList then
    begin
      if ImageList <> nil then
        raise Exception.Create('ImageList is already created.');
    end
      else FreeAndNil(FSystemImageList);
    FUseSystemImageList := Value;
  end;
end;

end.
