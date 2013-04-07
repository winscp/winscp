unit UnixPathComboBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TUnixPathComboBox = class(TCustomComboBox)
  private
    { Private declarations }
    FUnixPath: string;
    FRootName: string;
    FImageList: TImageList;
    FNotifyChange: boolean;
    F_NotifyChange: boolean;
    procedure SetUnixPath(AUnixPath: string);
    procedure SetRootName(ARootName: string);
    procedure UpdateItems;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DrawItem;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMCHAR(var Message: TMessage); message WM_CHAR;
  protected
    { Protected declarations }
    procedure CreateWnd; override;
    procedure Change; override;
    procedure Click; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property NotifyChange: Boolean read F_NotifyChange write F_NotifyChange;
  published
    { Published declarations }
    property Align;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Anchors;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnChange;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;

    property UnixPath: string read FUnixPath write SetUnixPath;
    property RootName: string read FRootName write SetRootName;
  end;

procedure Register;

implementation

uses
  ShellAPI, ImgList;

procedure Register;
begin
  RegisterComponents('My', [TUnixPathComboBox]);
end;

  { TUnixPathComboBox }

constructor TUnixPathComboBox.Create(AOwner: TComponent);
var
  sfi: TSHFileInfo;
begin
  inherited Create(AOwner);
  Style := csOwnerDrawFixed;
  FImageList := TImageList.create(self);
  FImageList.handle :=
    SHGetFileInfo('',0,sfi,sizeof(tshfileinfo), shgfi_sysiconindex or shgfi_smallicon);
  FImageList.shareimages := true;
  FImageList.BlendColor := clHighlight;
  FRootName := '/ <root>';
  F_NotifyChange := True;
end;

procedure TUnixPathComboBox.SetUnixPath(AUnixPath: string);
begin
  if FUnixPath <> AUnixPath then
  begin
    FUnixPath := AUnixPath;
    UpdateItems;
    FNotifyChange := true;
    Change; {The only way to notify OnChange event}
    FNotifyChange := false;
  end;
end;

procedure TUnixPathComboBox.UpdateItems;
var
  Temp: string;
  P: Integer;
begin
  with Items do
  begin
    BeginUpdate;
    Clear;
    try
      Temp := FUnixPath;
      while Temp <> '' do
      begin
        P := LastDelimiter('/', Temp);
        if P < Length(Temp) then Insert(0, Copy(Temp, P+1, Length(Temp)-(P+1)+1));
        SetLength(Temp, P-1);
        if Length(Temp) = 0 then
          Insert(0, FRootName);
      end;
    finally
      ItemIndex := Count-1;
      EndUpdate;
    end;
  end;
end;

procedure TUnixPathComboBox.SetRootName(ARootName: string);
begin
  if FRootName <> ARootName then
  begin
    FRootName := ARootName;
    if Items.Count > 0 then Items[0] := FRootName
      else UpdateItems;
  end;
end;

function GetIconIndex(AFile: string; Attrs, Attrs2: Cardinal): Integer;
var
  SFI: TSHFileInfo;
begin
  SHGetFileInfo(PChar(AFile), Attrs, SFI, SizeOf(TSHFileInfo),
    SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES or Attrs2);
  Result := SFI.iIcon;
end;

procedure TUnixPathComboBox.CNDrawItem(var Message: TWMDrawItem);
var
  Icon: Integer;
begin
  with Message.DrawItemStruct^ do
  begin
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if Integer(ItemID) < 0 then Canvas.FillRect(rcItem)
      else
    with Canvas do
    begin
      // This next line prevents 'edit selection' text from being indented as well.
      if (rcItem.top <> 3) then rcItem.left := rcItem.left + 10*Integer(ItemID);

      if Integer(ItemID) < Items.Count-1 then
          Icon := GetIconIndex('dummy', FILE_ATTRIBUTE_NORMAL or FILE_ATTRIBUTE_DIRECTORY, 0)
        else
          Icon := GetIconIndex('dummy', FILE_ATTRIBUTE_NORMAL or FILE_ATTRIBUTE_DIRECTORY, shgfi_openicon);

      if (Integer(itemID) >= 0) and (ods_Selected and ItemState <> 0) then
        FimageList.DrawingStyle := dsFocus
      else
        FimageList.DrawingStyle := dsNormal;

      FillRect(rcItem);
      FImageList.draw(canvas,rcItem.left+2,rcItem.top, Icon);
      rcItem.left := rcItem.left+FImageList.width+4;

      if (Integer(itemID) >= 0) and (ods_Selected and ItemState <> 0) then
      begin
        rcItem.right := rcItem.left+textwidth(items[ItemID])+4;
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color := clHighlightText;
        FillRect(rcItem);
      end;

      TextOut(rcItem.left+2, rcItem.top, items[ItemID]);
    end; {with canvas}

    if (ods_Focus and ItemState <> 0) then DrawFocusRect(hDC, rcItem);
    Canvas.Handle := 0;
  end;
end;

destructor TUnixPathComboBox.Destroy;
begin
  FImageList.handle := 0;
  FImageList.free;
  inherited Destroy;
end;

procedure TUnixPathComboBox.CreateWnd;
begin
  inherited CreateWnd;
  UpdateItems;
end;

procedure TUnixPathComboBox.CMFontChanged(var Message: TMessage);

  function GetItemHeight(Font: TFont): Integer;
  var
    DC: HDC;
    SaveFont: HFont;
    Metrics: TTextMetric;
  begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    Result := Metrics.tmHeight;
  end;

var
  IHeight: integer;
begin
  inherited;
  IHeight := GetItemHeight(Font)+2;
  if IHeight < FImageList.height then IHeight := FImageList.height;
  ItemHeight := IHeight;
  RecreateWnd;
end;

procedure TUnixPathComboBox.Change;
begin
  if FNotifyChange and F_NotifyChange then inherited Change;
end;

procedure TUnixPathComboBox.Click;
var
  NewPath: string;
  i: Integer;
begin
  inherited Click;
  if (ItemIndex >= 0) and (ItemIndex < Items.Count-1) and
    (sendmessage(handle,CB_GETDROPPEDSTATE,0,0)=0) then
  begin
    NewPath := '/';
    for i := 1 to ItemIndex do
    begin
      NewPath := NewPath + Items[i];
      if i < ItemIndex then NewPath := NewPath + '/';
    end;
    UnixPath := NewPath;
  end;
end;

procedure TUnixPathComboBox.WMCHAR(var Message: TMessage);
begin
  inherited;
  if (TWMKey(Message).CharCode = VK_RETURN) or
   (TWMKey(Message).CharCode = VK_ESCAPE) then Click;
end;

end.
