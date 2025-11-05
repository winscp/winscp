unit HistoryComboBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TUIStateAwareComboBox = class(TComboBox)
  private
    FDarkMode: Boolean;
    procedure SetDarkMode(Value: Boolean);
  protected
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: TWindowProcPtr); override;
    procedure CreateWnd; override;
  public
    property DarkMode: Boolean read FDarkMode write SetDarkMode default False;
  end;

type
  THistorySaveOn = set of (soExit, soDropDown);

const
  DefaultHistorySaveOn = [soExit, soDropDown];
  DefaultMaxHistorySize = 30;

type
  THistoryComboBox = class;

  THistoryComboBoxGetData = procedure(Sender: THistoryComboBox; var Data: Pointer) of object;
  THistoryComboBoxSetData = procedure(Sender: THistoryComboBox; Data: Pointer) of object;

  THistoryComboBox = class(TUIStateAwareComboBox)
  private
    { Private declarations }
    FSaveOn: THistorySaveOn;
    FMaxHistorySize: Integer;
    FOnGetData: THistoryComboBoxGetData;
    FOnSetData: THistoryComboBoxSetData;

    procedure SetMaxHistorySize(AMaxHistorySize: Integer);
    function GetMaxItemWidth: Integer;
  protected
    { Protected declarations }
    procedure DoExit; override;
    procedure DropDown; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Change; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SaveToHistory; virtual;
  published
    { Published declarations }
    property SaveOn: THistorySaveOn read FSaveOn write FSaveOn default DefaultHistorySaveOn;
    property MaxHistorySize: Integer read FMaxHistorySize write SetMaxHistorySize default DefaultMaxHistorySize;
    property OnGetData: THistoryComboBoxGetData read FOnGetData write FOnGetData;
    property OnSetData: THistoryComboBoxSetData read FOnSetData write FOnSetData;
  end;

procedure SaveToHistory(Strings: TStrings; T: string; Data: Pointer = nil; MaxHistorySize: Integer = DefaultMaxHistorySize);

procedure Register;

implementation

uses
  PasTools;

procedure Register;
begin
  RegisterComponents('Martin', [THistoryComboBox]);
end;

procedure SaveToHistory(Strings: TStrings; T: string; Data: Pointer; MaxHistorySize: Integer);
begin
  if T <> '' then
  begin
    while Strings.IndexOf(T) >= 0 do Strings.Delete(Strings.IndexOf(T));
    Strings.InsertObject(0, T, TObject(Data));
  end;
  while Strings.Count > MaxHistorySize do
    Strings.Delete(Strings.Count-1);
end;

  { TUIStateAwareComboBox }

procedure TUIStateAwareComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: TWindowProcPtr);
begin
  inherited;

  if Message.Msg = WM_SYSKEYDOWN then
  begin
    UpdateUIState(TWMKey(Message).CharCode);
  end;
end;

procedure TUIStateAwareComboBox.CreateWnd;
begin
  inherited;
  if DarkMode then
    SetDarkModeTheme(Self, 'CFD');
end;

procedure TUIStateAwareComboBox.SetDarkMode(Value: Boolean);
begin
  if DarkMode <> Value then
  begin
    FDarkMode := Value;
    RecreateWnd;
  end;
end;

  { THistoryComboBox }

constructor THistoryComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FSaveOn := DefaultHistorySaveOn;
  FMaxHistorySize := DefaultMaxHistorySize;
  FOnGetData := nil;
  FOnSetData := nil;
end;

procedure THistoryComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if ((Key = VK_DOWN) or (Key = VK_UP)) and
     (not (ssAlt in Shift)) and (soDropDown in SaveOn) then
  begin
    if Items.IndexOf(Text) < 0 then SaveToHistory;
  end;
  if DroppedDown and (Key = VK_DELETE) and (ssCtrl in Shift) and (SaveOn <> []) then
  begin
    Items.Clear;
    Key := 0;
  end;
  inherited;
end;

procedure THistoryComboBox.SetMaxHistorySize(AMaxHistorySize: Integer);
begin
  FMaxHistorySize := AMaxHistorySize;
  while Items.Count > FMaxHistorySize do
    Items.Delete(Items.Count-1);
end;

procedure THistoryComboBox.DoExit;
begin
  inherited;
  if soExit in SaveOn then SaveToHistory;
end;

procedure THistoryComboBox.DropDown;
var
  ItemWidth: Integer;
begin
  inherited;
  if soDropDown in SaveOn then SaveToHistory;

  ItemWidth := GetMaxItemWidth + ScaleByPixelsPerInch(8, Self);
  if Items.Count > DropDownCount then
    Inc(ItemWidth, GetSystemMetricsForControl(Self, SM_CXVSCROLL));
  Self.Perform(CB_SETDROPPEDWIDTH, ItemWidth, 0);
end;

procedure THistoryComboBox.Change;
var
  Index: Integer;
begin
  inherited Change;
  if Assigned(OnSetData) then
  begin
    // note that ItemIndex is not reliable
    Index := Items.IndexOf(Text);
    if Index >= 0 then OnSetData(Self, Items.Objects[Index]);
  end;
end;

procedure THistoryComboBox.SaveToHistory;
var
  Data: Pointer;
begin
  if Text <> '' then
  begin
    Data := nil;
    if Assigned(OnGetData) then
      OnGetData(Self, Data);
    HistoryComboBox.SaveToHistory(Items, Text, Data, MaxHistorySize);
    ItemIndex := 0;
  end;
end;

function THistoryComboBox.GetMaxItemWidth: Integer;
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
      GetTextExtentPoint32(DC, PChar(Items[Index]), Length(Items[Index]), Size);
      if Size.Cx > Result then Result := Size.Cx;
    end;
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
end;

initialization
end.
