unit ComboEdit;

{$J+}
{$WARN UNIT_PLATFORM OFF}

interface

uses Windows, Classes, StdCtrls, Controls, Messages, Forms, Graphics,
  Menus, Buttons, Dialogs, Mask,
  { SysUtils must overload deprecated FileCtrl (implements SelectDirectory) }
  FileCtrl, SysUtils;

const
  scAltDown = scAlt + vk_Down;
  scCtrlEnter = scCtrl + vk_Return;
  DefEditBtnWidth = 25;
  DefButtonCaption = '...';

resourcestring
  SBrowse = 'Browse';
  SDefaultFilter = 'All files (*.*)|*.*';
  SInvalidFileName = 'Invalid file name - %s';

type
  TFileExt = type string;

  { TCustomComboEdit }

  // Could be replaced by TCustomButtonedEdit
  TCustomComboEdit = class(TCustomEdit)
  private
    FButton: TButton;
    FBtnControl: TWinControl;
    FOnButtonClick: TNotifyEvent;
    FClickKey: TShortCut;
    procedure SetEditRect;
    procedure UpdateBtnBounds;
    procedure EditButtonClick(Sender: TObject);
    function GetMinHeight: Integer;
    function GetTextHeight: Integer;
    function GetButtonWidth: Integer;
    procedure SetButtonWidth(Value: Integer);
    function BtnWidthStored: Boolean;
    function GetButtonCaption: string;
    function ButtonCaptionStored: Boolean;
    procedure SetButtonCaption(Value: string);
    function GetButtonHint: string;
    procedure SetButtonHint(const Value: string);
    function GetButtonTabStop: Boolean;
    procedure SetButtonTabStop(Value: Boolean);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNCtlColor(var Message: TMessage); message CN_CTLCOLOREDIT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ButtonClick; dynamic;
    property Button: TButton read FButton;
    property ClickKey: TShortCut read FClickKey write FClickKey
      default scAltDown;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth
      stored BtnWidthStored;
    property ButtonCaption: string read GetButtonCaption write SetButtonCaption stored ButtonCaptionStored;
    property ButtonHint: string read GetButtonHint write SetButtonHint;
    property ButtonTabStop: Boolean read GetButtonTabStop write SetButtonTabStop default True;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoClick;
  end;

type
  TComboEdit = class(TCustomComboEdit)
  published
    property AutoSelect;
    property ButtonHint;
    property ButtonTabStop;
    property ButtonCaption;
    property BorderStyle;
    property CharCase;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ButtonWidth;
    property HideSelection;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;
  end;

  { TFileDirEdit }
{ The common parent of TFilenameEdit and TDirectoryEdit          }
{ For internal use only; it's not intended to be used separately }

const
  MaxFileLength = SizeOf(TFileName) - 1;

type
  TExecOpenDialogEvent = procedure(Sender: TObject; var Name: string;
    var Action: Boolean) of object;

  TFileDirEdit = class(TCustomComboEdit)
  private
    FErrMode: Cardinal;
    FAcceptFiles: Boolean;
    FOnDropFiles: TNotifyEvent;
    FOnBeforeDialog: TExecOpenDialogEvent;
    FOnAfterDialog: TExecOpenDialogEvent;
    procedure SetDragAccept(Value: Boolean);
    procedure SetAcceptFiles(Value: Boolean);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  protected
    FMultipleDirs: Boolean;
    procedure CreateHandle; override;
    procedure DestroyWindowHandle; override;
    procedure DoAfterDialog(var FileName: string; var Action: Boolean); dynamic;
    procedure DoBeforeDialog(var FileName: string; var Action: Boolean); dynamic;
    procedure ReceptFileDir(const AFileName: string); virtual; abstract;
    procedure ClearFileList; virtual;
    procedure DisableSysErrors;
    procedure EnableSysErrors;
    property MaxLength;
  published
    property AcceptFiles: Boolean read FAcceptFiles write SetAcceptFiles default False;
    property OnBeforeDialog: TExecOpenDialogEvent read FOnBeforeDialog
      write FOnBeforeDialog;
    property OnAfterDialog: TExecOpenDialogEvent read FOnAfterDialog
      write FOnAfterDialog;
    property OnDropFiles: TNotifyEvent read FOnDropFiles write FOnDropFiles;
    property OnButtonClick;
  end;

{ TFilenameEdit }

  TFileDialogKind = (dkOpen, dkSave , dkOpenPicture,
    dkSavePicture);

  TFilenameEdit = class(TFileDirEdit)
  private
    FDialog: TOpenDialog;
    FDialogKind: TFileDialogKind;
    procedure CreateEditDialog;
    function GetFileName: string;
    function GetDefaultExt: TFileExt;
    function GetFileEditStyle: TFileEditStyle;
    function GetFilter: string;
    function GetFilterIndex: Integer;
    function GetInitialDir: string;
    function GetHistoryList: TStrings;
    function GetOptions: TOpenOptions;
    function GetDialogTitle: string;
    function GetDialogFiles: TStrings;
    procedure SetDialogKind(Value: TFileDialogKind);
    procedure SetFileName(const Value: string);
    procedure SetDefaultExt(Value: TFileExt);
    procedure SetFileEditStyle(Value: TFileEditStyle);
    procedure SetFilter(const Value: string);
    procedure SetFilterIndex(Value: Integer);
    procedure SetInitialDir(const Value: string);
    procedure SetHistoryList(Value: TStrings);
    procedure SetOptions(Value: TOpenOptions);
    procedure SetDialogTitle(const Value: string);
    function IsCustomTitle: Boolean;
    function IsCustomFilter: Boolean;
  protected
    procedure ButtonClick; override;
    procedure ReceptFileDir(const AFileName: string); override;
    procedure ClearFileList; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Dialog: TOpenDialog read FDialog;
    property DialogFiles: TStrings read GetDialogFiles;
  published
    property DialogKind: TFileDialogKind read FDialogKind write SetDialogKind
      default dkOpen;
    property DefaultExt: TFileExt read GetDefaultExt write SetDefaultExt;
    property FileEditStyle: TFileEditStyle read GetFileEditStyle write SetFileEditStyle
      default fsEdit;
    property FileName: string read GetFileName write SetFileName stored False;
    property Filter: string read GetFilter write SetFilter stored IsCustomFilter;
    property FilterIndex: Integer read GetFilterIndex write SetFilterIndex default 1;
    property InitialDir: string read GetInitialDir write SetInitialDir;
    property HistoryList: TStrings read GetHistoryList write SetHistoryList;
    property DialogOptions: TOpenOptions read GetOptions write SetOptions
      default [ofHideReadOnly];
    property DialogTitle: string read GetDialogTitle write SetDialogTitle
      stored IsCustomTitle;
    property AutoSelect;
    property ButtonHint;
    property BorderStyle;
    property CharCase;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ButtonWidth;
    property HideSelection;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;
  end;

{ TDirectoryEdit }

  TDirectoryEdit = class(TFileDirEdit)
  private
    FInitialDir: string;
    FDialogText: string;
  protected
    procedure ButtonClick; override;
    procedure ReceptFileDir(const AFileName: string); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DialogText: string read FDialogText write FDialogText;
    property InitialDir: string read FInitialDir write FInitialDir;
    property MultipleDirs: Boolean read FMultipleDirs write FMultipleDirs default False;
    property AutoSelect;
    property ButtonHint;
    property BorderStyle;
    property CharCase;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ButtonWidth;
    property HideSelection;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;
  end;

  EComboEditError = class(Exception);

procedure Register;

implementation

uses
  ShellAPI, Consts, ExtDlgs, Variants, PasTools, UITypes;

procedure Register;
begin
  RegisterComponents('Martin', [TComboEdit, TFilenameEdit, TDirectoryEdit]);
end;

{ Utility functions }

type
  TCharSet = TSysCharSet;

function ExtractSubstr(const S: string; var Pos: Integer;
  const Delims: TCharSet): string;
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(S)) and not CharInSet(S[I], Delims) do Inc(I);
  Result := Copy(S, Pos, I - Pos);
  if (I <= Length(S)) and CharInSet(S[I], Delims) then Inc(I);
  Pos := I;
end;

function ValidFileName(const FileName: string): Boolean;
  function HasAny(const Str, Substr: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 1 to Length(Substr) do begin
      if Pos(Substr[I], Str) > 0 then begin
        Result := True;
        Break;
      end;
    end;
  end;
begin
  Result := (FileName <> '') and (not HasAny(FileName, '<>"[]|'));
  if Result then Result := Pos('\', ExtractFileName(FileName)) = 0;
end;

{ TCustomComboEdit }

constructor TCustomComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse];
  AutoSize := False;
  FClickKey := scCtrlEnter;
  FBtnControl := TWinControl.Create(Self);
  with FBtnControl do
  begin
    ControlStyle := ControlStyle + [csReplicatable];
    Width := DefEditBtnWidth;
    Height := 17;
    Visible := True;
    Parent := Self;
  end;

  FButton := TButton.Create(Self);
  with FButton do
  begin
    SetBounds(0, 0, FBtnControl.Width, FBtnControl.Height);
    ControlStyle := ControlStyle + [csReplicatable];
    ParentShowHint := True;
    Caption := DefButtonCaption;
    Visible := True;
    Parent := FBtnControl;
    OnClick := EditButtonClick;
  end;
  Height := 21;
end;

destructor TCustomComboEdit.Destroy;
begin
  FButton.OnClick := nil;
  inherited Destroy;
end;

procedure TCustomComboEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

procedure TCustomComboEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TCustomComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (FClickKey = ShortCut(Key, Shift)) and (ButtonWidth > 0) then
  begin
    EditButtonClick(Self);
    Key := 0;
  end;
end;

function TCustomComboEdit.GetButtonWidth: Integer;
begin
  Result := FButton.Width;
end;

procedure TCustomComboEdit.SetButtonWidth(Value: Integer);
begin
  if ButtonWidth <> Value then
  begin
    FBtnControl.Visible := Value > 1;
    if (csCreating in ControlState) then
    begin
      FBtnControl.Width := Value;
      FButton.Width := Value;
      with FButton do
        ControlStyle := ControlStyle - [csFixedWidth];
    end
    else if (Value <> ButtonWidth) and (Value < ClientWidth) then
    begin
      FButton.Width := Value;
      with FButton do
        ControlStyle := ControlStyle - [csFixedWidth];
      if HandleAllocated then UpdateBtnBounds;
    end;
  end;
end;

function TCustomComboEdit.GetButtonCaption: string;
begin
  Result := FButton.Caption;
end;

procedure TCustomComboEdit.SetButtonCaption(Value: string);
begin
  FButton.Caption := Value;
end;

function TCustomComboEdit.ButtonCaptionStored: Boolean;
begin
  Result := (FButton.Caption <> DefButtonCaption);
end;

function TCustomComboEdit.GetButtonHint: string;
begin
  Result := FButton.Hint;
end;

procedure TCustomComboEdit.SetButtonHint(const Value: string);
begin
  FButton.Hint := Value;
end;

function TCustomComboEdit.GetButtonTabStop: Boolean;
begin
  Result := FButton.TabStop;
end;

procedure TCustomComboEdit.SetButtonTabStop(Value: Boolean);
begin
  FButton.TabStop := Value;
end;

procedure TCustomComboEdit.SetEditRect;
var
  RMargin: Integer;
begin
  RMargin := FBtnControl.Width + ScaleByTextHeight(Self, 2);
  SendMessage(Handle, EM_SETMARGINS, EC_RIGHTMARGIN, MakeLong(0, RMargin));
end;

procedure TCustomComboEdit.UpdateBtnBounds;
var
  BtnRect: TRect;
begin
  if NewStyleControls then begin
    if Ctl3D and (BorderStyle = bsSingle) then
      BtnRect := Bounds(Width - FButton.Width - 4, 0,
        FButton.Width, Height - 4)
    else begin
      if BorderStyle = bsSingle then
        BtnRect := Bounds(Width - FButton.Width - 2, 2,
          FButton.Width, Height - 4)
      else
        BtnRect := Bounds(Width - FButton.Width, 0,
          FButton.Width, Height);
    end;
  end
  else
    BtnRect := Bounds(Width - FButton.Width, 0, FButton.Width, Height);
  with BtnRect do
    FBtnControl.SetBounds(Left, Top, Right - Left, Bottom - Top);
  FButton.Height := FBtnControl.Height;
  SetEditRect;
end;

procedure TCustomComboEdit.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  UpdateBtnBounds;
end;

procedure TCustomComboEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    MinHeight := GetMinHeight;
    { text edit bug: if size to less than MinHeight, then edit ctrl does
      not display the text }
    if Height < MinHeight then
    begin
      Height := MinHeight;
      Exit;
    end;
  end;
  UpdateBtnBounds;
end;

function TCustomComboEdit.GetTextHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    GetTextMetrics(DC, SysMetrics);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
  if SysMetrics.tmHeight < Metrics.tmHeight then Result := SysMetrics.tmHeight
    else Result := Metrics.tmHeight;
end;

function TCustomComboEdit.GetMinHeight: Integer;
var
  I: Integer;
begin
  I := GetTextHeight;
  Result := I + GetSystemMetricsForControl(Self, SM_CYBORDER) * 4 + 1;
end;

procedure TCustomComboEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  // Among other, this counters the EM_SETMARGINS call in TCustomEdit.WMSetFont.
  // Equivalent to TCustomButtonedEdit.WndProc.
  if HandleAllocated then UpdateBtnBounds;
end;

procedure TCustomComboEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FButton.Enabled := Enabled;
end;

procedure TCustomComboEdit.CNCtlColor(var Message: TMessage);
var
  TextColor: Longint;
begin
  inherited;
  if NewStyleControls then begin
    TextColor := ColorToRGB(Font.Color);
    if not Enabled and (ColorToRGB(Color) <> ColorToRGB(clGrayText)) then
      TextColor := ColorToRGB(clGrayText);
    SetTextColor(Message.WParam, TextColor);
  end;
end;

procedure TCustomComboEdit.EditButtonClick(Sender: TObject);
begin
  ButtonClick;
end;

procedure TCustomComboEdit.DoClick;
begin
  EditButtonClick(Self);
end;

procedure TCustomComboEdit.ButtonClick;
begin
  if Assigned(FOnButtonClick) then FOnButtonClick(Self);
end;

function TCustomComboEdit.BtnWidthStored: Boolean;
begin
  Result := ButtonWidth <> DefEditBtnWidth;
end;

{ TFileDirEdit }

procedure TFileDirEdit.DoBeforeDialog(var FileName: string;
  var Action: Boolean);
begin
  if Assigned(FOnBeforeDialog) then FOnBeforeDialog(Self, FileName, Action);
end;

procedure TFileDirEdit.DoAfterDialog(var FileName: string;
  var Action: Boolean);
begin
  if Assigned(FOnAfterDialog) then FOnAfterDialog(Self, FileName, Action);
end;

procedure TFileDirEdit.CreateHandle;
begin
  inherited CreateHandle;
  if FAcceptFiles then SetDragAccept(True);
end;

procedure TFileDirEdit.DestroyWindowHandle;
begin
  SetDragAccept(False);
  inherited DestroyWindowHandle;
end;

procedure TFileDirEdit.SetDragAccept(Value: Boolean);
begin
  if not (csDesigning in ComponentState) and (Handle <> 0) then
    DragAcceptFiles(Handle, Value);
end;

procedure TFileDirEdit.SetAcceptFiles(Value: Boolean);
begin
  if FAcceptFiles <> Value then begin
    SetDragAccept(Value);
    FAcceptFiles := Value;
  end;
end;

procedure TFileDirEdit.DisableSysErrors;
begin
  FErrMode := SetErrorMode(SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS);
end;

procedure TFileDirEdit.EnableSysErrors;
begin
  SetErrorMode(FErrMode);
  FErrMode := 0;
end;

procedure TFileDirEdit.WMDropFiles(var Msg: TWMDropFiles);
var
  AFileName: array[0..255] of Char;
  I, Num: Cardinal;
begin
  Msg.Result := 0;
  try
    Num := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);
    if Num > 0 then begin
      ClearFileList;
      for I := 0 to Num - 1 do begin
        DragQueryFile(Msg.Drop, I, PChar(@AFileName), Pred(SizeOf(AFileName)));
        ReceptFileDir(StrPas(AFileName));
        if not FMultipleDirs then Break;
      end;
      if Assigned(FOnDropFiles) then FOnDropFiles(Self);
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;

procedure TFileDirEdit.ClearFileList;
begin
end;

{ TFilenameEdit }

function StrPAlloc(const S: string): PChar;
begin
  Result := StrPCopy(StrAlloc(Length(S) + 1), S);
end;

function GetParamStr(P: PChar; var Param: string): PChar;
var
  Len: Integer;
  Buffer: array[Byte] of Char;
begin
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do Inc(P);
    if (P[0] = '"') and (P[1] = '"') then Inc(P, 2) else Break;
  end;
  Len := 0;
  while P[0] > ' ' do
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Buffer[Len] := P[0];
        Inc(Len);
        Inc(P);
      end;
      if P[0] <> #0 then Inc(P);
    end else
    begin
      Buffer[Len] := P[0];
      Inc(Len);
      Inc(P);
    end;
  SetString(Param, Buffer, Len);
  Result := P;
end;

function ParamCountFromCommandLine(CmdLine: PChar): Integer;
var
  S: string;
  P: PChar;
begin
  P := CmdLine;
  Result := 0;
  while True do
  begin
    P := GetParamStr(P, S);
    if S = '' then Break;
    Inc(Result);
  end;
end;

function ParamStrFromCommandLine(CmdLine: PChar; Index: Integer): string;
var
  P: PChar;
begin
  P := CmdLine;
  while True do
  begin
    P := GetParamStr(P, Result);
    if (Index = 0) or (Result = '') then Break;
    Dec(Index);
  end;
end;

procedure SplitCommandLine(const CmdLine: string; var ExeName,
  Params: string);
var
  Buffer: PChar;
  Cnt, I: Integer;
  S: string;
begin
  ExeName := '';
  Params := '';
  Buffer := StrPAlloc(CmdLine);
  try
    Cnt := ParamCountFromCommandLine(Buffer);
    if Cnt > 0 then begin
      ExeName := ParamStrFromCommandLine(Buffer, 0);
      for I := 1 to Cnt - 1 do begin
        S := ParamStrFromCommandLine(Buffer, I);
        if Pos(' ', S) > 0 then S := '"' + S + '"';
        Params := Params + S;
        if I < Cnt - 1 then Params := Params + ' ';
      end;
    end;
  finally
    StrDispose(Buffer);
  end;
end;

constructor TFilenameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateEditDialog;
end;

procedure TFilenameEdit.CreateEditDialog;
var
  NewDialog: TOpenDialog;
begin
  case FDialogKind of
    dkOpen: NewDialog := TOpenDialog.Create(Self);
    dkOpenPicture: NewDialog := TOpenPictureDialog.Create(Self);
    dkSavePicture: NewDialog := TSavePictureDialog.Create(Self);
    else {dkSave} NewDialog := TSaveDialog.Create(Self);
  end;

  try
    if FDialog <> nil then begin
      with NewDialog do begin
        DefaultExt := FDialog.DefaultExt;
        FileEditStyle := FDialog.FileEditStyle;
        FileName := FDialog.FileName;
        Filter := FDialog.Filter;
        FilterIndex := FDialog.FilterIndex;
        InitialDir := FDialog.InitialDir;
        HistoryList := FDialog.HistoryList;
        Files.Assign(FDialog.Files);
        Options := FDialog.Options;
        Title := FDialog.Title;
      end;
      FDialog.Free;
    end
    else begin
      NewDialog.Title := SBrowse;
      NewDialog.Filter := SDefaultFilter;
      NewDialog.Options := [ofHideReadOnly];
    end;
  finally
    FDialog := NewDialog;
  end;
end;

function TFilenameEdit.IsCustomTitle: Boolean;
begin
  Result := CompareStr(SBrowse, FDialog.Title) <> 0;
end;

function TFilenameEdit.IsCustomFilter: Boolean;
begin
  Result := CompareStr(SDefaultFilter, FDialog.Filter) <> 0;
end;

procedure TFilenameEdit.ButtonClick;
var
  Temp: string;
  Action: Boolean;
begin
  inherited ButtonClick;
  Temp := inherited Text;
  Action := True;
  DoBeforeDialog(Temp, Action);
  if not Action then Exit;
  if ValidFileName(Temp) then
    try
      if DirectoryExists(ExtractFilePath(Temp)) then
        SetInitialDir(ExtractFilePath(Temp));
      if (ExtractFileName(Temp) = '') or
        not ValidFileName(ExtractFileName(Temp)) then Temp := '';
      FDialog.FileName := Temp;
    except
      { ignore any exceptions }
    end;
  FDialog.HelpContext := Self.HelpContext;
  DisableSysErrors;
  try
    Action := FDialog.Execute;
  finally
    EnableSysErrors;
  end;
  if Action then Temp := FDialog.FileName;
  if CanFocus then SetFocus;
  DoAfterDialog(Temp, Action);
  if Action then begin
    inherited Text := Temp;
    SetInitialDir(ExtractFilePath(FDialog.FileName));
  end;
end;

function TFilenameEdit.GetFileName: string;
begin
  Result := inherited Text;
end;

procedure TFilenameEdit.SetFileName(const Value: string);
begin
  if (Value = '') or ValidFileName(Value) then begin
    inherited Text := Value;
    ClearFileList;
  end
  else raise EComboEditError.CreateFmt(SInvalidFilename, [Value]);
end;

procedure TFilenameEdit.ClearFileList;
begin
  FDialog.Files.Clear;
end;

procedure TFilenameEdit.ReceptFileDir(const AFileName: string);
begin
  if FMultipleDirs then begin
    if FDialog.Files.Count = 0 then SetFileName(AFileName);
    FDialog.Files.Add(AFileName);
  end
  else SetFileName(AFileName);
end;

function TFilenameEdit.GetDialogFiles: TStrings;
begin
  Result := FDialog.Files;
end;

function TFilenameEdit.GetDefaultExt: TFileExt;
begin
  Result := FDialog.DefaultExt;
end;

function TFilenameEdit.GetFileEditStyle: TFileEditStyle;
begin
  Result := FDialog.FileEditStyle;
end;

function TFilenameEdit.GetFilter: string;
begin
  Result := FDialog.Filter;
end;

function TFilenameEdit.GetFilterIndex: Integer;
begin
  Result := FDialog.FilterIndex;
end;

function TFilenameEdit.GetInitialDir: string;
begin
  Result := FDialog.InitialDir;
end;

function TFilenameEdit.GetHistoryList: TStrings;
begin
  Result := FDialog.HistoryList;
end;

function TFilenameEdit.GetOptions: TOpenOptions;
begin
  Result := FDialog.Options;
end;

function TFilenameEdit.GetDialogTitle: string;
begin
  Result := FDialog.Title;
end;

procedure TFilenameEdit.SetDialogKind(Value: TFileDialogKind);
begin
  if FDialogKind <> Value then begin
    FDialogKind := Value;
    CreateEditDialog;
  end;
end;

procedure TFilenameEdit.SetDefaultExt(Value: TFileExt);
begin
  FDialog.DefaultExt := Value;
end;

procedure TFilenameEdit.SetFileEditStyle(Value: TFileEditStyle);
begin
  FDialog.FileEditStyle := Value;
end;

procedure TFilenameEdit.SetFilter(const Value: string);
begin
  FDialog.Filter := Value;
end;

procedure TFilenameEdit.SetFilterIndex(Value: Integer);
begin
  FDialog.FilterIndex := Value;
end;

procedure TFilenameEdit.SetInitialDir(const Value: string);
begin
  FDialog.InitialDir := Value;
end;

procedure TFilenameEdit.SetHistoryList(Value: TStrings);
begin
  FDialog.HistoryList := Value;
end;

procedure TFilenameEdit.SetOptions(Value: TOpenOptions);
begin
  if Value <> FDialog.Options then begin
    FDialog.Options := Value;
    FMultipleDirs := ofAllowMultiSelect in FDialog.Options;
    if not FMultipleDirs then ClearFileList;
  end;
end;

procedure TFilenameEdit.SetDialogTitle(const Value: string);
begin
  FDialog.Title := Value;
end;

{ TDirectoryEdit }

constructor TDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TDirectoryEdit.ButtonClick;
var
  Temp: string;
  Action: Boolean;
begin
  inherited ButtonClick;
  Temp := Text;
  Action := True;
  DoBeforeDialog(Temp, Action);
  if not Action then Exit;
  if (Temp = '') then begin
    if (InitialDir <> '') then Temp := InitialDir
    else Temp := '\';
  end;
  if not DirectoryExists(Temp) then Temp := '\';
  DisableSysErrors;
  try
    Action := SelectDirectory(FDialogText, '', Temp);
  finally
    EnableSysErrors;
  end;
  if CanFocus then SetFocus;
  DoAfterDialog(Temp, Action);
  if Action then begin
    SelText := '';
    if (Text = '') or not MultipleDirs then Text := Temp
    else Text := Text + ';' + Temp;
    if (Temp <> '') and DirectoryExists(Temp) then InitialDir := Temp;
  end;
end;

procedure TDirectoryEdit.ReceptFileDir(const AFileName: string);
var
  Temp: string;
begin
  if FileExists(ApiPath(AFileName)) then Temp := ExtractFilePath(AFileName)
  else Temp := AFileName;
  if (Text = '') or not MultipleDirs then Text := Temp
  else Text := Text + ';' + Temp;
end;

initialization
end.
