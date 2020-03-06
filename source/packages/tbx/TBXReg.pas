unit TBXReg;

// TBX Package
// Copyright 2001-2004 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// Id: TBXReg.pas 16 2004-05-26 02:02:55Z Alex@ZEISS

interface

{$I TB2Ver.inc}

uses
  Windows, Classes, Controls, SysUtils, Graphics, ImgList, Dialogs,
  DesignIntf, DesignEditors, VCLEditors,
  TB2Reg, TB2Toolbar, TB2Item, TBX, TB2DsgnItemEditor,
  TBXExtItems, TBXLists, TBXToolPals, TBXStatusBars;

procedure Register;

type
  TThemeProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TMLStringProperty = class(TCaptionProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TTBXColorProperty = class(TColorProperty)
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);
  end;

  TTBXStatusBarEditor = class(TDefaultEditor)
  protected
    procedure GetPanelsProp(const Prop: IProperty);
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TTBXItemsEditor = class(TTBItemsEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

uses
  Forms, TBXThemes, TBXStrEdit, TBXUtils, TypInfo, TB2Version;

{ TThemeProperty }

function TThemeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

procedure TThemeProperty.GetValues(Proc: TGetStrProc);
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  GetAvailableTBXThemes(SL);
  for I := 0 to SL.Count - 1 do Proc(SL[I]);
  SL.Free;
end;

{ TMLStringProperty }

function WordCount(const S: string; const Delims: TSysCharSet): Integer;
var
  L, I: Cardinal;
begin
  Result := 0;
  I := 1;
  L := Length(S);
  while I <= L do
  begin
    while (I <= L) and {MP}CharInSet(S[I], Delims) do Inc(I);
    if I <= L then Inc(Result);
    while (I <= L) and not {MP}CharInSet(S[I], Delims) do Inc(I);
  end;
end;

function WordPosition(const N: Integer; const S: string;
  const WordDelims: TSysCharSet): Integer;
var
  Count, I: Integer;
begin
  Count := 0;
  I := 1;
  Result := 0;
  while (I <= Length(S)) and (Count <> N) do begin
    { skip over delimiters }
    while (I <= Length(S)) and {MP}CharInSet(S[I], WordDelims) do Inc(I);
    { if we're not beyond end of S, we're at the start of a word }
    if I <= Length(S) then Inc(Count);
    { if not finished, find the end of the current word }
    if Count <> N then
      while (I <= Length(S)) and not CharInSet(S[I], WordDelims) do Inc(I)
    else Result := I;
  end;
end;

function ExtractWord(N: Integer; const S: string;
  const WordDelims: TSysCharSet): string;
var
  I: Integer;
  Len: Integer;
begin
  Len := 0;
  I := WordPosition(N, S, WordDelims);
  if I <> 0 then
    { find the end of the current word }
    while (I <= Length(S)) and not CharInSet(S[I], WordDelims) do begin
      { add the I'th character to result }
      Inc(Len);
      SetLength(Result, Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  SetLength(Result, Len);
end;

procedure TMLStringProperty.Edit;
var
  Temp: string;
  Component: TPersistent;
  I, N: Integer;
begin
  with TStrEditDlg.Create(Application) do
  try
    Component := GetComponent(0);
    if Component is TComponent then Caption := TComponent(Component).Name + '.' + GetName
    else Caption := GetName;

    Temp := GetStrValue;
    N := WordCount(Temp, [#13, #10]);
    for I := 1 to N do Memo.Lines.Add(ExtractWord(I, Temp, [#13, #10]));

    Memo.MaxLength := GetEditLimit;
    if ShowModal = mrOk then
    begin
      Temp := Memo.Text;
      while (Length(Temp) > 0) and (Temp[Length(Temp)] < ' ') do
        System.Delete(Temp, Length(Temp), 1);
      SetStrValue(Temp);
    end;
  finally
    Free;
  end;
end;

function TMLStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ TTBXColorProperty }

function TTBXColorProperty.GetValue: string;
begin
  Result := TBXColorToString(TColor(GetOrdValue));
end;

procedure TTBXColorProperty.GetValues(Proc: TGetStrProc);
begin
  TBXGetColorValues(Proc);
end;

procedure TTBXColorProperty.SetValue(const Value: string);
begin
  SetOrdValue(TBXStringToColor(Value));
end;

procedure TTBXColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);

  function ColorToBorderColor(AColor: TColor): TColor;
  begin
    if IsDarkColor(AColor) then
    begin
      Result := AColor;
      SetContrast(Result, AColor, 40);
    end
    else Result := clBlack;
  end;

var
  R: TRect;
  C: TColor;
  OldPenColor, OldBrushColor: TColor;
begin
  R := ARect;
  with ACanvas do
  try
    OldPenColor := Pen.Color;
    OldBrushColor := Brush.Color;
    Pen.Color := Brush.Color;
    Rectangle(R);
    R.Right := (ARect.Bottom - ARect.Top) + ARect.Left;
    InflateRect(R, -1, -1);
    C := TBXStringToColor(Value);
    if C <> clNone then
    begin
      Brush.Color := C;
      Pen.Color := ColorToBorderColor(ColorToRGB(C));
      Rectangle(R);
    end
    else
    begin
      Brush.Color := clWindow;
      Pen.Color := clBtnShadow;
      Rectangle(R);
      MoveTo(R.Left, R.Bottom - 1);
      LineTo(R.Right - 1, R.Top);
      MoveTo(R.Left, R.Top);
      LineTo(R.Right, R.Bottom);
    end;
    Brush.Color := OldBrushColor;
    Pen.Color := OldPenColor;
  finally
    R.Left := R.Right;
    R.Right := ARect.Right;
    ACanvas.TextRect(R, R.Left + 1, R.Top + 1, Value);
  end;
end;

{ TTBXStatusBarEditor }

procedure TTBXStatusBarEditor.Edit;
var
  Components: IDesignerSelections;
begin
  Components := CreateSelectionList;
  Components.Add(Component);
  GetComponentProperties(Components, [tkClass], Designer, GetPanelsProp);
end;

procedure TTBXStatusBarEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then Edit;
end;

function TTBXStatusBarEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then Result := '&Panels Editor...';
end;

function TTBXStatusBarEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TTBXStatusBarEditor.GetPanelsProp(const Prop: IProperty);
begin
  if SameText(Prop.GetName, 'Panels') then Prop.Edit;
end;

{ TTBXItemsEditor }

procedure TTBXItemsEditor.ExecuteVerb(Index: Integer);
const
  AboutText =
    '%s'#13#10 +
    '©2001–2004 Alex A. Denisov'#13#10 +
    'For conditions of distribution and use, see TBX documentation.'#13#10 +
    #13#10 +
    'Running on'#13#10 +
    '%s'#13#10 +
    '©1998-2004 by Jordan Russell'#13#10 +
    'For conditions of distribution and use, see Toolbar2000 documentation.'#13#10 +
    #13#10 +
    'Visit https://jrsoftware.org/ for the latest versions of Toolbar2000'#13#10 +
    '';
begin
  case Index of
    0: Edit;
    1:
      begin
        MessageDlg(
          Format(AboutText,
          [TBXVersionText, Toolbar2000VersionPropText]),
          mtInformation, [mbOK], 0);
      end;
  end;
end;

{ THookObj }

type
  THookObj = class
    procedure HookProc(Sender: TTBItemEditForm);
  end;

var O: THookObj;

procedure THookObj.HookProc(Sender: TTBItemEditForm);
var
  TB: TTBToolbar;
  Item: TTBCustomItem;
  NewItem: TTBItem;
  S: string;
  I: Integer;
begin
  TB := TTBToolbar.Create(Sender);
  TB.Top := Sender.Height;
  TB.Parent := Sender;
  TB.Align := alTop;
  TB.Images := Sender.ToolbarItems.SubMenuImages;
  TB.ShowHint := True;

  for I := 0 to Sender.MoreMenu.Count - 1 do
  begin
    Item := Sender.MoreMenu.Items[I];
    if Item is TTBCustomItem then
    begin
      S := TTBCustomItemClass(Item.Tag).ClassName;
      if StrLComp(PChar(S), 'TTBX', 4) = 0 then
      begin
        NewItem := TTBItem.Create(TB);
        TB.Items.Add(NewItem);
        NewItem.Caption := Item.Caption;
        NewItem.ImageIndex := Item.ImageIndex;
        NewItem.Tag := Item.Tag;
        NewItem.Hint := S;
        NewItem.OnClick := Item.OnClick;
      end;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Toolbar2000', [TTBXDock, TTBXToolbar,
    TTBXPopupMenu, TTBXColorSet, TTBXStatusBar]);
  RegisterNoIcon([TTBXItem, TTBXSubMenuItem, TTBXSeparatorItem,
    TTBXLabelItem, TTBXColorItem,
    TTBXDropDownItem,
    TTBXComboBoxItem, TTBXStringList, TTBXColorPalette]);

  RegisterClasses([TTBXItem, TTBXSubMenuItem, TTBXSeparatorItem,
    TTBXLabelItem, TTBXColorItem,
    TTBXDropDownItem,
    TTBXComboBoxItem, TTBXStringList, TTBXColorPalette]);


  RegisterComponentEditor(TTBXToolbar, TTBXItemsEditor);
  RegisterComponentEditor(TTBXPopupMenu, TTBXItemsEditor);
  RegisterPropertyEditor(TypeInfo(string), TTBXCustomItem, 'Caption', TMLStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TTBXCustomItem, 'Hint', TMLStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TTBXLabelItem, 'Caption', TCaptionProperty);
  RegisterPropertyEditor(TypeInfo(string), TTBToolbar, 'ChevronHint', TMLStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TTBXToolbar, 'ChevronHint', TMLStringProperty);
  RegisterPropertyEditor(TypeInfo(TColor), TPersistent, '', TTBXColorProperty);

  RegisterComponentEditor(TTBXStatusBar, TTBXStatusBarEditor);

  TBRegisterItemClass(TTBXItem, 'New &TBX Item', HInstance);
  TBRegisterItemClass(TTBXSubMenuItem, 'New TBX Submenu Item', HInstance);
  TBRegisterItemClass(TTBXSeparatorItem, 'New TBX Separator Item', HInstance);
  TBRegisterItemClass(TTBXLabelItem, 'New TBX Label Item', HInstance);
  TBRegisterItemClass(TTBXColorItem, 'New TBX Color Item', HInstance);
  TBRegisterItemClass(TTBXDropDownItem, 'New TBX Drop Down Item', HInstance);
  TBRegisterItemClass(TTBXComboBoxItem, 'New TBX Combo Box Item', HInstance);
  TBRegisterItemClass(TTBXStringList, 'New TBX String List', HInstance);
  TBRegisterItemClass(TTBXColorPalette, 'New TBX Color Palette', HInstance);

end;

initialization
  O := THookObj.Create;
  TBUnregisterDsgnEditorHook(O.HookProc);
  TBRegisterDsgnEditorHook(O.HookProc);

finalization
  TBUnregisterDsgnEditorHook(O.HookProc);
  O.Free;
end.
