unit TB2MRU;

{
  Toolbar2000
  Copyright (C) 1998-2005 by Jordan Russell
  All rights reserved.

  The contents of this file are subject to the "Toolbar2000 License"; you may
  not use or distribute this file except in compliance with the
  "Toolbar2000 License". A copy of the "Toolbar2000 License" may be found in
  TB2k-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/TB2k-LICENSE.txt

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License (the "GPL"), in which case the provisions of the
  GPL are applicable instead of those in the "Toolbar2000 License". A copy of
  the GPL may be found in GPL-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/GPL-LICENSE.txt
  If you wish to allow use of your version of this file only under the terms of
  the GPL and not to allow others to use your version of this file under the
  "Toolbar2000 License", indicate your decision by deleting the provisions
  above and replace them with the notice and other provisions required by the
  GPL. If you do not delete the provisions above, a recipient may use your
  version of this file under either the "Toolbar2000 License" or the GPL.

  $jrsoftware: tb2k/Source/TB2MRU.pas,v 1.23 2005/01/06 03:56:50 jr Exp $
}

interface

{$I TB2Ver.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TB2Item, IniFiles, Registry;

type
  TTBMRUListClickEvent = procedure(Sender: TObject; const Filename: String) of object;

  TTBMRUList = class(TComponent)
  private
    FAddFullPath: Boolean;
    FContainer: TTBCustomItem;
    FHidePathExtension: Boolean;
    FList: TStrings;
    FMaxItems: Integer;
    FOnChange: TNotifyEvent;
    FOnClick: TTBMRUListClickEvent;
    FPrefix: String;
    procedure ClickHandler(Sender: TObject);
    procedure SetHidePathExtension(Value: Boolean);
    procedure SetList(Value: TStrings);
    procedure SetMaxItems(Value: Integer);
  protected
    property Container: TTBCustomItem read FContainer;
    function GetFirstKey: Integer; virtual;
    function GetItemClass: TTBCustomItemClass; virtual;
    procedure SetItemCaptions; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(Filename: String);
    procedure Remove(const Filename: String);
    procedure LoadFromIni(Ini: TCustomIniFile; const Section: String);
    procedure LoadFromRegIni(Ini: TRegIniFile; const Section: String);
    procedure SaveToIni(Ini: TCustomIniFile; const Section: String);
    procedure SaveToRegIni(Ini: TRegIniFile; const Section: String);
  published
    { MaxItems must be published before Items }
    property AddFullPath: Boolean read FAddFullPath write FAddFullPath default True;
    property HidePathExtension: Boolean read FHidePathExtension write SetHidePathExtension default True;
    property MaxItems: Integer read FMaxItems write SetMaxItems default 4;
    property Items: TStrings read FList write SetList;
    property OnClick: TTBMRUListClickEvent read FOnClick write FOnClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Prefix: String read FPrefix write FPrefix;
  end;

  TTBMRUListItem = class(TTBCustomItem)
  private
    FMRUList: TTBMRUList;
    procedure SetMRUList(Value: TTBMRUList);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MRUList: TTBMRUList read FMRUList write SetMRUList;
    //property Caption;
    //property LinkSubitems;
  end;

implementation

uses
  TB2Common, TB2Consts, CommDlg;

type
  TTBMRUListStrings = class(TStrings)
  private
    FInternalList: TStrings;
    FMRUList: TTBMRUList;
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function Get(Index: Integer): String; override;
    function GetCount: Integer; override;
    function IndexOf(const S: String): Integer; override; 
    procedure Insert(Index: Integer; const S: String); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure Put(Index: Integer; const S: String); override;
  end;


{ TTBMRUListStrings }

constructor TTBMRUListStrings.Create;
begin
  inherited;
  FInternalList := TStringList.Create;
end;

destructor TTBMRUListStrings.Destroy;
begin
  inherited;
  FInternalList.Free;
end;

procedure TTBMRUListStrings.Changed;
begin
  if Assigned(FMRUList.FOnChange) and
     not(csLoading in FMRUList.ComponentState) then
    FMRUList.FOnChange(FMRUList);
end;

procedure TTBMRUListStrings.Clear;
var
  I: Integer;
begin
  for I := FInternalList.Count-1 downto 0 do
    Delete(I);
end;

procedure TTBMRUListStrings.Delete(Index: Integer);
begin
  FMRUList.FContainer[Index].Free;
  FInternalList.Delete(Index);
  FMRUList.SetItemCaptions;
  Changed;
end;

function TTBMRUListStrings.Get(Index: Integer): String;
begin
  Result := FInternalList[Index];
end;

function TTBMRUListStrings.GetCount: Integer;
begin
  Result := FInternalList.Count;
end;

function TTBMRUListStrings.IndexOf(const S: String): Integer;
begin
  { This is identical to TStrings.IndexOf except we use AnsiCompareFileName. }
  for Result := 0 to GetCount - 1 do
    if AnsiCompareFileName(Get(Result), S) = 0 then Exit;
  Result := -1;
end;

procedure TTBMRUListStrings.Insert(Index: Integer; const S: String);
var
  Item: TTBCustomItem;
begin
  Item := FMRUList.GetItemClass.Create(FMRUList.FContainer);
  Item.OnClick := FMRUList.ClickHandler;
  FMRUList.FContainer.Insert(Index, Item);
  FInternalList.Insert(Index, S);
  FMRUList.SetItemCaptions;
  Changed;
end;

procedure TTBMRUListStrings.Move(CurIndex, NewIndex: Integer);
begin
  FInternalList.Move(CurIndex, NewIndex);
  FMRUList.FContainer.Move(CurIndex, NewIndex);
  FMRUList.SetItemCaptions;
  Changed;
end;

procedure TTBMRUListStrings.Put(Index: Integer; const S: String);
begin
  FInternalList[Index] := S;
  FMRUList.SetItemCaptions;
  Changed;
end;


{ TTBMRUList }

constructor TTBMRUList.Create(AOwner: TComponent);
begin
  inherited;
  FAddFullPath := True;
  FHidePathExtension := True;
  FMaxItems := 4;
  FPrefix := 'MRU';
  FList := TTBMRUListStrings.Create;
  TTBMRUListStrings(FList).FMRUList := Self;
  FContainer := TTBCustomItem.Create(nil);
end;

destructor TTBMRUList.Destroy;
begin
  FContainer.Free;
  FList.Free;
  inherited;
end;

procedure TTBMRUList.Add(Filename: String);
var
  I: Integer;
begin
  if AddFullPath then
    Filename := ExpandFileName(Filename);
  { If Filename is already in the MRU list, move it to the top }
  I := FList.IndexOf(Filename);
  if I <> -1 then begin
    if I > 0 then
      FList.Move(I, 0);
    FList[0] := Filename;  { ...in case the capitalization changed }
  end
  else
    FList.Insert(0, Filename);
end;

procedure TTBMRUList.Remove(const Filename: String);
var
  I: Integer;
begin
  I := FList.IndexOf(Filename);
  if I <> -1 then
    FList.Delete(I);
end;

procedure TTBMRUList.LoadFromIni(Ini: TCustomIniFile; const Section: String);
var
  I: Integer;
  S: String;
begin
  FList.Clear;
  for I := 1 to FMaxItems do begin
    S := Ini.ReadString(Section, FPrefix + IntToStr(I), '');
    if S <> '' then
      FList.Add(S);
  end;
end;

procedure TTBMRUList.LoadFromRegIni(Ini: TRegIniFile; const Section: String);
var
  I: Integer;
  S: String;
begin
  FList.Clear;
  for I := 1 to FMaxItems do begin
    S := Ini.ReadString(Section, FPrefix + IntToStr(I), '');
    if S <> '' then
      FList.Add(S);
  end;
end;

procedure TTBMRUList.SaveToIni(Ini: TCustomIniFile; const Section: String);
var
  I: Integer;
begin
  for I := 1 to FMaxItems do begin
    if I <= FList.Count then
      Ini.WriteString(Section, FPrefix + IntToStr(I), FList[I-1])
    else
      Ini.DeleteKey(Section, FPrefix + IntToStr(I));
  end;
end;

procedure TTBMRUList.SaveToRegIni(Ini: TRegIniFile; const Section: String);
var
  I: Integer;
begin
  for I := 1 to FMaxItems do begin
    if I <= FList.Count then
      Ini.WriteString(Section, FPrefix + IntToStr(I), FList[I-1])
    else
      Ini.DeleteKey(Section, FPrefix + IntToStr(I));
  end;
end;

procedure TTBMRUList.SetItemCaptions;
var
  I, J, N: Integer;
  Key: Char;
  S: String;
  Buf: array[0..MAX_PATH-1] of Char;
begin
  while FList.Count > FMaxItems do
    FList.Delete(FList.Count-1);
  N := GetFirstKey;
  for I := 0 to FContainer.Count-1 do begin
    Key := #0;
    if N < 9 then
      Key := Chr(Ord('1') + N)
    else begin
      { No more numbers; try letters }
      J := N - 9;
      if J < 26 then
        Key := Chr(Ord('A') + J);
    end;
    S := FList[I];
    if HidePathExtension and (GetFileTitle(PChar(S), Buf, SizeOf(Buf)) = 0) then
      S := Buf;
    S := EscapeAmpersands(S);
    if Key <> #0 then
      FContainer[I].Caption := Format('&%s %s', [Key, S])
    else
      FContainer[I].Caption := S;
    Inc(N);
  end;
end;

procedure TTBMRUList.ClickHandler(Sender: TObject);
var
  I: Integer;
begin
  I := FContainer.IndexOf(TTBCustomItem(Sender));
  if I <> -1 then begin
    if I > 0 then
      FList.Move(I, 0);
    if Assigned(FOnClick) then
      FOnClick(Self, FList[0]);
  end;
end;

procedure TTBMRUList.SetHidePathExtension(Value: Boolean);
begin
  if FHidePathExtension <> Value then begin
    FHidePathExtension := Value;
    SetItemCaptions;
  end;
end;

procedure TTBMRUList.SetList(Value: TStrings);
begin
  FList.Assign(Value);
end;

procedure TTBMRUList.SetMaxItems(Value: Integer);
begin
  FMaxItems := Value;
  SetItemCaptions;
end;

function TTBMRUList.GetItemClass: TTBCustomItemClass;
begin
  Result := TTBCustomItem;
end;

function TTBMRUList.GetFirstKey: Integer;
begin
  Result := 0;
end;


{ TTBMRUListItem }

constructor TTBMRUListItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle + [tbisEmbeddedGroup];
  Caption := STBMRUListItemDefCaption;
end;

procedure TTBMRUListItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FMRUList) and (Operation = opRemove) then
    SetMRUList(nil);
end;

procedure TTBMRUListItem.SetMRUList(Value: TTBMRUList);
begin
  if FMRUList <> Value then begin
    FMRUList := Value;
    if Assigned(FMRUList) then begin
      Value.FreeNotification(Self);
      LinkSubitems := FMRUList.FContainer;
    end
    else
      LinkSubitems := nil;
  end;
end;

end.
