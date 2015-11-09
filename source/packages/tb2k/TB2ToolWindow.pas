unit TB2ToolWindow;

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

  $jrsoftware: tb2k/Source/TB2ToolWindow.pas,v 1.18 2005/01/06 03:56:50 jr Exp $
}

interface

{$I TB2Ver.inc}

uses
  Windows, Classes, Graphics, Controls, TB2Dock;

type
  { TTBToolWindow }

  TTBToolWindow = class(TTBCustomDockableWindow)
  private
    FMinClientWidth, FMinClientHeight, FMaxClientWidth, FMaxClientHeight: Integer;
    FBarHeight, FBarWidth: Integer;
    function CalcSize(ADock: TTBDock): TPoint;
    function GetClientAreaWidth: Integer;
    procedure SetClientAreaWidth(Value: Integer);
    function GetClientAreaHeight: Integer;
    procedure SetClientAreaHeight(Value: Integer);
    procedure SetClientAreaSize(AWidth, AHeight: Integer);
  protected
    function DoArrange(CanMoveControls: Boolean; PreviousDockType: TTBDockType;
      NewFloating: Boolean; NewDock: TTBDock): TPoint; override;
    procedure GetBaseSize(var ASize: TPoint); override;
    procedure GetMinMaxSize(var AMinClientWidth, AMinClientHeight,
      AMaxClientWidth, AMaxClientHeight: Integer); override;
    procedure Paint; override;
    procedure SizeChanging(const AWidth, AHeight: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure ReadPositionData(const Data: TTBReadPositionData); override;
    procedure WritePositionData(const Data: TTBWritePositionData); override;
  published
    property ActivateParent;
    property Align;
    property Anchors;
    property BorderStyle;
    property Caption;
    property Color;
    property CloseButton;
    property CloseButtonWhenDocked;
    property ClientAreaHeight: Integer read GetClientAreaHeight write SetClientAreaHeight;
    property ClientAreaWidth: Integer read GetClientAreaWidth write SetClientAreaWidth;
    property CurrentDock;
    property DefaultDock;
    property DockableTo;
    property DockMode;
    property DockPos;
    property DockRow;
    property DragHandleStyle;
    property FloatingMode;
    property Font;
    property FullSize;
    property HideWhenInactive;
    property LastDock;
    property MaxClientHeight: Integer read FMaxClientHeight write FMaxClientHeight default 0;
    property MaxClientWidth: Integer read FMaxClientWidth write FMaxClientWidth default 0;
    property MinClientHeight: Integer read FMinClientHeight write FMinClientHeight default 32;
    property MinClientWidth: Integer read FMinClientWidth write FMinClientWidth default 32;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Resizable;
    property ShowCaption;
    property ShowHint;
    property Stretch;
    property SmoothDrag;
    property TabOrder;
    property UseLastDock;
    {}{property Version;}
    property Visible;

    property OnClose;
    property OnCloseQuery;
    {$IFDEF JR_D5}
    property OnContextPopup;
    {$ENDIF}
    property OnDragDrop;
    property OnDragOver;
    property OnDockChanged;
    property OnDockChanging;
    property OnDockChangingHidden;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMove;
    property OnRecreated;
    property OnRecreating;
    property OnResize;
    property OnVisibleChanged;
  end;

implementation

const
  { Constants for TTBToolWindow-specific registry values. Do not localize! }
  rvClientWidth = 'ClientWidth';
  rvClientHeight = 'ClientHeight';


{ TTBToolWindow }

constructor TTBToolWindow.Create(AOwner: TComponent);
begin
  inherited;
  FMinClientWidth := 32;
  FMinClientHeight := 32;
  { Initialize the client size to 32x32 }
  SetBounds(Left, Top, 32, 32);
end;

procedure TTBToolWindow.Paint;
var
  R: TRect;
begin
  { Draw dotted border in design mode }
  if csDesigning in ComponentState then
    with Canvas do begin
      R := ClientRect;
      Pen.Style := psDot;
      Pen.Color := clBtnShadow;
      Brush.Style := bsClear;
      Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      Pen.Style := psSolid;
    end;
end;

procedure TTBToolWindow.ReadPositionData(const Data: TTBReadPositionData);
begin
  inherited;
  { Restore ClientAreaWidth/ClientAreaHeight variables }
  if Resizable then
    with Data do
      SetClientAreaSize(ReadIntProc(Name, rvClientWidth, FBarWidth, ExtraData),
        ReadIntProc(Name, rvClientHeight, FBarHeight, ExtraData));
end;

procedure TTBToolWindow.WritePositionData(const Data: TTBWritePositionData);
begin
  inherited;
  { Write values of FBarWidth/FBarHeight }
  with Data do begin
    WriteIntProc(Name, rvClientWidth, FBarWidth, ExtraData);
    WriteIntProc(Name, rvClientHeight, FBarHeight, ExtraData);
  end;
end;

procedure TTBToolWindow.GetMinMaxSize(var AMinClientWidth, AMinClientHeight,
  AMaxClientWidth, AMaxClientHeight: Integer);
begin
  AMinClientWidth := FMinClientWidth;
  AMinClientHeight := FMinClientHeight;
  AMaxClientWidth := FMaxClientWidth;
  AMaxClientHeight := FMaxClientHeight;
end;

procedure TTBToolWindow.SizeChanging(const AWidth, AHeight: Integer);
begin
  FBarWidth := AWidth;
  if Parent <> nil then Dec(FBarWidth, Width - ClientWidth);
  FBarHeight := AHeight;
  if Parent <> nil then Dec(FBarHeight, Height - ClientHeight);
end;

function TTBToolWindow.CalcSize(ADock: TTBDock): TPoint;
begin
  Result.X := FBarWidth;
  Result.Y := FBarHeight;
  if Assigned(ADock) then
    if FullSize then
    begin
      { If docked and full size, return the size corresponding to docked size }
      if not(ADock.Position in [dpLeft, dpRight]) then
        Result.X := ADock.ClientWidth - (Width - ClientWidth)
      else
        Result.Y := ADock.ClientHeight - (Height - ClientHeight);
    end
    else if Stretch then
    begin
      { If docked and stretching, return the minimum size so that the toolbar
        can shrink below FBarWidth/FBarHeight }
      if not(ADock.Position in [dpLeft, dpRight]) then
        Result.X := FMinClientWidth
      else
        Result.Y := FMinClientHeight;
    end;
end;

procedure TTBToolWindow.GetBaseSize(var ASize: TPoint);
begin
  ASize := CalcSize(CurrentDock);
end;

function TTBToolWindow.DoArrange(CanMoveControls: Boolean;
  PreviousDockType: TTBDockType; NewFloating: Boolean; NewDock: TTBDock): TPoint;
begin
  Result := CalcSize(NewDock);
end;

function TTBToolWindow.GetClientAreaWidth: Integer;
begin
  if Parent = nil then
    Result := Width
  else
    Result := ClientWidth;
end;

procedure TTBToolWindow.SetClientAreaWidth(Value: Integer);
begin
  SetClientAreaSize(Value, ClientAreaHeight);
end;

function TTBToolWindow.GetClientAreaHeight: Integer;
begin
  if Parent = nil then
    Result := Height
  else
    Result := ClientHeight;
end;

procedure TTBToolWindow.SetClientAreaHeight(Value: Integer);
begin
  SetClientAreaSize(ClientAreaWidth, Value);
end;

procedure TTBToolWindow.SetClientAreaSize(AWidth, AHeight: Integer);
var
  Client: TRect;
begin
  if Parent = nil then
    SetBounds(Left, Top, AWidth, AHeight)
  else begin
    Client := GetClientRect;
    SetBounds(Left, Top, Width - Client.Right + AWidth,
      Height - Client.Bottom + AHeight);
  end;
end;

end.
