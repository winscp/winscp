unit TBXStrEdit;

// TBX Package
// Copyright 2001-2004 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// $Id: TBXStrEdit.pas 7 2004-02-21 06:07:53Z  $

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls;

type
  TStrEditDlg = class(TForm)
    Memo: TMemo;
    OK: TButton;
    Cancel: TButton;
    procedure MemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure ArrangeControls;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

procedure TStrEditDlg.ArrangeControls;
var
  R, B: TRect;
  W, H: Integer;
begin
  R := ClientRect;
  InflateRect(R, -16, -16);
  B := R;
  W := 60; H := 23;
  B.Left := B.Right - W;
  B.Top := B.Bottom - H;
  Cancel.BoundsRect := B;
  B.Right := B.Left - 4;
  B.Left := B.Right - W;
  OK.BoundsRect := B;
  Dec(R.Bottom, H + 8);
  Memo.BoundsRect := R;
end;

constructor TStrEditDlg.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  AutoScroll := False;
  Scaled := False;
  Position := poScreenCenter;
  Memo := TMemo.Create(Self);
  with Memo do
  begin
    ScrollBars := ssBoth;
    OnKeyDown := MemoKeyDown;
  end;
  OK := TButton.Create(Self);
  with OK do
  begin
    Caption := 'OK';
    Default := True;
    ModalResult := mrOk;
  end;
  Cancel := TButton.Create(Self);
  with Cancel do
  begin
    Cancel := True;
    Caption := 'Cancel';
    ModalResult := mrCancel;
  end;
end;

procedure TStrEditDlg.MemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Cancel.Click;
end;

procedure TStrEditDlg.Resize;
begin
  inherited;
  ArrangeControls;
end;

end.
