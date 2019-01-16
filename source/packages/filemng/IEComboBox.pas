unit IEComboBox;

{==================================================================

 Components TIECustomCombobox  /  Version 1.4  / January 2000
            TIEComboBox
            TIEDriveBox
 ==================================================================


    Description:
    ============
    TIECustomComboBox is a combobox with variable width of the dropdown list.

    TIEComboBox publishes the properties of the class TIECustomComboBox.


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

type
// =======================================================================
// Class TIECustomComboBox
// =======================================================================
  TIECustomComboBox = class(TCustomComboBox)
  protected
    procedure DropDown; override;
    function GetMaxItemWidth: Integer;
  end;


// =======================================================================
// Class TIEComboBox
// =======================================================================
  TIEComboBox = class(TIECustomComboBox)
  published
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

procedure Register;

implementation

uses
  SysUtils, Forms, Dialogs, Imglist, ShellAPI, CommCtrl, Math, Windows, PasTools;

procedure Register;
begin
  RegisterComponents('DriveDir', [TIEComboBox]);
end;

// =======================================================================
// Class TIECustomComboBox
// =======================================================================

procedure TIECustomComboBox.DropDown;
var
  ItemWidth: Integer;
begin
  {set the width of the list box to 8 pixels > than the
   widest string to buffer the right side. Anything less than
   8 for some reason touches the end of the item on high-res
   monitor settings.}
  // The same code is in THistoryComboBox.DropDown
  ItemWidth := GetMaxItemWidth + ScaleByPixelsPerInch(8, Self);
  if Items.Count > DropDownCount then
    Inc(ItemWidth, GetSystemMetricsForControl(Self, SM_CXVSCROLL));
  Self.Perform(CB_SETDROPPEDWIDTH, ItemWidth, 0);

  inherited;
end; {TIECustomComboBox.DropDown}

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
      GetTextExtentPoint32(DC, PChar(Items[Index]), Length(Items[Index]), Size);
      if Size.Cx > Result then Result := Size.Cx;
    end;
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
end; {TIECustomComboBox.GetMaxItemWidth}

initialization
end.
