//---------------------------------------------------------------------------
#ifndef UnixPathComboBoxH
#define UnixPathComboBoxH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>

#include <CustomPathComboBox.hpp>
//---------------------------------------------------------------------------
#define DEFAULT_ROOTNAME "/ <root>"
//---------------------------------------------------------------------------
class PACKAGE TUnixPathComboBox : public TCustomPathComboBox
{
private:
  AnsiString FRootName;
  Boolean __fastcall IsRootNameStored();
  void __fastcall SetRootName(AnsiString value);
protected:
  virtual void __fastcall CreateWnd();
  virtual Integer __fastcall GetItemImage(Integer Index);
  virtual Integer __fastcall GetItemIndent(Integer Index);
  virtual void __fastcall PathChanged();
  void __fastcall ResetItems();
  virtual void __fastcall SetPath(AnsiString Value);
public:
  __fastcall TUnixPathComboBox(TComponent* Owner);
__published:
  __property AnsiString RootName = { read = FRootName, write = SetRootName, stored = IsRootNameStored };

  __property OnCloseUp;

  __property Align;
  __property Anchors;
  __property BiDiMode;
  __property Color;
  __property Constraints;
  __property Ctl3D;
  __property DragCursor;
  __property DragKind;
  __property DragMode;
  __property DropDownCount;
  __property Enabled;
  __property Font;
  __property ImeMode;
  __property ImeName;
  __property ParentBiDiMode;
  __property ParentColor;
  __property ParentCtl3D;
  __property ParentFont;
  __property ParentShowHint;
  __property PopupMenu;
  __property ShowHint;
  __property TabOrder;
  __property TabStop;
  __property Visible;
  __property OnChange;
  __property OnClick;
  __property OnDblClick;
  __property OnDragDrop;
  __property OnDragOver;
  __property OnDrawItem;
  __property OnDropDown;
  __property OnEndDock;
  __property OnEndDrag;
  __property OnEnter;
  __property OnExit;
  __property OnKeyDown;
  __property OnKeyPress;
  __property OnKeyUp;
  __property OnStartDock;
  __property OnStartDrag;
};
//---------------------------------------------------------------------------
#endif
 