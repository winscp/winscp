// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'IEComboBox.pas' rev: 6.00

#ifndef IEComboBoxHPP
#define IEComboBoxHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Menus.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Types.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Iecombobox
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TIECloseUpEvent)(System::TObject* Sender, bool Canceled);

class DELPHICLASS TIECustomComboBox;
class PASCALIMPLEMENTATION TIECustomComboBox : public Stdctrls::TCustomComboBox 
{
	typedef Stdctrls::TCustomComboBox inherited;
	
private:
	int FDropDownFixedWidth;
	TIECloseUpEvent FOnCloseUp;
	bool FCanceled;
	bool FUseSystemImageList;
	Controls::TImageList* FSystemImageList;
	int __fastcall GetTopIndex(void);
	void __fastcall SetTopIndex(int Value);
	void __fastcall SetUseSystemImageList(bool Value);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Messages::TMessage &Message);
	
protected:
	virtual void __fastcall DrawItem(int Index, const Types::TRect &Rect, Windows::TOwnerDrawState State);
	virtual int __fastcall GetItemImage(int Index);
	virtual int __fastcall GetItemIndent(int Index);
	AnsiString __fastcall GetItemText(int Index);
	virtual AnsiString __fastcall GetItemTextEx(int Index, bool ForList);
	virtual Controls::TImageList* __fastcall ImageList(void);
	HIDESBASE MESSAGE void __fastcall CNCommand(Messages::TWMCommand &Message);
	virtual void __fastcall DoCloseUp(bool Canceled);
	DYNAMIC void __fastcall DropDown(void);
	int __fastcall GetMaxItemWidth(void);
	void __fastcall ResetItemHeight(void);
	
public:
	__fastcall virtual TIECustomComboBox(Classes::TComponent* AOwner);
	__fastcall virtual ~TIECustomComboBox(void);
	int __fastcall GetTextWidth(AnsiString Str);
	void __fastcall DoPreloadImages(void);
	__property int ItemImage[int Index] = {read=GetItemImage};
	__property int ItemIndent[int Index] = {read=GetItemIndent};
	__property AnsiString ItemText[int Index] = {read=GetItemText};
	__property int TopIndex = {read=GetTopIndex, write=SetTopIndex, nodefault};
	__property bool UseSystemImageList = {read=FUseSystemImageList, write=SetUseSystemImageList, nodefault};
	__property int DropDownFixedWidth = {read=FDropDownFixedWidth, write=FDropDownFixedWidth, default=0};
	__property TIECloseUpEvent OnCloseUp = {read=FOnCloseUp, write=FOnCloseUp};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TIECustomComboBox(HWND ParentWindow) : Stdctrls::TCustomComboBox(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TIEComboBox;
class PASCALIMPLEMENTATION TIEComboBox : public TIECustomComboBox 
{
	typedef TIECustomComboBox inherited;
	
__published:
	__property DropDownFixedWidth  = {default=0};
	__property OnCloseUp ;
	__property Style  = {default=0};
	__property Align  = {default=0};
	__property Anchors  = {default=3};
	__property BiDiMode ;
	__property Color  = {default=-2147483643};
	__property Constraints ;
	__property Ctl3D ;
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property DropDownCount  = {default=8};
	__property Enabled  = {default=1};
	__property Font ;
	__property ImeMode  = {default=3};
	__property ImeName ;
	__property ItemHeight ;
	__property Items ;
	__property MaxLength  = {default=0};
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=0};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ShowHint ;
	__property Sorted  = {default=0};
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property Text ;
	__property Visible  = {default=1};
	__property OnChange ;
	__property OnClick ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnDrawItem ;
	__property OnDropDown ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMeasureItem ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TIECustomComboBox.Create */ inline __fastcall virtual TIEComboBox(Classes::TComponent* AOwner) : TIECustomComboBox(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TIECustomComboBox.Destroy */ inline __fastcall virtual ~TIEComboBox(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TIEComboBox(HWND ParentWindow) : TIECustomComboBox(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
static const Shortint IconWidth = 0x10;
extern PACKAGE void __fastcall Register(void);
extern PACKAGE int __fastcall GetItemHeight(Graphics::TFont* Font);

}	/* namespace Iecombobox */
using namespace Iecombobox;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// IEComboBox
