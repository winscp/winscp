// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UnixPathComboBox.pas' rev: 5.00

#ifndef UnixPathComboBoxHPP
#define UnixPathComboBoxHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Menus.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Unixpathcombobox
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TUnixPathComboBox;
class PASCALIMPLEMENTATION TUnixPathComboBox : public Stdctrls::TCustomComboBox 
{
	typedef Stdctrls::TCustomComboBox inherited;
	
private:
	AnsiString FUnixPath;
	AnsiString FRootName;
	Controls::TImageList* FImageList;
	bool FNotifyChange;
	bool F_NotifyChange;
	void __fastcall SetUnixPath(AnsiString AUnixPath);
	void __fastcall SetRootName(AnsiString ARootName);
	void __fastcall UpdateItems(void);
	HIDESBASE MESSAGE void __fastcall CNDrawItem(Messages::TWMDrawItem &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMCHAR(Messages::TMessage &Message);
	
protected:
	virtual void __fastcall CreateWnd(void);
	DYNAMIC void __fastcall Change(void);
	DYNAMIC void __fastcall Click(void);
	
public:
	__fastcall virtual TUnixPathComboBox(Classes::TComponent* AOwner);
	__fastcall virtual ~TUnixPathComboBox(void);
	__property bool NotifyChange = {read=F_NotifyChange, write=F_NotifyChange, nodefault};
	
__published:
	__property Align ;
	__property Color ;
	__property Ctl3D ;
	__property DragMode ;
	__property DragCursor ;
	__property DropDownCount ;
	__property Anchors ;
	__property Enabled ;
	__property Font ;
	__property ParentColor ;
	__property ParentCtl3D ;
	__property ParentFont ;
	__property ParentShowHint ;
	__property PopupMenu ;
	__property ShowHint ;
	__property TabOrder ;
	__property TabStop ;
	__property Visible ;
	__property OnClick ;
	__property OnChange ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnDropDown ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnStartDrag ;
	__property AnsiString UnixPath = {read=FUnixPath, write=SetUnixPath};
	__property AnsiString RootName = {read=FRootName, write=SetRootName};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TUnixPathComboBox(HWND ParentWindow) : Stdctrls::TCustomComboBox(
		ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Unixpathcombobox */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Unixpathcombobox;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnixPathComboBox
