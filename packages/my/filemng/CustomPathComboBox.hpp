// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CustomPathComboBox.pas' rev: 6.00

#ifndef CustomPathComboBoxHPP
#define CustomPathComboBoxHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ComCtrls.hpp>	// Pascal unit
#include <IEComboBox.hpp>	// Pascal unit
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

namespace Custompathcombobox
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TCustomPathComboBox;
class PASCALIMPLEMENTATION TCustomPathComboBox : public Iecombobox::TIECustomComboBox 
{
	typedef Iecombobox::TIECustomComboBox inherited;
	
private:
	Comctrls::TCustomListView* FDirView;
	int FLastItem;
	bool FShowFullPath;
	
protected:
	AnsiString FPath;
	virtual void __fastcall DoCloseUp(bool Canceled);
	DYNAMIC void __fastcall DropDown(void);
	virtual void __fastcall SetPath(AnsiString Value) = 0 ;
	virtual AnsiString __fastcall GetPath();
	DYNAMIC void __fastcall Change(void);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	virtual void __fastcall PathChanged(void);
	void __fastcall SetShowFullPath(bool Value);
	virtual AnsiString __fastcall GetItemTextEx(int Index, bool ForList);
	__property Style  = {default=3};
	__property Items  = {stored=false};
	__property ItemHeight  = {stored=false};
	__property MaxLength  = {stored=false, default=0};
	__property Sorted  = {stored=false, default=0};
	__property Text  = {stored=false};
	
public:
	__fastcall virtual TCustomPathComboBox(Classes::TComponent* AOwner);
	__property Comctrls::TCustomListView* DirView = {read=FDirView, write=FDirView};
	__property AnsiString Path = {read=GetPath, write=SetPath};
	__property bool ShowFullPath = {read=FShowFullPath, write=SetShowFullPath, default=0};
public:
	#pragma option push -w-inl
	/* TIECustomComboBox.Destroy */ inline __fastcall virtual ~TCustomPathComboBox(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCustomPathComboBox(HWND ParentWindow) : Iecombobox::TIECustomComboBox(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Custompathcombobox */
using namespace Custompathcombobox;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CustomPathComboBox
