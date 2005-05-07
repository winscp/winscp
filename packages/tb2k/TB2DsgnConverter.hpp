// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TB2DsgnConverter.pas' rev: 6.00

#ifndef TB2DsgnConverterHPP
#define TB2DsgnConverterHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <TB2Item.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tb2dsgnconverter
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTBConverterForm;
class PASCALIMPLEMENTATION TTBConverterForm : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Stdctrls::TListBox* MessageList;
	Stdctrls::TButton* CloseButton;
	Stdctrls::TButton* CopyButton;
	void __fastcall CloseButtonClick(System::TObject* Sender);
	void __fastcall CopyButtonClick(System::TObject* Sender);
	void __fastcall FormClose(System::TObject* Sender, Forms::TCloseAction &Action);
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TTBConverterForm(Classes::TComponent* AOwner) : Forms::TForm(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTBConverterForm(Classes::TComponent* AOwner, int Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTBConverterForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTBConverterForm(HWND ParentWindow) : Forms::TForm(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall DoConvert(const Tb2item::TTBCustomItem* ParentItem, const Classes::TComponent* Owner);

}	/* namespace Tb2dsgnconverter */
using namespace Tb2dsgnconverter;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TB2DsgnConverter
