// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GrayedCheckBox.pas' rev: 6.00

#ifndef GrayedCheckBoxHPP
#define GrayedCheckBoxHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Classes.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Grayedcheckbox
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TGrayedCheckBox;
class PASCALIMPLEMENTATION TGrayedCheckBox : public Stdctrls::TCheckBox 
{
	typedef Stdctrls::TCheckBox inherited;
	
protected:
	virtual void __fastcall Toggle(void);
public:
	#pragma option push -w-inl
	/* TCustomCheckBox.Create */ inline __fastcall virtual TGrayedCheckBox(Classes::TComponent* AOwner) : Stdctrls::TCheckBox(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TGrayedCheckBox(HWND ParentWindow) : Stdctrls::TCheckBox(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TGrayedCheckBox(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Grayedcheckbox */
using namespace Grayedcheckbox;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GrayedCheckBox
