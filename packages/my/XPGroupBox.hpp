// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'XPGroupBox.pas' rev: 6.00

#ifndef XPGroupBoxHPP
#define XPGroupBoxHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Controls.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Xpgroupbox
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TXPGroupBox;
class PASCALIMPLEMENTATION TXPGroupBox : public Stdctrls::TGroupBox 
{
	typedef Stdctrls::TGroupBox inherited;
	
private:
	unsigned FButtonThemeData;
	bool FThemesActive;
	unsigned __fastcall GetButtonThemeData(void);
	
protected:
	virtual void __fastcall WndProc(Messages::TMessage &Message);
	void __fastcall UpdateAccel(Word CharCode);
	
public:
	__fastcall virtual TXPGroupBox(Classes::TComponent* AOwner);
	__fastcall virtual ~TXPGroupBox(void);
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TXPGroupBox(HWND ParentWindow) : Stdctrls::TGroupBox(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Xpgroupbox */
using namespace Xpgroupbox;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// XPGroupBox
