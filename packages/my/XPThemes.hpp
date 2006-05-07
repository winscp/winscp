// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'XPThemes.pas' rev: 6.00

#ifndef XPThemesHPP
#define XPThemesHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Controls.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Xpthemes
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TXPTheme;
class PASCALIMPLEMENTATION TXPTheme : public System::TObject 
{
	typedef System::TObject inherited;
	
public:
	__fastcall TXPTheme(void);
	__fastcall virtual ~TXPTheme(void);
	bool __fastcall XPComCtl(void);
	void __fastcall ShowFocus(Forms::TCustomForm* Form);
	void __fastcall ShowAccelerators(Forms::TCustomForm* Form);
	
private:
	unsigned FThemeLib;
	HWND FWindowHandle;
	bool FThemesActive;
	HRESULT __stdcall (*FDrawThemeBackground)(unsigned hTheme, HDC hdc, int iPartId, int iStateId, const Types::TRect &pRect, Types::PRect pClipRect);
	unsigned __stdcall (*FOpenThemeData)(HWND hwnd, wchar_t * pszClassList);
	HRESULT __stdcall (*FCloseThemeData)(unsigned hTheme);
	HRESULT __stdcall (*FDrawThemeText)(unsigned hTheme, HDC hdc, int iPartId, int iStateId, wchar_t * pszText, int iCharCount, unsigned dwTextFlags, unsigned dwTextFlags2, const Types::TRect &pRect);
	HRESULT __stdcall (*FGetThemeTextExtent)(unsigned hTheme, HDC hdc, int iPartId, int iStateId, wchar_t * pszText, int iCharCount, unsigned dwTextFlags, Types::PRect pBoundingRect, Types::TRect &pExtentRect);
	BOOL __stdcall (*FIsThemeBackgroundPartiallyTransparent)(unsigned hTheme, int iPartId, int iStateId);
	HRESULT __stdcall (*FDrawThemeParentBackground)(HWND hwnd, HDC hdc, Types::PRect prc);
	unsigned __stdcall (*FGetThemeAppProperties)(void);
	BOOL __stdcall (*FIsAppThemed)(void);
	BOOL __stdcall (*FIsThemeActive)(void);
	void __fastcall WndProc(Messages::TMessage &Msg);
	void __fastcall UpdateThemesActive(void);
};


class DELPHICLASS TXPGroupBox;
class PASCALIMPLEMENTATION TXPGroupBox : public Stdctrls::TGroupBox 
{
	typedef Stdctrls::TGroupBox inherited;
	
private:
	unsigned FButtonThemeData;
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
extern PACKAGE TXPTheme* XPTheme;
extern PACKAGE void __fastcall XPPageControl(Comctrls::TPageControl* PageControl);
extern PACKAGE void __fastcall Register(void);

}	/* namespace Xpthemes */
using namespace Xpthemes;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// XPThemes
