// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TB2Acc.pas' rev: 6.00

#ifndef TB2AccHPP
#define TB2AccHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <TB2Item.hpp>	// Pascal unit
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

namespace Tb2acc
{
//-- type declarations -------------------------------------------------------
__interface ITBAccessible;
typedef System::DelphiInterface<ITBAccessible> _di_ITBAccessible;
__interface INTERFACE_UUID("{618736E0-3C3D-11CF-810C-00AA00389B71}") ITBAccessible  : public IDispatch 
{
	
public:
	virtual HRESULT __stdcall get_accParent(/* out */ _di_IDispatch &ppdispParent) = 0 ;
	virtual HRESULT __stdcall get_accChildCount(/* out */ int &pcountChildren) = 0 ;
	virtual HRESULT __stdcall get_accChild(const OleVariant varChild, /* out */ _di_IDispatch &ppdispChild) = 0 ;
	virtual HRESULT __stdcall get_accName(const OleVariant varChild, /* out */ WideString &pszName) = 0 ;
	virtual HRESULT __stdcall get_accValue(const OleVariant varChild, /* out */ WideString &pszValue) = 0 ;
	virtual HRESULT __stdcall get_accDescription(const OleVariant varChild, /* out */ WideString &pszDescription) = 0 ;
	virtual HRESULT __stdcall get_accRole(const OleVariant varChild, /* out */ OleVariant &pvarRole) = 0 ;
	virtual HRESULT __stdcall get_accState(const OleVariant varChild, /* out */ OleVariant &pvarState) = 0 ;
	virtual HRESULT __stdcall get_accHelp(const OleVariant varChild, /* out */ WideString &pszHelp) = 0 ;
	virtual HRESULT __stdcall get_accHelpTopic(/* out */ WideString &pszHelpFile, const OleVariant varChild, /* out */ int &pidTopic) = 0 ;
	virtual HRESULT __stdcall get_accKeyboardShortcut(const OleVariant varChild, /* out */ WideString &pszKeyboardShortcut) = 0 ;
	virtual HRESULT __stdcall get_accFocus(/* out */ OleVariant &pvarID) = 0 ;
	virtual HRESULT __stdcall get_accSelection(/* out */ OleVariant &pvarChildren) = 0 ;
	virtual HRESULT __stdcall get_accDefaultAction(const OleVariant varChild, /* out */ WideString &pszDefaultAction) = 0 ;
	virtual HRESULT __stdcall accSelect(int flagsSelect, const OleVariant varChild) = 0 ;
	virtual HRESULT __stdcall accLocation(/* out */ int &pxLeft, /* out */ int &pyTop, /* out */ int &pcxWidth, /* out */ int &pcyHeight, const OleVariant varChild) = 0 ;
	virtual HRESULT __stdcall accNavigate(int navDir, const OleVariant varStart, /* out */ OleVariant &pvarEnd) = 0 ;
	virtual HRESULT __stdcall accHitTest(int xLeft, int yTop, /* out */ OleVariant &pvarID) = 0 ;
	virtual HRESULT __stdcall accDoDefaultAction(const OleVariant varChild) = 0 ;
	virtual HRESULT __stdcall put_accName(const OleVariant varChild, const WideString pszName) = 0 ;
	virtual HRESULT __stdcall put_accValue(const OleVariant varChild, const WideString pszValue) = 0 ;
};

class DELPHICLASS TTBCustomAccObject;
class PASCALIMPLEMENTATION TTBCustomAccObject : public Tb2item::TTBBaseAccObject 
{
	typedef Tb2item::TTBBaseAccObject inherited;
	
private:
	TTBCustomAccObject* FPrevious;
	TTBCustomAccObject* FNext;
	
public:
	__fastcall TTBCustomAccObject(void);
	__fastcall virtual ~TTBCustomAccObject(void);
private:
	void *__IDispatch;	/* IDispatch */
	
public:
	operator IDispatch*(void) { return (IDispatch*)&__IDispatch; }
	operator IInterface*(void) { return (IInterface*)&__IDispatch; }
	
};


class DELPHICLASS TTBViewAccObject;
class PASCALIMPLEMENTATION TTBViewAccObject : public TTBCustomAccObject 
{
	typedef TTBCustomAccObject inherited;
	
private:
	Tb2item::TTBView* FView;
	bool __fastcall Check(const OleVariant &varChild, HRESULT &ErrorCode);
	HRESULT __stdcall accDoDefaultAction(const OleVariant varChild);
	HRESULT __stdcall accHitTest(int xLeft, int yTop, /* out */ OleVariant &pvarID);
	HRESULT __stdcall accLocation(/* out */ int &pxLeft, /* out */ int &pyTop, /* out */ int &pcxWidth, /* out */ int &pcyHeight, const OleVariant varChild);
	HRESULT __stdcall accNavigate(int navDir, const OleVariant varStart, /* out */ OleVariant &pvarEnd);
	HRESULT __stdcall accSelect(int flagsSelect, const OleVariant varChild);
	HRESULT __stdcall get_accChild(const OleVariant varChild, /* out */ _di_IDispatch &ppdispChild);
	HRESULT __stdcall get_accChildCount(/* out */ int &pcountChildren);
	HRESULT __stdcall get_accDefaultAction(const OleVariant varChild, /* out */ WideString &pszDefaultAction);
	HRESULT __stdcall get_accDescription(const OleVariant varChild, /* out */ WideString &pszDescription);
	HRESULT __stdcall get_accFocus(/* out */ OleVariant &pvarID);
	HRESULT __stdcall get_accHelp(const OleVariant varChild, /* out */ WideString &pszHelp);
	HRESULT __stdcall get_accHelpTopic(/* out */ WideString &pszHelpFile, const OleVariant varChild, /* out */ int &pidTopic);
	HRESULT __stdcall get_accKeyboardShortcut(const OleVariant varChild, /* out */ WideString &pszKeyboardShortcut);
	HRESULT __stdcall get_accName(const OleVariant varChild, /* out */ WideString &pszName);
	HRESULT __stdcall get_accParent(/* out */ _di_IDispatch &ppdispParent);
	HRESULT __stdcall get_accRole(const OleVariant varChild, /* out */ OleVariant &pvarRole);
	HRESULT __stdcall get_accSelection(/* out */ OleVariant &pvarChildren);
	HRESULT __stdcall get_accState(const OleVariant varChild, /* out */ OleVariant &pvarState);
	HRESULT __stdcall get_accValue(const OleVariant varChild, /* out */ WideString &pszValue);
	HRESULT __stdcall put_accName(const OleVariant varChild, const WideString pszName);
	HRESULT __stdcall put_accValue(const OleVariant varChild, const WideString pszValue);
	
public:
	__fastcall TTBViewAccObject(Tb2item::TTBView* AView);
	__fastcall virtual ~TTBViewAccObject(void);
	virtual void __fastcall ClientIsDestroying(void);
private:
	void *__ITBAccessible;	/* Tb2acc::ITBAccessible */
	
public:
	operator ITBAccessible*(void) { return (ITBAccessible*)&__ITBAccessible; }
	operator IDispatch*(void) { return (IDispatch*)&__ITBAccessible; }
	operator IInterface*(void) { return (IInterface*)&__ITBAccessible; }
	
};


class DELPHICLASS TTBItemViewerAccObject;
class PASCALIMPLEMENTATION TTBItemViewerAccObject : public TTBCustomAccObject 
{
	typedef TTBCustomAccObject inherited;
	
private:
	Tb2item::TTBItemViewer* FViewer;
	bool __fastcall Check(const OleVariant &varChild, HRESULT &ErrorCode);
	bool __fastcall IsActionable(void);
	bool __fastcall IsAvailable(void);
	bool __fastcall IsFocusable(void);
	HRESULT __stdcall accDoDefaultAction(const OleVariant varChild);
	HRESULT __stdcall accHitTest(int xLeft, int yTop, /* out */ OleVariant &pvarID);
	HRESULT __stdcall accLocation(/* out */ int &pxLeft, /* out */ int &pyTop, /* out */ int &pcxWidth, /* out */ int &pcyHeight, const OleVariant varChild);
	HRESULT __stdcall accNavigate(int navDir, const OleVariant varStart, /* out */ OleVariant &pvarEnd);
	HRESULT __stdcall accSelect(int flagsSelect, const OleVariant varChild);
	HRESULT __stdcall get_accChild(const OleVariant varChild, /* out */ _di_IDispatch &ppdispChild);
	HRESULT __stdcall get_accChildCount(/* out */ int &pcountChildren);
	HRESULT __stdcall get_accDefaultAction(const OleVariant varChild, /* out */ WideString &pszDefaultAction);
	HRESULT __stdcall get_accDescription(const OleVariant varChild, /* out */ WideString &pszDescription);
	HRESULT __stdcall get_accFocus(/* out */ OleVariant &pvarID);
	HRESULT __stdcall get_accHelp(const OleVariant varChild, /* out */ WideString &pszHelp);
	HRESULT __stdcall get_accHelpTopic(/* out */ WideString &pszHelpFile, const OleVariant varChild, /* out */ int &pidTopic);
	HRESULT __stdcall get_accKeyboardShortcut(const OleVariant varChild, /* out */ WideString &pszKeyboardShortcut);
	HRESULT __stdcall get_accName(const OleVariant varChild, /* out */ WideString &pszName);
	HRESULT __stdcall get_accParent(/* out */ _di_IDispatch &ppdispParent);
	HRESULT __stdcall get_accRole(const OleVariant varChild, /* out */ OleVariant &pvarRole);
	HRESULT __stdcall get_accSelection(/* out */ OleVariant &pvarChildren);
	HRESULT __stdcall get_accState(const OleVariant varChild, /* out */ OleVariant &pvarState);
	HRESULT __stdcall get_accValue(const OleVariant varChild, /* out */ WideString &pszValue);
	HRESULT __stdcall put_accName(const OleVariant varChild, const WideString pszName);
	HRESULT __stdcall put_accValue(const OleVariant varChild, const WideString pszValue);
	
public:
	__fastcall TTBItemViewerAccObject(Tb2item::TTBItemViewer* AViewer);
	__fastcall virtual ~TTBItemViewerAccObject(void);
	virtual void __fastcall ClientIsDestroying(void);
	void __fastcall HandleAccSelect(const bool AExecute);
private:
	void *__ITBAccessible;	/* Tb2acc::ITBAccessible */
	
public:
	operator ITBAccessible*(void) { return (ITBAccessible*)&__ITBAccessible; }
	operator IDispatch*(void) { return (IDispatch*)&__ITBAccessible; }
	operator IInterface*(void) { return (IInterface*)&__ITBAccessible; }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE int __stdcall (*LresultFromObjectFunc)(const GUID &riid, int wParam, System::_di_IInterface pUnk);
extern PACKAGE HRESULT __stdcall (*AccessibleObjectFromWindowFunc)(HWND hwnd, unsigned dwId, const GUID &riid, /* out */ void * &ppvObject);
extern PACKAGE int ViewAccObjectInstances;
extern PACKAGE int ItemViewerAccObjectInstances;
extern PACKAGE void __fastcall CallNotifyWinEvent(unsigned event, HWND hwnd, unsigned idObject, int idChild);
extern PACKAGE bool __fastcall InitializeOleAcc(void);

}	/* namespace Tb2acc */
using namespace Tb2acc;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TB2Acc
