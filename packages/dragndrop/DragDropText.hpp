// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DragDropText.pas' rev: 6.00

#ifndef DragDropTextHPP
#define DragDropTextHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ActiveX.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <DragDrop.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dragdroptext
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDataObjectText;
class PASCALIMPLEMENTATION TDataObjectText : public Dragdrop::TDataObject 
{
	typedef Dragdrop::TDataObject inherited;
	
private:
	Classes::TMemoryStream* TextStream;
	
public:
	__fastcall TDataObjectText(Classes::TStringList* StringList);
	__fastcall virtual ~TDataObjectText(void);
	virtual HRESULT __fastcall RenderData(const tagFORMATETC &FormatEtc, tagSTGMEDIUM &StgMedium);
};


class DELPHICLASS TDropTargetText;
class PASCALIMPLEMENTATION TDropTargetText : public Dragdrop::TDropTarget 
{
	typedef Dragdrop::TDropTarget inherited;
	
protected:
	virtual void __fastcall AcceptDataObject(_di_IDataObject DataObj, bool &Accept);
	
public:
	__fastcall TDropTargetText(Dragdrop::TDragDrop* AOwner);
	__fastcall virtual ~TDropTargetText(void);
	virtual void __fastcall RenderDropped(_di_IDataObject DataObj, int grfKeyState, const Types::TPoint &pt, int &dwEffect);
};


class DELPHICLASS TDragDropText;
class PASCALIMPLEMENTATION TDragDropText : public Dragdrop::TDragDrop 
{
	typedef Dragdrop::TDragDrop inherited;
	
private:
	Classes::TStringList* FLines;
	
protected:
	virtual Dragdrop::TDataObject* __fastcall CreateDataObject(void);
	
public:
	__fastcall virtual TDragDropText(Classes::TComponent* AOwner);
	__fastcall virtual ~TDragDropText(void);
	__property Classes::TStringList* Lines = {read=FLines, write=FLines};
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Dragdroptext */
using namespace Dragdroptext;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DragDropText
