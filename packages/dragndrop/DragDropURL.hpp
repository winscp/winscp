// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DragDropURL.pas' rev: 6.00

#ifndef DragDropURLHPP
#define DragDropURLHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ShlObj.hpp>	// Pascal unit
#include <PIDL.hpp>	// Pascal unit
#include <ActiveX.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <DragDrop.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dragdropurl
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDataObjectURL;
class PASCALIMPLEMENTATION TDataObjectURL : public Dragdrop::TDataObject 
{
	typedef Dragdrop::TDataObject inherited;
	
private:
	Classes::TMemoryStream* URLStream;
	Classes::TMemoryStream* FGDStream;
	
public:
	__fastcall TDataObjectURL(AnsiString ScrapFileName, AnsiString URL, bool Scrap);
	__fastcall virtual ~TDataObjectURL(void);
	virtual HRESULT __fastcall RenderData(const tagFORMATETC &FormatEtc, tagSTGMEDIUM &StgMedium);
};


class DELPHICLASS TDropTargetURL;
class PASCALIMPLEMENTATION TDropTargetURL : public Dragdrop::TDropTarget 
{
	typedef Dragdrop::TDropTarget inherited;
	
protected:
	virtual void __fastcall AcceptDataObject(_di_IDataObject DataObj, bool &Accept);
	
public:
	__fastcall TDropTargetURL(Dragdrop::TDragDrop* AOwner);
	__fastcall virtual ~TDropTargetURL(void);
	virtual void __fastcall RenderDropped(_di_IDataObject DataObj, int grfKeyState, const Types::TPoint &pt, int &dwEffect);
};


class DELPHICLASS TDragDropURL;
class PASCALIMPLEMENTATION TDragDropURL : public Dragdrop::TDragDrop 
{
	typedef Dragdrop::TDragDrop inherited;
	
private:
	AnsiString FURL;
	AnsiString FScrapFileName;
	
protected:
	virtual Dragdrop::TDataObject* __fastcall CreateDataObject(void);
	
public:
	__fastcall virtual TDragDropURL(Classes::TComponent* AOwner);
	__fastcall virtual ~TDragDropURL(void);
	__property AnsiString URL = {read=FURL, write=FURL};
	__property AnsiString ScrapFileName = {read=FScrapFileName, write=FScrapFileName};
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Dragdropurl */
using namespace Dragdropurl;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DragDropURL
