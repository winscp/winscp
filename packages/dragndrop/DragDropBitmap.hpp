// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DragDropBitmap.pas' rev: 6.00

#ifndef DragDropBitmapHPP
#define DragDropBitmapHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <ActiveX.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <DragDrop.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dragdropbitmap
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDataObjectBitmap;
class PASCALIMPLEMENTATION TDataObjectBitmap : public Dragdrop::TDataObject 
{
	typedef Dragdrop::TDataObject inherited;
	
private:
	Classes::TMemoryStream* DIBStream;
	
public:
	__fastcall TDataObjectBitmap(const Graphics::TBitmap* Bitmap);
	__fastcall virtual ~TDataObjectBitmap(void);
	virtual HRESULT __fastcall RenderData(const tagFORMATETC &FormatEtc, tagSTGMEDIUM &StgMedium);
};


class DELPHICLASS TDropTargetBitmap;
class PASCALIMPLEMENTATION TDropTargetBitmap : public Dragdrop::TDropTarget 
{
	typedef Dragdrop::TDropTarget inherited;
	
protected:
	virtual void __fastcall AcceptDataObject(_di_IDataObject DataObj, bool &Accept);
	
public:
	__fastcall TDropTargetBitmap(Dragdrop::TDragDrop* AOwner);
	__fastcall virtual ~TDropTargetBitmap(void);
	virtual void __fastcall RenderDropped(_di_IDataObject DataObj, int grfKeyState, const Types::TPoint &pt, int &dwEffect);
};


class DELPHICLASS TDragDropBitmap;
class PASCALIMPLEMENTATION TDragDropBitmap : public Dragdrop::TDragDrop 
{
	typedef Dragdrop::TDragDrop inherited;
	
private:
	Graphics::TBitmap* FBitmap;
	void __fastcall SetBitmap(Graphics::TBitmap* Bitmap);
	
protected:
	virtual Dragdrop::TDataObject* __fastcall CreateDataObject(void);
	
public:
	__fastcall virtual TDragDropBitmap(Classes::TComponent* AOwner);
	__fastcall virtual ~TDragDropBitmap(void);
	__property Graphics::TBitmap* Bitmap = {read=FBitmap, write=SetBitmap};
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Dragdropbitmap */
using namespace Dragdropbitmap;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DragDropBitmap
