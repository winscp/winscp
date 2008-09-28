// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'PasTools.pas' rev: 6.00

#ifndef PasToolsHPP
#define PasToolsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Controls.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Types.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Pastools
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TControlScrollBeforeUpdate)(System::TObject* ObjectToValidate);

typedef void __fastcall (__closure *TControlScrollAfterUpdate)(void);

class DELPHICLASS TCustomControlScrollOnDragOver;
class PASCALIMPLEMENTATION TCustomControlScrollOnDragOver : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	TControlScrollBeforeUpdate FOnBeforeUpdate;
	TControlScrollAfterUpdate FOnAfterUpdate;
	Extctrls::TTimer* FDragOverTimer;
	Controls::TControl* FControl;
	_FILETIME FDragOverTime;
	_FILETIME FLastVScrollTime;
	int FVScrollCount;
	void __fastcall DragOverTimer(System::TObject* Sender);
	void __fastcall BeforeUpdate(System::TObject* ObjectToValidate);
	void __fastcall AfterUpdate(void);
	
public:
	__fastcall TCustomControlScrollOnDragOver(Controls::TControl* Control, bool ScheduleDragOver);
	__fastcall virtual ~TCustomControlScrollOnDragOver(void);
	virtual void __fastcall StartDrag(void);
	virtual void __fastcall EndDrag(void);
	virtual void __fastcall DragOver(const Types::TPoint &Point) = 0 ;
	__property TControlScrollBeforeUpdate OnBeforeUpdate = {read=FOnBeforeUpdate, write=FOnBeforeUpdate};
	__property TControlScrollAfterUpdate OnAfterUpdate = {read=FOnAfterUpdate, write=FOnAfterUpdate};
};


class DELPHICLASS TTreeViewScrollOnDragOver;
class PASCALIMPLEMENTATION TTreeViewScrollOnDragOver : public TCustomControlScrollOnDragOver 
{
	typedef TCustomControlScrollOnDragOver inherited;
	
private:
	Comctrls::TTreeNode* FLastDragNode;
	_FILETIME FLastHScrollTime;
	
public:
	virtual void __fastcall StartDrag(void);
	virtual void __fastcall DragOver(const Types::TPoint &Point);
public:
	#pragma option push -w-inl
	/* TCustomControlScrollOnDragOver.Create */ inline __fastcall TTreeViewScrollOnDragOver(Controls::TControl* Control, bool ScheduleDragOver) : TCustomControlScrollOnDragOver(Control, ScheduleDragOver) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomControlScrollOnDragOver.Destroy */ inline __fastcall virtual ~TTreeViewScrollOnDragOver(void) { }
	#pragma option pop
	
};


class DELPHICLASS TListViewScrollOnDragOver;
class PASCALIMPLEMENTATION TListViewScrollOnDragOver : public TCustomControlScrollOnDragOver 
{
	typedef TCustomControlScrollOnDragOver inherited;
	
public:
	virtual void __fastcall DragOver(const Types::TPoint &Point);
public:
	#pragma option push -w-inl
	/* TCustomControlScrollOnDragOver.Create */ inline __fastcall TListViewScrollOnDragOver(Controls::TControl* Control, bool ScheduleDragOver) : TCustomControlScrollOnDragOver(Control, ScheduleDragOver) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomControlScrollOnDragOver.Destroy */ inline __fastcall virtual ~TListViewScrollOnDragOver(void) { }
	#pragma option pop
	
};


class DELPHICLASS TListBoxScrollOnDragOver;
class PASCALIMPLEMENTATION TListBoxScrollOnDragOver : public TCustomControlScrollOnDragOver 
{
	typedef TCustomControlScrollOnDragOver inherited;
	
public:
	virtual void __fastcall DragOver(const Types::TPoint &Point);
public:
	#pragma option push -w-inl
	/* TCustomControlScrollOnDragOver.Create */ inline __fastcall TListBoxScrollOnDragOver(Controls::TControl* Control, bool ScheduleDragOver) : TCustomControlScrollOnDragOver(Control, ScheduleDragOver) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomControlScrollOnDragOver.Destroy */ inline __fastcall virtual ~TListBoxScrollOnDragOver(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE Classes::TComponent* __fastcall Construct(TMetaClass* ComponentClass, Classes::TComponent* Owner);
extern PACKAGE bool __fastcall IsVista(void);

}	/* namespace Pastools */
using namespace Pastools;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// PasTools
