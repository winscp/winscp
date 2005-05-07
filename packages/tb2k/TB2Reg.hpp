// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TB2Reg.pas' rev: 6.00

#ifndef TB2RegHPP
#define TB2RegHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <TB2DsgnItemEditor.hpp>	// Pascal unit
#include <TB2ExtItems.hpp>	// Pascal unit
#include <TB2Item.hpp>	// Pascal unit
#include <TB2Dock.hpp>	// Pascal unit
#include <TB2Toolbar.hpp>	// Pascal unit
#include <VCLEditors.hpp>	// Pascal unit
#include <DesignEditors.hpp>	// Pascal unit
#include <DesignIntf.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <ActnList.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tb2reg
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTBImageIndexPropertyEditor;
class PASCALIMPLEMENTATION TTBImageIndexPropertyEditor : public Designeditors::TIntegerProperty 
{
	typedef Designeditors::TIntegerProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall GetValues(Classes::TGetStrProc Proc);
	virtual Imglist::TCustomImageList* __fastcall GetImageListAt(int Index);
	void __fastcall ListMeasureHeight(const AnsiString Value, Graphics::TCanvas* ACanvas, int &AHeight);
	void __fastcall ListMeasureWidth(const AnsiString Value, Graphics::TCanvas* ACanvas, int &AWidth);
	void __fastcall ListDrawValue(const AnsiString Value, Graphics::TCanvas* ACanvas, const Types::TRect &ARect, bool ASelected);
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall virtual TTBImageIndexPropertyEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TIntegerProperty(ADesigner, APropCount) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TTBImageIndexPropertyEditor(void) { }
	#pragma option pop
	
private:
	void *__ICustomPropertyListDrawing;	/* Vcleditors::ICustomPropertyListDrawing */
	
public:
	operator ICustomPropertyListDrawing*(void) { return (ICustomPropertyListDrawing*)&__ICustomPropertyListDrawing; }
	
};


class DELPHICLASS TTBItemImageIndexPropertyEditor;
class PASCALIMPLEMENTATION TTBItemImageIndexPropertyEditor : public TTBImageIndexPropertyEditor 
{
	typedef TTBImageIndexPropertyEditor inherited;
	
public:
	virtual Imglist::TCustomImageList* __fastcall GetImageListAt(int Index);
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall virtual TTBItemImageIndexPropertyEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : TTBImageIndexPropertyEditor(ADesigner, APropCount) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TTBItemImageIndexPropertyEditor(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Tb2reg */
using namespace Tb2reg;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TB2Reg
