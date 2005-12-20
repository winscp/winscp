// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TBXReg.pas' rev: 6.00

#ifndef TBXRegHPP
#define TBXRegHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <TBXStatusBars.hpp>	// Pascal unit
#include <TBXToolPals.hpp>	// Pascal unit
#include <TBXLists.hpp>	// Pascal unit
#include <TBXExtItems.hpp>	// Pascal unit
#include <TB2DsgnItemEditor.hpp>	// Pascal unit
#include <TBX.hpp>	// Pascal unit
#include <TB2Item.hpp>	// Pascal unit
#include <TB2Toolbar.hpp>	// Pascal unit
#include <TB2Reg.hpp>	// Pascal unit
#include <VCLEditors.hpp>	// Pascal unit
#include <DesignEditors.hpp>	// Pascal unit
#include <DesignIntf.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tbxreg
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TThemeProperty;
class PASCALIMPLEMENTATION TThemeProperty : public Designeditors::TStringProperty 
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall GetValues(Classes::TGetStrProc Proc);
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall virtual TThemeProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TThemeProperty(void) { }
	#pragma option pop
	
};


class DELPHICLASS TMLStringProperty;
class PASCALIMPLEMENTATION TMLStringProperty : public Vcleditors::TCaptionProperty 
{
	typedef Vcleditors::TCaptionProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall Edit(void);
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall virtual TMLStringProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Vcleditors::TCaptionProperty(ADesigner, APropCount) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TMLStringProperty(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXLinkImageIndexPropertyEditor;
class PASCALIMPLEMENTATION TTBXLinkImageIndexPropertyEditor : public Tb2reg::TTBImageIndexPropertyEditor 
{
	typedef Tb2reg::TTBImageIndexPropertyEditor inherited;
	
public:
	virtual Imglist::TCustomImageList* __fastcall GetImageListAt(int Index);
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall virtual TTBXLinkImageIndexPropertyEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : Tb2reg::TTBImageIndexPropertyEditor(ADesigner, APropCount) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TTBXLinkImageIndexPropertyEditor(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXColorProperty;
class PASCALIMPLEMENTATION TTBXColorProperty : public Vcleditors::TColorProperty 
{
	typedef Vcleditors::TColorProperty inherited;
	
public:
	virtual AnsiString __fastcall GetValue();
	virtual void __fastcall GetValues(Classes::TGetStrProc Proc);
	virtual void __fastcall SetValue(const AnsiString Value);
	HIDESBASE void __fastcall ListDrawValue(const AnsiString Value, Graphics::TCanvas* ACanvas, const Types::TRect &ARect, bool ASelected);
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall virtual TTBXColorProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Vcleditors::TColorProperty(ADesigner, APropCount) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TTBXColorProperty(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXStatusBarEditor;
class PASCALIMPLEMENTATION TTBXStatusBarEditor : public Designeditors::TDefaultEditor 
{
	typedef Designeditors::TDefaultEditor inherited;
	
protected:
	void __fastcall GetPanelsProp(const Designintf::_di_IProperty Prop);
	
public:
	virtual void __fastcall Edit(void);
	virtual void __fastcall ExecuteVerb(int Index);
	virtual AnsiString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount(void);
public:
	#pragma option push -w-inl
	/* TComponentEditor.Create */ inline __fastcall virtual TTBXStatusBarEditor(Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TDefaultEditor(AComponent, ADesigner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TTBXStatusBarEditor(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXItemsEditor;
class PASCALIMPLEMENTATION TTBXItemsEditor : public Tb2dsgnitemeditor::TTBItemsEditor 
{
	typedef Tb2dsgnitemeditor::TTBItemsEditor inherited;
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
public:
	#pragma option push -w-inl
	/* TComponentEditor.Create */ inline __fastcall virtual TTBXItemsEditor(Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Tb2dsgnitemeditor::TTBItemsEditor(AComponent, ADesigner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TTBXItemsEditor(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Tbxreg */
using namespace Tbxreg;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TBXReg
