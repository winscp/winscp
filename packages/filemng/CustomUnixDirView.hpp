// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CustomUnixDirView.pas' rev: 6.00

#ifndef CustomUnixDirViewHPP
#define CustomUnixDirViewHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <UnixDirViewColProperties.hpp>	// Pascal unit
#include <ListViewColProperties.hpp>	// Pascal unit
#include <CustomDirView.hpp>	// Pascal unit
#include <IEListView.hpp>	// Pascal unit
#include <NortonLikeListView.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
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

namespace Customunixdirview
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TCustomUnixDirView;
class PASCALIMPLEMENTATION TCustomUnixDirView : public Customdirview::TCustomDirView 
{
	typedef Customdirview::TCustomDirView inherited;
	
private:
	void __fastcall SetUnixColProperties(Unixdirviewcolproperties::TUnixDirViewColProperties* Value);
	Unixdirviewcolproperties::TUnixDirViewColProperties* __fastcall GetUnixColProperties(void);
	
protected:
	virtual Listviewcolproperties::TCustomListViewColProperties* __fastcall NewColProperties(void);
	
public:
	__property Items ;
	
__published:
	__property Unixdirviewcolproperties::TUnixDirViewColProperties* UnixColProperties = {read=GetUnixColProperties, write=SetUnixColProperties};
public:
	#pragma option push -w-inl
	/* TCustomDirView.Create */ inline __fastcall virtual TCustomUnixDirView(Classes::TComponent* AOwner) : Customdirview::TCustomDirView(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomDirView.Destroy */ inline __fastcall virtual ~TCustomUnixDirView(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCustomUnixDirView(HWND ParentWindow) : Customdirview::TCustomDirView(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Customunixdirview */
using namespace Customunixdirview;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CustomUnixDirView
