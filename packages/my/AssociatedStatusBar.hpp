// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'AssociatedStatusBar.pas' rev: 6.00

#ifndef AssociatedStatusBarHPP
#define AssociatedStatusBarHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
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

namespace Associatedstatusbar
{
//-- type declarations -------------------------------------------------------
struct TStatusFileInfo
{
	int FilesCount;
	int SelectedCount;
	__int64 FilesSize;
	__int64 SelectedSize;
} ;

class DELPHICLASS TCustomAssociatedStatusBar;
typedef void __fastcall (__closure *TFormatFileInfoEvent)(TCustomAssociatedStatusBar* Sender, const TStatusFileInfo &FileInfo, AnsiString &Text);

class PASCALIMPLEMENTATION TCustomAssociatedStatusBar : public Comctrls::TStatusBar 
{
	typedef Comctrls::TStatusBar inherited;
	
private:
	TStatusFileInfo FFileInfo;
	AnsiString FFileInfoFormat;
	int FFileInfoPanel;
	Controls::TWinControl* FFocusControl;
	TFormatFileInfoEvent FOnFormatFileInfo;
	void __fastcall SetFileInfo(const TStatusFileInfo &Value);
	void __fastcall SetFileInfoFormat(AnsiString Value);
	void __fastcall SetFileInfoPanel(int Value);
	void __fastcall SetFocusControl(Controls::TWinControl* Value);
	bool __fastcall StoreFileInfoFormat(void);
	
protected:
	DYNAMIC void __fastcall Click(void);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	void __fastcall UpdateData(void);
	
public:
	__fastcall virtual TCustomAssociatedStatusBar(Classes::TComponent* AOwner);
	__property TStatusFileInfo FileInfo = {read=FFileInfo, write=SetFileInfo};
	__property AnsiString FileInfoFormat = {read=FFileInfoFormat, write=SetFileInfoFormat, stored=StoreFileInfoFormat};
	__property int FileInfoPanel = {read=FFileInfoPanel, write=SetFileInfoPanel, default=0};
	__property Controls::TWinControl* FocusControl = {read=FFocusControl, write=SetFocusControl};
	__property TFormatFileInfoEvent OnFormatFileInfo = {read=FOnFormatFileInfo, write=FOnFormatFileInfo};
	__property SimplePanel  = {default=0};
public:
	#pragma option push -w-inl
	/* TCustomStatusBar.Destroy */ inline __fastcall virtual ~TCustomAssociatedStatusBar(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCustomAssociatedStatusBar(HWND ParentWindow) : Comctrls::TStatusBar(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TAssociatedStatusBar;
class PASCALIMPLEMENTATION TAssociatedStatusBar : public TCustomAssociatedStatusBar 
{
	typedef TCustomAssociatedStatusBar inherited;
	
__published:
	__property FocusControl ;
	__property FileInfoFormat ;
	__property FileInfoPanel  = {default=0};
	__property OnFormatFileInfo ;
public:
	#pragma option push -w-inl
	/* TCustomAssociatedStatusBar.Create */ inline __fastcall virtual TAssociatedStatusBar(Classes::TComponent* AOwner) : TCustomAssociatedStatusBar(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomStatusBar.Destroy */ inline __fastcall virtual ~TAssociatedStatusBar(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TAssociatedStatusBar(HWND ParentWindow) : TCustomAssociatedStatusBar(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE __int64 FormatBytesAbove;
extern PACKAGE System::ResourceString _SDefaultFileInfoFormat;
#define Associatedstatusbar_SDefaultFileInfoFormat System::LoadResourceString(&Associatedstatusbar::_SDefaultFileInfoFormat)
extern PACKAGE AnsiString __fastcall FormatBytes(__int64 Bytes);
extern PACKAGE void __fastcall Register(void);

}	/* namespace Associatedstatusbar */
using namespace Associatedstatusbar;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AssociatedStatusBar
