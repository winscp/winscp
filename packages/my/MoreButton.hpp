// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'MoreButton.pas' rev: 6.00

#ifndef MoreButtonHPP
#define MoreButtonHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Menus.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
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

namespace Morebutton
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TMBChangingEvent)(System::TObject* Sender, bool &AllowChange);

typedef AnsiString MoreButton__2[2];

class DELPHICLASS TMoreButton;
class PASCALIMPLEMENTATION TMoreButton : public Stdctrls::TButtonControl 
{
	typedef Stdctrls::TButtonControl inherited;
	
private:
	bool FDefault;
	bool FActive;
	bool FExpanded;
	AnsiString FCaptions[2];
	int FExpandedHeight;
	Classes::TNotifyEvent FOnChange;
	TMBChangingEvent FOnChanging;
	Controls::TWinControl* FPanel;
	bool FRepositionForm;
	void __fastcall SetDefault(bool Value);
	HIDESBASE MESSAGE void __fastcall CMDialogKey(Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall CMDialogChar(Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall CMFocusChanged(Controls::TCMFocusChanged &Message);
	MESSAGE void __fastcall CNCommand(Messages::TWMCommand &Message);
	AnsiString __fastcall DefaultCaptions(bool Expanded);
	void __fastcall SetCaptions(int Index, AnsiString Value);
	void __fastcall SetExpanded(bool Value);
	void __fastcall SetExpandedHeight(int Value);
	void __fastcall SetPanel(Controls::TWinControl* Value);
	bool __fastcall StoreAnchors(void);
	bool __fastcall StoreCaptions(int Index);
	bool __fastcall StoreExpanded(void);
	bool __fastcall StoreExpandedHeight(void);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Messages::TWMEraseBkgnd &Message);
	
protected:
	virtual bool __fastcall CanExpand(void);
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd(void);
	virtual void __fastcall DoChange(void);
	virtual void __fastcall SetButtonStyle(bool ADefault);
	
public:
	__fastcall virtual TMoreButton(Classes::TComponent* AOwner);
	DYNAMIC void __fastcall Click(void);
	DYNAMIC bool __fastcall UseRightToLeftAlignment(void);
	
__published:
	__property AnsiString CollapsedCaption = {read=FCaptions[133], write=SetCaptions, stored=StoreCaptions, index=0};
	__property Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property bool Expanded = {read=FExpanded, write=SetExpanded, stored=StoreExpanded, nodefault};
	__property AnsiString ExpandedCaption = {read=FCaptions[134], write=SetCaptions, stored=StoreCaptions, index=1};
	__property int ExpandedHeight = {read=FExpandedHeight, write=SetExpandedHeight, stored=StoreExpandedHeight, nodefault};
	__property Controls::TWinControl* Panel = {read=FPanel, write=SetPanel};
	__property bool RepositionForm = {read=FRepositionForm, write=FRepositionForm, nodefault};
	__property Action ;
	__property Anchors  = {stored=StoreAnchors, default=3};
	__property BiDiMode ;
	__property Constraints ;
	__property bool Default = {read=FDefault, write=SetDefault, default=0};
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property Font ;
	__property ParentBiDiMode  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ShowHint ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property Visible  = {default=1};
	__property OnClick ;
	__property OnContextPopup ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property TMBChangingEvent OnChanging = {read=FOnChanging, write=FOnChanging};
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMouseDown ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TMoreButton(HWND ParentWindow) : Stdctrls::TButtonControl(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TMoreButton(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::ResourceString _SDefaultExpandedCaption;
#define Morebutton_SDefaultExpandedCaption System::LoadResourceString(&Morebutton::_SDefaultExpandedCaption)
extern PACKAGE System::ResourceString _SDefaultCollapsedCaption;
#define Morebutton_SDefaultCollapsedCaption System::LoadResourceString(&Morebutton::_SDefaultCollapsedCaption)
static const Shortint DefaultExpandedHeight = 0x32;
static const bool DefaultExpanded = true;
extern PACKAGE void __fastcall Register(void);

}	/* namespace Morebutton */
using namespace Morebutton;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MoreButton
