// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ToolbarPanel.pas' rev: 6.00

#ifndef ToolbarPanelHPP
#define ToolbarPanelHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Menus.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <Buttons.hpp>	// Pascal unit
#include <ActnList.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
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

namespace Toolbarpanel
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TSpeedButtonDisplay { bdImage, bdEllipse, bdCaption, bdShortCut };
#pragma option pop

class DELPHICLASS TToolbarSpeedButton;
class PASCALIMPLEMENTATION TToolbarSpeedButton : public Buttons::TSpeedButton 
{
	typedef Buttons::TSpeedButton inherited;
	
private:
	AnsiString FActionCaption;
	TSpeedButtonDisplay FDisplay;
	Classes::TShortCut FShortCut;
	bool FPreserveEnabled;
	bool __fastcall GetSimulateDisabled(void);
	void __fastcall SetActionCaption(AnsiString Value);
	void __fastcall SetDisplay(TSpeedButtonDisplay Value);
	void __fastcall SetShortCut(Classes::TShortCut Value);
	
protected:
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	AnsiString __fastcall AssembleCaption(TSpeedButtonDisplay ADisplay);
	void __fastcall CopyImage(int Index);
	void __fastcall ChangeButton(void);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	virtual bool __fastcall GetEnabled(void);
	virtual void __fastcall Paint(void);
	virtual void __fastcall SetEnabled(bool Value);
	
public:
	__fastcall virtual TToolbarSpeedButton(Classes::TComponent* AOwner);
	int __fastcall CalcWidth(TSpeedButtonDisplay ADisplay);
	__property AnsiString ActionCaption = {read=FActionCaption, write=SetActionCaption};
	__property TSpeedButtonDisplay Display = {read=FDisplay, write=SetDisplay, nodefault};
	__property Margin  = {default=3};
	__property Classes::TShortCut ShortCut = {read=FShortCut, write=SetShortCut, nodefault};
	__property bool SimulateDisabled = {read=GetSimulateDisabled, nodefault};
	__property Spacing  = {default=1};
public:
	#pragma option push -w-inl
	/* TSpeedButton.Destroy */ inline __fastcall virtual ~TToolbarSpeedButton(void) { }
	#pragma option pop
	
};


class DELPHICLASS TCustomToolbarPanel;
class PASCALIMPLEMENTATION TCustomToolbarPanel : public Extctrls::TCustomPanel 
{
	typedef Extctrls::TCustomPanel inherited;
	
private:
	Actnlist::TCustomActionList* FActionList;
	bool FAlreadyCreated;
	AnsiString FCategory;
	TSpeedButtonDisplay FDisplay;
	bool FFlat;
	bool FProgressiveHidding;
	bool FSameWidth;
	bool FStretch;
	Imglist::TCustomImageList* FImages[2];
	int FUpdating;
	bool FWantResize;
	void __fastcall ConstraintsChange(System::TObject* Sender);
	int __fastcall GetButtonCount(void);
	TToolbarSpeedButton* __fastcall GetButtons(int Index);
	void __fastcall SetActionList(Actnlist::TCustomActionList* Value);
	void __fastcall SetCategory(AnsiString Value);
	HIDESBASE void __fastcall SetConstraints(Controls::TSizeConstraints* Value);
	void __fastcall SetDisplay(TSpeedButtonDisplay Value);
	void __fastcall SetFlat(bool Value);
	void __fastcall SetProgressiveHidding(bool Value);
	void __fastcall SetSameWidth(bool Value);
	void __fastcall SetStretch(bool Value);
	bool __fastcall ConstraintsStored(void);
	Controls::TSizeConstraints* __fastcall GetConstraints(void);
	void __fastcall SetImages(int Index, Imglist::TCustomImageList* Value);
	
protected:
	void __fastcall BeginUpdate(void);
	void __fastcall EndUpdate(void);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	virtual void __fastcall Paint(void);
	DYNAMIC void __fastcall Resize(void);
	void __fastcall ResizeButtons(void);
	void __fastcall UpdateConstraints(void);
	
public:
	__fastcall virtual TCustomToolbarPanel(Classes::TComponent* AOwner);
	void __fastcall CreateButtons(void);
	__property Actnlist::TCustomActionList* ActionList = {read=FActionList, write=SetActionList};
	__property Align  = {default=2};
	__property AnsiString Category = {read=FCategory, write=SetCategory};
	__property bool Flat = {read=FFlat, write=SetFlat, nodefault};
	__property Height  = {default=22};
	__property BevelOuter  = {default=0};
	__property int ButtonCount = {read=GetButtonCount, nodefault};
	__property TToolbarSpeedButton* Buttons[int Index] = {read=GetButtons};
	__property Controls::TSizeConstraints* Constraints = {read=GetConstraints, write=SetConstraints, stored=ConstraintsStored};
	__property Imglist::TCustomImageList* DisabledImages = {read=FImages[142], write=SetImages, index=1};
	__property TSpeedButtonDisplay Display = {read=FDisplay, write=SetDisplay, default=3};
	__property Imglist::TCustomImageList* Images = {read=FImages[141], write=SetImages, index=0};
	__property bool ProgressiveHidding = {read=FProgressiveHidding, write=SetProgressiveHidding, default=1};
	__property bool SameWidth = {read=FSameWidth, write=SetSameWidth, default=0};
	__property bool Stretch = {read=FStretch, write=SetStretch, nodefault};
public:
	#pragma option push -w-inl
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TCustomToolbarPanel(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCustomToolbarPanel(HWND ParentWindow) : Extctrls::TCustomPanel(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TToolbarPanel;
class PASCALIMPLEMENTATION TToolbarPanel : public TCustomToolbarPanel 
{
	typedef TCustomToolbarPanel inherited;
	
__published:
	__property Category ;
	__property ActionList ;
	__property Display  = {default=3};
	__property SameWidth  = {default=0};
	__property Stretch ;
	__property ProgressiveHidding  = {default=1};
	__property Images ;
	__property DisabledImages ;
	__property Align  = {default=2};
	__property Anchors  = {default=3};
	__property BevelOuter  = {default=0};
	__property BevelInner  = {default=0};
	__property BevelWidth  = {default=1};
	__property Constraints ;
	__property Enabled  = {default=1};
	__property PopupMenu ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=0};
	__property ShowHint ;
	__property Visible  = {default=1};
public:
	#pragma option push -w-inl
	/* TCustomToolbarPanel.Create */ inline __fastcall virtual TToolbarPanel(Classes::TComponent* AOwner) : TCustomToolbarPanel(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TToolbarPanel(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TToolbarPanel(HWND ParentWindow) : TCustomToolbarPanel(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
static const Shortint MinToolbarPanelWidth = 0x17;
static const Shortint DefaultToolbarPanelHeight = 0x16;
static const Shortint DefaultToolbarSpeedButtonMargin = 0x3;
static const Shortint DefaultToolbarSpeedButtonSpacing = 0x1;
extern PACKAGE void __fastcall Register(void);

}	/* namespace Toolbarpanel */
using namespace Toolbarpanel;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ToolbarPanel
