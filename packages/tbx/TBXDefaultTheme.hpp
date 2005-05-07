// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TBXDefaultTheme.pas' rev: 6.00

#ifndef TBXDefaultThemeHPP
#define TBXDefaultThemeHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ImgList.hpp>	// Pascal unit
#include <TBXThemes.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tbxdefaulttheme
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTBXDefaultTheme;
class PASCALIMPLEMENTATION TTBXDefaultTheme : public Tbxthemes::TTBXTheme 
{
	typedef Tbxthemes::TTBXTheme inherited;
	
private:
	MESSAGE void __fastcall TBXSysCommand(Messages::TMessage &Message);
	
protected:
	Graphics::TColor ToolbarColor;
	Graphics::TColor ToolbarText;
	Graphics::TColor DockPanelColor;
	Graphics::TColor StatusBarColor;
	virtual void __fastcall SetupColorCache(void);
	
public:
	__fastcall virtual TTBXDefaultTheme(const AnsiString AName);
	__fastcall virtual ~TTBXDefaultTheme(void);
	virtual bool __fastcall GetBooleanMetrics(int Index);
	virtual Types::TPoint __fastcall GetImageOffset(Graphics::TCanvas* Canvas, const Tbxthemes::TTBXItemInfo &ItemInfo, Imglist::TCustomImageList* ImageList);
	virtual int __fastcall GetIntegerMetrics(int Index);
	virtual void __fastcall GetMargins(int MarginID, /* out */ Tbxthemes::TTBXMargins &Margins);
	virtual Graphics::TColor __fastcall GetItemColor(const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual Graphics::TColor __fastcall GetItemTextColor(const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual Graphics::TColor __fastcall GetItemImageBackground(const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual int __fastcall GetPopupShadowType(void);
	virtual void __fastcall GetViewBorder(int ViewType, /* out */ Types::TPoint &Border);
	virtual Graphics::TColor __fastcall GetViewColor(int ViewType);
	virtual void __fastcall GetViewMargins(int ViewType, /* out */ Tbxthemes::TTBXMargins &Margins);
	virtual void __fastcall PaintBackgnd(Graphics::TCanvas* Canvas, const Types::TRect &ADockRect, const Types::TRect &ARect, const Types::TRect &AClipRect, Graphics::TColor AColor, bool Transparent, int AViewType);
	virtual void __fastcall PaintButton(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall PaintCaption(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo, const AnsiString ACaption, unsigned AFormat, bool Rotated);
	virtual void __fastcall PaintCheckMark(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall PaintRadioButton(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall PaintChevron(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall PaintDock(Graphics::TCanvas* Canvas, const Types::TRect &ClientRect, const Types::TRect &DockRect, int DockPosition);
	virtual void __fastcall PaintDockPanelNCArea(Graphics::TCanvas* Canvas, const Types::TRect &R, const Tbxthemes::TTBXDockPanelInfo &DockPanelInfo);
	virtual void __fastcall PaintDropDownArrow(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall PaintEditButton(Graphics::TCanvas* Canvas, const Types::TRect &ARect, Tbxthemes::TTBXItemInfo &ItemInfo, const Tbxthemes::TTBXEditBtnInfo &ButtonInfo);
	virtual void __fastcall PaintEditFrame(Graphics::TCanvas* Canvas, const Types::TRect &ARect, Tbxthemes::TTBXItemInfo &ItemInfo, const Tbxthemes::TTBXEditInfo &EditInfo);
	virtual void __fastcall PaintFloatingBorder(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXWindowInfo &WindowInfo);
	virtual void __fastcall PaintFrame(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall PaintImage(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo, Imglist::TCustomImageList* ImageList, int ImageIndex);
	virtual void __fastcall PaintMDIButton(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo, unsigned ButtonKind);
	virtual void __fastcall PaintMenuItem(Graphics::TCanvas* Canvas, const Types::TRect &ARect, Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall PaintMenuItemFrame(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall PaintPageScrollButton(Graphics::TCanvas* Canvas, const Types::TRect &ARect, int ButtonType, bool Hot);
	virtual void __fastcall PaintPopupNCArea(Graphics::TCanvas* Canvas, const Types::TRect &R, const Tbxthemes::TTBXPopupInfo &PopupInfo);
	virtual void __fastcall PaintSeparator(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo, bool Horizontal, bool LineSeparator);
	virtual void __fastcall PaintToolbarNCArea(Graphics::TCanvas* Canvas, const Types::TRect &R, const Tbxthemes::TTBXToolbarInfo &ToolbarInfo);
	virtual void __fastcall PaintFrameControl(Graphics::TCanvas* Canvas, const Types::TRect &R, int Kind, int State, void * Params);
	virtual void __fastcall PaintStatusBar(Graphics::TCanvas* Canvas, const Types::TRect &R, int Part);
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Tbxdefaulttheme */
using namespace Tbxdefaulttheme;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TBXDefaultTheme
