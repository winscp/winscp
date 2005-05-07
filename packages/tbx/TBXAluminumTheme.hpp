// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TBXAluminumTheme.pas' rev: 6.00

#ifndef TBXAluminumThemeHPP
#define TBXAluminumThemeHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ImgList.hpp>	// Pascal unit
#include <TBXDefaultTheme.hpp>	// Pascal unit
#include <TBXThemes.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tbxaluminumtheme
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TItemPart { ipBody, ipText, ipFrame };
#pragma option pop

#pragma option push -b-
enum TBtnItemState { bisNormal, bisDisabled, bisSelected, bisPressed, bisHot, bisDisabledHot, bisSelectedHot, bisPopupParent };
#pragma option pop

#pragma option push -b-
enum TMenuItemState { misNormal, misDisabled, misHot, misDisabledHot };
#pragma option pop

#pragma option push -b-
enum TWinFramePart { wfpBorder, wfpCaption, wfpCaptionText };
#pragma option pop

#pragma option push -b-
enum TWinFrameState { wfsActive, wfsInactive };
#pragma option pop

class DELPHICLASS TTBXAluminumTheme;
class PASCALIMPLEMENTATION TTBXAluminumTheme : public Tbxdefaulttheme::TTBXDefaultTheme 
{
	typedef Tbxdefaulttheme::TTBXDefaultTheme inherited;
	
protected:
	Graphics::TColor DockColor;
	Graphics::TColor MenuBarColor;
	Graphics::TColor ToolbarColor;
	Graphics::TColor StatusbarColor;
	Graphics::TColor PopupColor;
	Graphics::TColor DockPanelColor;
	Graphics::TColor PopupFrameColor;
	Graphics::TColor WinFrameColors[2][3];
	Graphics::TColor PnlFrameColors[2][3];
	Graphics::TColor MenuItemColors[4][3];
	Graphics::TColor BtnItemColors[8][3];
	Graphics::TColor SeparatorColor;
	int DefaultRoughness;
	virtual void __fastcall SetupColorCache(void);
	Graphics::TColor __fastcall GetPartColor(const Tbxthemes::TTBXItemInfo &ItemInfo, TItemPart ItemPart);
	Graphics::TColor __fastcall GetBtnColor(const Tbxthemes::TTBXItemInfo &ItemInfo, TItemPart ItemPart);
	
public:
	__fastcall virtual TTBXAluminumTheme(const AnsiString AName);
	__fastcall virtual ~TTBXAluminumTheme(void);
	virtual bool __fastcall GetBooleanMetrics(int Index);
	virtual int __fastcall GetIntegerMetrics(int Index);
	virtual void __fastcall GetMargins(int MarginID, /* out */ Tbxthemes::TTBXMargins &Margins);
	virtual Types::TPoint __fastcall GetImageOffset(Graphics::TCanvas* Canvas, const Tbxthemes::TTBXItemInfo &ItemInfo, Imglist::TCustomImageList* ImageList);
	virtual Graphics::TColor __fastcall GetItemColor(const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual Graphics::TColor __fastcall GetItemTextColor(const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual Graphics::TColor __fastcall GetItemImageBackground(const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual int __fastcall GetPopupShadowType(void);
	virtual void __fastcall GetViewBorder(int ViewType, /* out */ Types::TPoint &Border);
	virtual Graphics::TColor __fastcall GetViewColor(int AViewType);
	virtual void __fastcall PaintBackgnd(Graphics::TCanvas* Canvas, const Types::TRect &ADockRect, const Types::TRect &ARect, const Types::TRect &AClipRect, Graphics::TColor AColor, bool Transparent, int AViewType);
	virtual void __fastcall PaintButton(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall PaintCaption(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo, const AnsiString ACaption, unsigned AFormat, bool Rotated);
	virtual void __fastcall PaintCheckMark(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall PaintChevron(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall PaintDock(Graphics::TCanvas* Canvas, const Types::TRect &ClientRect, const Types::TRect &DockRect, int DockPosition);
	virtual void __fastcall PaintDockPanelNCArea(Graphics::TCanvas* Canvas, const Types::TRect &R, const Tbxthemes::TTBXDockPanelInfo &DockPanelInfo);
	virtual void __fastcall PaintDropDownArrow(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall PaintEditButton(Graphics::TCanvas* Canvas, const Types::TRect &ARect, Tbxthemes::TTBXItemInfo &ItemInfo, const Tbxthemes::TTBXEditBtnInfo &ButtonInfo);
	virtual void __fastcall PaintEditFrame(Graphics::TCanvas* Canvas, const Types::TRect &ARect, Tbxthemes::TTBXItemInfo &ItemInfo, const Tbxthemes::TTBXEditInfo &EditInfo);
	virtual void __fastcall PaintFrame(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall PaintFloatingBorder(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXWindowInfo &WindowInfo);
	virtual void __fastcall PaintImage(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo, Imglist::TCustomImageList* ImageList, int ImageIndex);
	virtual void __fastcall PaintMenuItem(Graphics::TCanvas* Canvas, const Types::TRect &ARect, Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall PaintMenuItemFrame(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall PaintPopupNCArea(Graphics::TCanvas* Canvas, const Types::TRect &R, const Tbxthemes::TTBXPopupInfo &PopupInfo);
	virtual void __fastcall PaintPageScrollButton(Graphics::TCanvas* Canvas, const Types::TRect &ARect, int ButtonType, bool Hot);
	virtual void __fastcall PaintMDIButton(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo, unsigned ButtonKind);
	virtual void __fastcall PaintSeparator(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo, bool Horizontal, bool LineSeparator);
	virtual void __fastcall PaintToolbarNCArea(Graphics::TCanvas* Canvas, const Types::TRect &R, const Tbxthemes::TTBXToolbarInfo &ToolbarInfo);
	virtual void __fastcall PaintFrameControl(Graphics::TCanvas* Canvas, const Types::TRect &R, int Kind, int State, void * Params);
	virtual void __fastcall PaintStatusBar(Graphics::TCanvas* Canvas, const Types::TRect &R, int Part);
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Tbxaluminumtheme */
using namespace Tbxaluminumtheme;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TBXAluminumTheme
