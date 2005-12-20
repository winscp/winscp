// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TBXThemes.pas' rev: 6.00

#ifndef TBXThemesHPP
#define TBXThemesHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ImgList.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tbxthemes
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TTBXItemLayout { tbxlAuto, tbxlGlyphLeft, tbxlGlyphTop };
#pragma option pop

#pragma pack(push, 4)
struct TTBXMargins
{
	int LeftWidth;
	int RightWidth;
	int TopHeight;
	int BottomHeight;
} ;
#pragma pack(pop)

#pragma option push -b-
enum TTBXHoverKind { hkNone, hkKeyboardHover, hkMouseHover };
#pragma option pop

#pragma option push -b-
enum TTBXComboPart { cpNone, cpCombo, cpSplitLeft, cpSplitRight };
#pragma option pop

#pragma pack(push, 4)
struct TTBXItemInfo
{
	int ViewType;
	int ItemOptions;
	bool Enabled;
	bool Pushed;
	TTBXHoverKind HoverKind;
	bool Selected;
	bool ImageShown;
	int ImageWidth;
	int ImageHeight;
	bool IsVertical;
	TTBXComboPart ComboPart;
	bool IsPopupParent;
	int PopupMargin;
	int AppFlags;
	int AppData;
} ;
#pragma pack(pop)

#pragma pack(push, 4)
struct TTBXWindowInfo
{
	HWND ParentHandle;
	HWND WindowHandle;
	int ViewType;
	int ClientWidth;
	int ClientHeight;
	bool ShowCaption;
	Types::TPoint FloatingBorderSize;
	int CloseButtonState;
	int RedrawPart;
	char *Caption;
	Graphics::TColor EffectiveColor;
	bool Active;
	int AppFlags;
	int AppData;
} ;
#pragma pack(pop)

#pragma pack(push, 4)
struct TTBXPopupInfo
{
	HWND WindowHandle;
	int ViewType;
	Types::TRect ParentRect;
	Types::TPoint BorderSize;
	int AppFlags;
	int AppData;
} ;
#pragma pack(pop)

#pragma pack(push, 4)
struct TTBXToolbarInfo
{
	HWND WindowHandle;
	int ViewType;
	bool IsVertical;
	bool AllowDrag;
	Forms::TFormBorderStyle BorderStyle;
	Types::TPoint BorderSize;
	int ClientWidth;
	int ClientHeight;
	int DragHandleStyle;
	int CloseButtonState;
	char *Caption;
	Graphics::TColor EffectiveColor;
	int AppFlags;
	int AppData;
} ;
#pragma pack(pop)

#pragma pack(push, 4)
struct TTBXDockPanelInfo
{
	HWND WindowHandle;
	int ViewType;
	bool IsVertical;
	bool AllowDrag;
	Forms::TFormBorderStyle BorderStyle;
	Types::TPoint BorderSize;
	int ClientWidth;
	int ClientHeight;
	bool ShowCaption;
	int CloseButtonState;
	char *Caption;
	Graphics::TColor EffectiveColor;
	int AppFlags;
	int AppData;
} ;
#pragma pack(pop)

#pragma pack(push, 4)
struct TTBXEditBtnInfo
{
	int ButtonType;
	int ButtonState;
} ;
#pragma pack(pop)

#pragma pack(push, 4)
struct TTBXEditInfo
{
	int LeftBtnWidth;
	int RightBtnWidth;
	TTBXEditBtnInfo LeftBtnInfo;
	TTBXEditBtnInfo RightBtnInfo;
} ;
#pragma pack(pop)

class DELPHICLASS TTBXTheme;
class PASCALIMPLEMENTATION TTBXTheme : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	AnsiString FName;
	int FTag;
	
public:
	__fastcall virtual TTBXTheme(const AnsiString AName);
	virtual Types::TPoint __fastcall GetImageOffset(Graphics::TCanvas* Canvas, const TTBXItemInfo &ItemInfo, Imglist::TCustomImageList* ImageList) = 0 ;
	virtual Graphics::TColor __fastcall GetItemColor(const TTBXItemInfo &ItemInfo) = 0 ;
	virtual Graphics::TColor __fastcall GetItemTextColor(const TTBXItemInfo &ItemInfo) = 0 ;
	virtual Graphics::TColor __fastcall GetItemImageBackground(const TTBXItemInfo &ItemInfo) = 0 ;
	virtual void __fastcall GetMargins(int MarginID, /* out */ TTBXMargins &Margins) = 0 ;
	virtual int __fastcall GetPopupShadowType(void) = 0 ;
	virtual void __fastcall GetViewBorder(int ViewType, /* out */ Types::TPoint &Border) = 0 ;
	virtual Graphics::TColor __fastcall GetViewColor(int ViewType) = 0 ;
	virtual void __fastcall GetViewMargins(int ViewType, /* out */ TTBXMargins &Margins) = 0 ;
	virtual void __fastcall PaintBackgnd(Graphics::TCanvas* Canvas, const Types::TRect &ADockRect, const Types::TRect &ARect, const Types::TRect &AClipRect, Graphics::TColor AColor, bool Transparent, int AViewType) = 0 ;
	virtual void __fastcall PaintButton(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const TTBXItemInfo &ItemInfo) = 0 ;
	virtual void __fastcall PaintCaption(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const TTBXItemInfo &ItemInfo, const AnsiString ACaption, unsigned AFormat, bool Rotated) = 0 ;
	virtual void __fastcall PaintCheckMark(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const TTBXItemInfo &ItemInfo) = 0 ;
	virtual void __fastcall PaintChevron(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const TTBXItemInfo &ItemInfo) = 0 ;
	virtual void __fastcall PaintEditFrame(Graphics::TCanvas* Canvas, const Types::TRect &ARect, TTBXItemInfo &ItemInfo, const TTBXEditInfo &EditInfo) = 0 ;
	virtual void __fastcall PaintEditButton(Graphics::TCanvas* Canvas, const Types::TRect &ARect, TTBXItemInfo &ItemInfo, const TTBXEditBtnInfo &ButtonInfo) = 0 ;
	virtual void __fastcall PaintDock(Graphics::TCanvas* Canvas, const Types::TRect &ClientRect, const Types::TRect &DockRect, int DockPosition) = 0 ;
	virtual void __fastcall PaintDockPanelNCArea(Graphics::TCanvas* Canvas, const Types::TRect &R, const TTBXDockPanelInfo &DockPanelInfo) = 0 ;
	virtual void __fastcall PaintDropDownArrow(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const TTBXItemInfo &ItemInfo) = 0 ;
	virtual void __fastcall PaintFloatingBorder(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const TTBXWindowInfo &WindowInfo) = 0 ;
	virtual void __fastcall PaintFrame(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const TTBXItemInfo &ItemInfo) = 0 ;
	virtual void __fastcall PaintImage(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const TTBXItemInfo &ItemInfo, Imglist::TCustomImageList* ImageList, int ImageIndex) = 0 ;
	virtual void __fastcall PaintMDIButton(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const TTBXItemInfo &ItemInfo, unsigned ButtonKind) = 0 ;
	virtual void __fastcall PaintMenuItem(Graphics::TCanvas* Canvas, const Types::TRect &ARect, TTBXItemInfo &ItemInfo) = 0 ;
	virtual void __fastcall PaintMenuItemFrame(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const TTBXItemInfo &ItemInfo) = 0 ;
	virtual void __fastcall PaintPageScrollButton(Graphics::TCanvas* Canvas, const Types::TRect &ARect, int ButtonType, bool Hot) = 0 ;
	virtual void __fastcall PaintPopupNCArea(Graphics::TCanvas* Canvas, const Types::TRect &R, const TTBXPopupInfo &PopupInfo) = 0 ;
	virtual void __fastcall PaintSeparator(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const TTBXItemInfo &ItemInfo, bool Horizontal, bool LineSeparator) = 0 ;
	virtual void __fastcall PaintToolbarNCArea(Graphics::TCanvas* Canvas, const Types::TRect &R, const TTBXToolbarInfo &WindowInfo) = 0 ;
	virtual void __fastcall PaintFrameControl(Graphics::TCanvas* Canvas, const Types::TRect &R, int Kind, int State, void * Params) = 0 ;
	virtual void __fastcall PaintStatusBar(Graphics::TCanvas* Canvas, const Types::TRect &R, int Part) = 0 ;
	virtual int __fastcall GetIntegerMetrics(int Index) = 0 ;
	__property int SplitBtnArrowWidth = {read=GetIntegerMetrics, index=10, nodefault};
	__property int DropdownArrowWidth = {read=GetIntegerMetrics, index=20, nodefault};
	__property int DropdownArrowMargin = {read=GetIntegerMetrics, index=21, nodefault};
	__property int MenuImageTextSpace = {read=GetIntegerMetrics, index=32, nodefault};
	__property int MenuLeftCaptionMargin = {read=GetIntegerMetrics, index=33, nodefault};
	__property int MenuRightCaptionMargin = {read=GetIntegerMetrics, index=34, nodefault};
	__property int MenuSeparatorSize = {read=GetIntegerMetrics, index=35, nodefault};
	__property int MenuMDIDW = {read=GetIntegerMetrics, index=36, nodefault};
	__property int MenuMDIDH = {read=GetIntegerMetrics, index=37, nodefault};
	__property int TlbrSeparatorSize = {read=GetIntegerMetrics, index=50, nodefault};
	__property int EditFrameWidth = {read=GetIntegerMetrics, index=60, nodefault};
	__property int EditTextMarginHorz = {read=GetIntegerMetrics, index=61, nodefault};
	__property int EditTextMarginVert = {read=GetIntegerMetrics, index=62, nodefault};
	__property int EditBtnWidth = {read=GetIntegerMetrics, index=65, nodefault};
	__property int EditMenuRightIndent = {read=GetIntegerMetrics, index=66, nodefault};
	virtual bool __fastcall GetBooleanMetrics(int Index) = 0 ;
	__property bool OfficeXPPopupAlignment = {read=GetBooleanMetrics, index=1, nodefault};
	__property bool EditMenuFullSelect = {read=GetBooleanMetrics, index=3, nodefault};
	__property bool EditHeightEven = {read=GetBooleanMetrics, index=4, nodefault};
	__property bool PaintDockBackground = {read=GetBooleanMetrics, index=5, nodefault};
	__property bool SolidToolbarNCArea = {read=GetBooleanMetrics, index=6, nodefault};
	__property bool SolidToolbarClientArea = {read=GetBooleanMetrics, index=7, nodefault};
	__property AnsiString Name = {read=FName};
	__property int Tag = {read=FTag, write=FTag, nodefault};
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TTBXTheme(void) { }
	#pragma option pop
	
};


typedef TMetaClass*TTBXThemeClass;

//-- var, const, procedure ---------------------------------------------------
static const Word TBX_SYSCOMMAND = 0x538;
static const Shortint TSC_BEFOREVIEWCHANGE = 0x1;
static const Shortint TSC_VIEWCHANGE = 0x2;
static const Shortint TSC_AFTERVIEWCHANGE = 0x3;
static const Shortint TSC_APPACTIVATE = 0x4;
static const Shortint TSC_APPDEACTIVATE = 0x5;
static const Shortint TMI_SPLITBTN_ARROWWIDTH = 0xa;
static const Shortint TMI_DROPDOWN_ARROWWIDTH = 0x14;
static const Shortint TMI_DROPDOWN_ARROWMARGIN = 0x15;
static const Shortint TMI_MENU_IMGTEXTSPACE = 0x20;
static const Shortint TMI_MENU_LCAPTIONMARGIN = 0x21;
static const Shortint TMI_MENU_RCAPTIONMARGIN = 0x22;
static const Shortint TMI_MENU_SEPARATORSIZE = 0x23;
static const Shortint TMI_MENU_MDI_DW = 0x24;
static const Shortint TMI_MENU_MDI_DH = 0x25;
static const Shortint TMI_TLBR_SEPARATORSIZE = 0x32;
static const Shortint TMI_EDIT_FRAMEWIDTH = 0x3c;
static const Shortint TMI_EDIT_TEXTMARGINHORZ = 0x3d;
static const Shortint TMI_EDIT_TEXTMARGINVERT = 0x3e;
static const Shortint TMI_EDIT_BTNWIDTH = 0x41;
static const Shortint TMI_EDIT_MENURIGHTINDENT = 0x42;
static const Shortint TMB_OFFICEXPPOPUPALIGNMENT = 0x1;
static const Shortint TMB_EDITMENUFULLSELECT = 0x3;
static const Shortint TMB_EDITHEIGHTEVEN = 0x4;
static const Shortint TMB_PAINTDOCKBACKGROUND = 0x5;
static const Shortint TMB_SOLIDTOOLBARNCAREA = 0x6;
static const Shortint TMB_SOLIDTOOLBARCLIENTAREA = 0x7;
static const Shortint TMB_SOLIDTOOLBARS = 0x6;
static const Shortint MID_TOOLBARITEM = 0x1;
static const Shortint MID_MENUITEM = 0x2;
static const Shortint MID_STATUSPANE = 0x3;
static const Shortint VT_UNKNOWN = 0x0;
static const Word VT_TOOLBAR = 0x1000;
static const Word VT_POPUP = 0x2000;
static const Word VT_DOCKPANEL = 0x4000;
static const Word VT_DOCKWINDOW = 0x8000;
static const int VT_STATUSBAR = 0x10000;
static const Word TVT_FLOATING = 0x800;
static const Word TVT_RESIZABLE = 0x400;
static const Word TVT_EMBEDDED = 0x200;
static const Word TVT_NORMALTOOLBAR = 0x1001;
static const Word TVT_MENUBAR = 0x1002;
static const Word TVT_TOOLWINDOW = 0x1004;
static const Word PVT_POPUPMENU = 0x2001;
static const Word PVT_LISTBOX = 0x2002;
static const Word PVT_TOOLBOX = 0x2004;
static const Word PVT_CHEVRONMENU = 0x2008;
static const Word DPVT_FLOATING = 0x800;
static const Word DPVT_RESIZABLE = 0x400;
static const Word DPVT_NORMAL = 0x4001;
static const Word DWVT_FLOATING = 0x800;
static const Word DWVT_RESIZABLE = 0x400;
static const Word DWVT_NORMAL = 0x8001;
static const Shortint IT_TOOLBARBUTTON = 0x1;
static const Shortint IT_MENUITEM = 0x2;
static const Shortint IO_TOOLBARSTYLE = 0x1;
static const Shortint IO_SUBMENUITEM = 0x4;
static const Shortint IO_COMBO = 0x8;
static const Shortint IO_DESIGNING = 0x10;
static const Shortint IO_APPACTIVE = 0x20;
static const Shortint IO_RADIO = 0x40;
static const Shortint DHS_DOUBLE = 0x0;
static const Shortint DHS_NONE = 0x1;
static const Shortint DHS_SINGLE = 0x2;
static const Shortint CDBS_VISIBLE = 0x1;
static const Shortint CDBS_HOT = 0x2;
static const Shortint CDBS_PRESSED = 0x4;
static const Shortint WRP_BORDER = 0x1;
static const Shortint WRP_CAPTION = 0x2;
static const Shortint WRP_CLOSEBTN = 0x4;
static const Shortint PST_NONE = 0x0;
static const Shortint PST_WINDOWSXP = 0x1;
static const Shortint PST_OFFICEXP = 0x2;
static const Shortint PST_WINDOWS2K = 0x3;
static const Shortint EBT_DROPDOWN = 0x1;
static const Shortint EBT_SPIN = 0x2;
static const Shortint EBDS_DISABLED = 0x1;
static const Shortint EBDS_HOT = 0x2;
static const Shortint EBDS_PRESSED = 0x4;
static const Shortint EBSS_DISABLED = 0x1;
static const Shortint EBSS_HOT = 0x2;
static const Shortint EBSS_UP = 0x4;
static const Shortint EBSS_DOWN = 0x8;
static const Shortint PSBT_UP = 0x1;
static const Shortint PSBT_DOWN = 0x2;
static const Shortint PSBT_LEFT = 0x3;
static const Shortint PSBT_RIGHT = 0x4;
static const Shortint PFC_CHECKBOX = 0x1;
static const Shortint PFC_RADIOBUTTON = 0x2;
static const Shortint PFS_CHECKED = 0x1;
static const Shortint PFS_MIXED = 0x2;
static const Shortint PFS_DISABLED = 0x4;
static const Shortint PFS_HOT = 0x8;
static const Shortint PFS_PUSHED = 0x10;
static const Shortint PFS_FOCUSED = 0x20;
static const Shortint ISF_DISABLED = 0x1;
static const Shortint ISF_HOT = 0x2;
static const Shortint ISF_PUSHED = 0x4;
static const Shortint ISF_SELECTED = 0x8;
static const Word ISF_LOCATIONMASK = 0xf00;
static const Shortint ISF_TOOLBARCOLOR = 0x0;
static const Word ISF_MENUCOLOR = 0x100;
static const Word ISF_STATUSCOLOR = 0x200;
static const Shortint SBP_BODY = 0x0;
static const Shortint SBP_PANE = 0x1;
static const Shortint SBP_LASTPANE = 0x2;
static const Shortint SBP_GRIPPER = 0x3;
static const Shortint DP_TOP = 0x1;
static const Shortint DP_BOTTOM = 0x2;
static const Shortint DP_LEFT = 0x3;
static const Shortint DP_RIGHT = 0x4;
static const Shortint TSP_FLATMENUSTYLE = 0x1;
static const Shortint TSP_XPVISUALSTYLE = 0x2;
static const Shortint FMS_AUTOMATIC = 0x0;
static const Shortint FMS_DISABLED = 0x1;
static const Shortint FMS_ENABLED = 0x2;
static const Shortint XPVS_AUTOMATIC = 0x0;
static const Shortint XPVS_DISABLED = 0x2;
extern PACKAGE Graphics::TColor clHotLight;
extern PACKAGE Graphics::TColor clPopup;
extern PACKAGE Graphics::TColor clPopupText;
extern PACKAGE Graphics::TColor clToolbar;
extern PACKAGE Graphics::TColor clToolbarText;
extern PACKAGE bool TBXLoColor;
extern PACKAGE bool TBXHiContrast;
extern PACKAGE bool TBXNoBlending;
extern PACKAGE unsigned SCROLLBAR_THEME;
extern PACKAGE unsigned REBAR_THEME;
extern PACKAGE unsigned BUTTON_THEME;
extern PACKAGE unsigned TOOLBAR_THEME;
extern PACKAGE unsigned WINDOW_THEME;
extern PACKAGE unsigned COMBO_THEME;
extern PACKAGE unsigned EXPLORERBAR_THEME;
extern PACKAGE unsigned STATUSBAR_THEME;
extern PACKAGE unsigned SPIN_THEME;
extern PACKAGE bool USE_FLATMENUS;
extern PACKAGE bool USE_THEMES;
extern PACKAGE void __fastcall SetTBXSysParam(int Param, int Value);
extern PACKAGE int __fastcall GetTBXSysParam(int Param);
extern PACKAGE void __fastcall AddTBXSysChangeNotification(System::TObject* AObject);
extern PACKAGE void __fastcall RemoveTBXSysChangeNotification(System::TObject* AObject);
extern PACKAGE void __fastcall RegisterTBXTheme(const AnsiString AName, TMetaClass* AThemeClass);
extern PACKAGE void __fastcall UnregisterTBXTheme(const AnsiString AName);
extern PACKAGE bool __fastcall IsTBXThemeAvailable(const AnsiString AName);
extern PACKAGE void __fastcall GetAvailableTBXThemes(Classes::TStrings* Strings);
extern PACKAGE TTBXTheme* __fastcall GetTBXTheme(const AnsiString AName);
extern PACKAGE void __fastcall ReleaseTBXTheme(TTBXTheme* &ATheme);
extern PACKAGE Types::TRect __fastcall GetTBXCaptionRect(const TTBXWindowInfo &WindowInfo, bool AdjustForBorder, bool MinusCloseButton);
extern PACKAGE Types::TRect __fastcall GetTBXCloseButtonRect(const TTBXWindowInfo &WindowInfo, bool AdjustForBorder);
extern PACKAGE Types::TRect __fastcall GetTBXDockedCloseButtonRect(const TTBXToolbarInfo &ToolbarInfo);
extern PACKAGE int __fastcall GetTBXDragHandleSize(const TTBXToolbarInfo &ToolbarInfo);

}	/* namespace Tbxthemes */
using namespace Tbxthemes;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TBXThemes
