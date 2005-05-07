// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TBX.pas' rev: 6.00

#ifndef TBXHPP
#define TBXHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Menus.hpp>	// Pascal unit
#include <TBXThemes.hpp>	// Pascal unit
#include <TBXUtils.hpp>	// Pascal unit
#include <TB2Anim.hpp>	// Pascal unit
#include <TB2Toolbar.hpp>	// Pascal unit
#include <TB2Dock.hpp>	// Pascal unit
#include <TB2Item.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tbx
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TTextWrapping { twNone, twEndEllipsis, twPathEllipsis, twWrap };
#pragma option pop

typedef TTextWrapping TTextTruncation;

#pragma option push -b-
enum TTriState { tsDefault, tsTrue, tsFalse };
#pragma option pop

typedef Word TFontSize;

class DELPHICLASS TFontSettings;
class PASCALIMPLEMENTATION TFontSettings : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;
	
private:
	TTriState FBold;
	TTriState FItalic;
	TTriState FUnderline;
	TTriState FStrikeOut;
	TFontSize FSize;
	Graphics::TColor FColor;
	AnsiString FName;
	Classes::TNotifyEvent FOnChange;
	void __fastcall SetBold(TTriState Value);
	void __fastcall SetColor(Graphics::TColor Value);
	void __fastcall SetItalic(TTriState Value);
	void __fastcall SetName(const AnsiString Value);
	void __fastcall SetSize(TFontSize Value);
	void __fastcall SetStrikeOut(TTriState Value);
	void __fastcall SetUnderline(TTriState Value);
	
protected:
	void __fastcall Modified(void);
	__property Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	
public:
	__fastcall TFontSettings(void);
	void __fastcall Apply(Graphics::TFont* Font)/* overload */;
	void __fastcall Apply(tagLOGFONTA &LF, Graphics::TColor &FontColor)/* overload */;
	virtual void __fastcall Assign(Classes::TPersistent* Src);
	HFONT __fastcall CreateTransformedFont(HFONT Src, Graphics::TColor &FontColor);
	
__published:
	__property TTriState Bold = {read=FBold, write=SetBold, default=0};
	__property TTriState Italic = {read=FItalic, write=SetItalic, default=0};
	__property TTriState Underline = {read=FUnderline, write=SetUnderline, default=0};
	__property TTriState StrikeOut = {read=FStrikeOut, write=SetStrikeOut, default=0};
	__property TFontSize Size = {read=FSize, write=SetSize, default=100};
	__property Graphics::TColor Color = {read=FColor, write=SetColor, default=536870911};
	__property AnsiString Name = {read=FName, write=SetName};
public:
	#pragma option push -w-inl
	/* TPersistent.Destroy */ inline __fastcall virtual ~TFontSettings(void) { }
	#pragma option pop
	
};


#pragma pack(push, 4)
struct TTBXPopupPositionInfo
{
	Tb2item::TTBCustomItem* Item;
	Tb2item::TTBView* ParentView;
	Tb2item::TTBItemViewer* ParentViewer;
	bool PositionAsSubmenu;
	Types::TPoint APopupPoint;
	Tb2item::TTBPopupAlignment Alignment;
	Tb2item::TTBPopupWindow* PopupWindow;
	int X;
	int Y;
	Types::TRect ParentItemRect;
	int AppFlags;
	int AppData;
} ;
#pragma pack(pop)

typedef TMetaClass*TTBXThemeClass;

typedef void __fastcall (__closure *TAdjustFontEvent)(Tb2item::TTBCustomItem* Item, Tb2item::TTBItemViewer* Viewer, Graphics::TFont* Font, int StateFlags);

typedef void __fastcall (__closure *TDrawImageEvent)(Tb2item::TTBCustomItem* Item, Tb2item::TTBItemViewer* Viewer, Graphics::TCanvas* Canvas, const Types::TRect &ImageRect, const Types::TPoint &ImageOffset, int StateFlags);

class DELPHICLASS TTBXCustomItem;
class PASCALIMPLEMENTATION TTBXCustomItem : public Tb2item::TTBCustomItem 
{
	typedef Tb2item::TTBCustomItem inherited;
	
private:
	bool FAlwaysSelectFirst;
	TFontSettings* FFontSettings;
	Tbxthemes::TTBXItemLayout FLayout;
	int FMinHeight;
	int FMinWidth;
	bool FToolBoxPopup;
	TAdjustFontEvent FOnAdjustFont;
	TDrawImageEvent FOnDrawImage;
	void __fastcall FontSettingsChanged(System::TObject* Sender);
	bool __fastcall GetStretch(void);
	void __fastcall SetFontSettings(TFontSettings* Value);
	void __fastcall SetLayout(Tbxthemes::TTBXItemLayout Value);
	void __fastcall SetMinHeight(int Value);
	void __fastcall SetMinWidth(int Value);
	void __fastcall SetStretch(bool Value);
	
protected:
	virtual Tb2item::TTBPopupWindow* __fastcall CreatePopup(const Tb2item::TTBView* ParentView, const Tb2item::TTBItemViewer* ParentViewer, const bool PositionAsSubmenu, const bool SelectFirstItem, const bool Customizing, const Types::TPoint &APopupPoint, const Tb2item::TTBPopupAlignment Alignment);
	virtual TMetaClass* __fastcall GetItemViewerClass(Tb2item::TTBView* AView);
	virtual void __fastcall GetPopupPosition(Tb2item::TTBView* ParentView, Tb2item::TTBPopupWindow* PopupWindow, Tb2item::TTBPopupPositionRec &PopupPositionRec);
	virtual TMetaClass* __fastcall GetPopupWindowClass(void);
	__property bool ToolBoxPopup = {read=FToolBoxPopup, write=FToolBoxPopup, default=0};
	__property TAdjustFontEvent OnAdjustFont = {read=FOnAdjustFont, write=FOnAdjustFont};
	__property TDrawImageEvent OnDrawImage = {read=FOnDrawImage, write=FOnDrawImage};
	
public:
	__fastcall virtual TTBXCustomItem(Classes::TComponent* AOwner);
	__fastcall virtual ~TTBXCustomItem(void);
	void __fastcall Invalidate(void);
	__property bool AlwaysSelectFirst = {read=FAlwaysSelectFirst, write=FAlwaysSelectFirst, default=0};
	__property TFontSettings* FontSettings = {read=FFontSettings, write=SetFontSettings};
	__property Tbxthemes::TTBXItemLayout Layout = {read=FLayout, write=SetLayout, default=0};
	__property int MinHeight = {read=FMinHeight, write=SetMinHeight, default=0};
	__property int MinWidth = {read=FMinWidth, write=SetMinWidth, default=0};
	__property bool Stretch = {read=GetStretch, write=SetStretch, default=0};
};


class DELPHICLASS TTBXItem;
class PASCALIMPLEMENTATION TTBXItem : public TTBXCustomItem 
{
	typedef TTBXCustomItem inherited;
	
__published:
	__property Action ;
	__property AutoCheck  = {default=0};
	__property Caption ;
	__property Checked  = {default=0};
	__property DisplayMode  = {default=0};
	__property Enabled  = {default=1};
	__property FontSettings ;
	__property GroupIndex  = {default=0};
	__property HelpContext  = {default=0};
	__property HelpKeyword ;
	__property Hint ;
	__property ImageIndex  = {default=-1};
	__property Images ;
	__property InheritOptions  = {default=1};
	__property Layout  = {default=0};
	__property MaskOptions  = {default=0};
	__property MinHeight  = {default=0};
	__property MinWidth  = {default=0};
	__property Options  = {default=0};
	__property RadioItem  = {default=0};
	__property ShortCut  = {default=0};
	__property Stretch  = {default=0};
	__property Visible  = {default=1};
	__property OnAdjustFont ;
	__property OnDrawImage ;
	__property OnClick ;
	__property OnSelect ;
public:
	#pragma option push -w-inl
	/* TTBXCustomItem.Create */ inline __fastcall virtual TTBXItem(Classes::TComponent* AOwner) : TTBXCustomItem(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBXCustomItem.Destroy */ inline __fastcall virtual ~TTBXItem(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXItemViewer;
class PASCALIMPLEMENTATION TTBXItemViewer : public Tb2item::TTBItemViewer 
{
	typedef Tb2item::TTBItemViewer inherited;
	
private:
	bool FWide;
	
protected:
	virtual void __fastcall DoPaintCaption(Graphics::TCanvas* Canvas, const Types::TRect &ClientAreaRect, Types::TRect &CaptionRect, bool IsTextRotated, bool &PaintDefault);
	virtual int __fastcall GetAccRole(void);
	DYNAMIC tagSIZE __fastcall GetImageSize();
	virtual int __fastcall GetItemType(void);
	DYNAMIC unsigned __fastcall GetTextFlags(void);
	DYNAMIC tagSIZE __fastcall GetTextSize(Graphics::TCanvas* Canvas, const AnsiString Text, unsigned TextFlags, bool Rotated, int StateFlags);
	virtual bool __fastcall IsToolbarSize(void);
	virtual void __fastcall CalcSize(const Graphics::TCanvas* Canvas, int &AWidth, int &AHeight);
	virtual void __fastcall DrawItemImage(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall DoAdjustFont(Graphics::TFont* AFont, int StateFlags);
	virtual bool __fastcall GetImageShown(void);
	virtual bool __fastcall IsPtInButtonPart(int X, int Y);
	virtual void __fastcall MouseUp(int X, int Y, bool MouseWasDownOnMenu);
	virtual void __fastcall Paint(const Graphics::TCanvas* Canvas, const Types::TRect &ClientAreaRect, bool IsHoverItem, bool IsPushed, bool UseDisabledShadow);
	__property bool Wide = {read=FWide, write=FWide, default=1};
	
public:
	__fastcall virtual TTBXItemViewer(Tb2item::TTBView* AView, Tb2item::TTBCustomItem* AItem, int AGroupLevel);
	virtual bool __fastcall IsToolbarStyle(void);
public:
	#pragma option push -w-inl
	/* TTBItemViewer.Destroy */ inline __fastcall virtual ~TTBXItemViewer(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXSubmenuItem;
class PASCALIMPLEMENTATION TTBXSubmenuItem : public TTBXCustomItem 
{
	typedef TTBXCustomItem inherited;
	
private:
	bool __fastcall GetDropdownCombo(void);
	void __fastcall SetDropdownCombo(bool Value);
	
public:
	__fastcall virtual TTBXSubmenuItem(Classes::TComponent* AOwner);
	
__published:
	__property Action ;
	__property AlwaysSelectFirst  = {default=0};
	__property AutoCheck  = {default=0};
	__property Caption ;
	__property Checked  = {default=0};
	__property DisplayMode  = {default=0};
	__property bool DropdownCombo = {read=GetDropdownCombo, write=SetDropdownCombo, default=0};
	__property Enabled  = {default=1};
	__property FontSettings ;
	__property GroupIndex  = {default=0};
	__property HelpContext  = {default=0};
	__property HelpKeyword ;
	__property Hint ;
	__property ImageIndex  = {default=-1};
	__property Images ;
	__property InheritOptions  = {default=1};
	__property Layout  = {default=0};
	__property LinkSubitems ;
	__property MaskOptions  = {default=0};
	__property MinHeight  = {default=0};
	__property MinWidth  = {default=0};
	__property Options  = {default=0};
	__property RadioItem  = {default=0};
	__property ShortCut  = {default=0};
	__property Stretch  = {default=0};
	__property SubMenuImages ;
	__property ToolBoxPopup  = {default=0};
	__property Visible  = {default=1};
	__property OnAdjustFont ;
	__property OnDrawImage ;
	__property OnClick ;
	__property OnPopup ;
	__property OnSelect ;
public:
	#pragma option push -w-inl
	/* TTBXCustomItem.Destroy */ inline __fastcall virtual ~TTBXSubmenuItem(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXSeparatorItem;
class PASCALIMPLEMENTATION TTBXSeparatorItem : public Tb2item::TTBSeparatorItem 
{
	typedef Tb2item::TTBSeparatorItem inherited;
	
private:
	int FSize;
	void __fastcall SetSize(int Value);
	
public:
	__fastcall virtual TTBXSeparatorItem(Classes::TComponent* AOwner);
	virtual TMetaClass* __fastcall GetItemViewerClass(Tb2item::TTBView* AView);
	
__published:
	__property int Size = {read=FSize, write=SetSize, default=-1};
	__property MaskOptions  = {default=0};
	__property Options  = {default=0};
public:
	#pragma option push -w-inl
	/* TTBCustomItem.Destroy */ inline __fastcall virtual ~TTBXSeparatorItem(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXSeparatorItemViewer;
class PASCALIMPLEMENTATION TTBXSeparatorItemViewer : public Tb2item::TTBSeparatorItemViewer 
{
	typedef Tb2item::TTBSeparatorItemViewer inherited;
	
protected:
	virtual void __fastcall CalcSize(const Graphics::TCanvas* Canvas, int &AWidth, int &AHeight);
	virtual void __fastcall Paint(const Graphics::TCanvas* Canvas, const Types::TRect &ClientAreaRect, bool IsHoverItem, bool IsPushed, bool UseDisabledShadow);
	virtual bool __fastcall IsToolbarSize(void);
	
public:
	virtual bool __fastcall IsToolbarStyle(void);
public:
	#pragma option push -w-inl
	/* TTBItemViewer.Create */ inline __fastcall virtual TTBXSeparatorItemViewer(Tb2item::TTBView* AView, Tb2item::TTBCustomItem* AItem, int AGroupLevel) : Tb2item::TTBSeparatorItemViewer(AView, AItem, AGroupLevel) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBItemViewer.Destroy */ inline __fastcall virtual ~TTBXSeparatorItemViewer(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXPopupWindow;
class PASCALIMPLEMENTATION TTBXPopupWindow : public Tb2item::TTBPopupWindow 
{
	typedef Tb2item::TTBPopupWindow inherited;
	
private:
	#pragma pack(push, 1)
	Types::TRect FControlRect;
	#pragma pack(pop)
	
	Tbxutils::TShadows* FShadows;
	HIDESBASE MESSAGE void __fastcall CMHintShow(Forms::TCMHintShow &Message);
	MESSAGE void __fastcall TBMGetViewType(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Messages::TWMNCCalcSize &Message);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMPrint(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Messages::TWMEraseBkgnd &Message);
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanged(Messages::TWMWindowPosMsg &Message);
	MESSAGE void __fastcall WMTB2kPopupShowing(Messages::TMessage &Message);
	
protected:
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	virtual void __fastcall CreateShadow(void);
	virtual void __fastcall DestroyShadow(void);
	DYNAMIC Types::TPoint __fastcall GetNCSize();
	virtual bool __fastcall GetShowShadow(void);
	DYNAMIC TMetaClass* __fastcall GetViewClass(void);
	
public:
	__fastcall virtual ~TTBXPopupWindow(void);
	Graphics::TColor __fastcall GetFillColor(void);
public:
	#pragma option push -w-inl
	/* TTBPopupWindow.CreatePopupWindow */ inline __fastcall virtual TTBXPopupWindow(Classes::TComponent* AOwner, const Tb2item::TTBView* AParentView, const Tb2item::TTBCustomItem* AItem, const bool ACustomizing) : Tb2item::TTBPopupWindow(AOwner, AParentView, AItem, ACustomizing) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomControl.Create */ inline __fastcall virtual TTBXPopupWindow(Classes::TComponent* AOwner) : Tb2item::TTBPopupWindow(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTBXPopupWindow(HWND ParentWindow) : Tb2item::TTBPopupWindow(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXPopupView;
class PASCALIMPLEMENTATION TTBXPopupView : public Tb2item::TTBPopupView 
{
	typedef Tb2item::TTBPopupView inherited;
	
public:
	#pragma option push -w-inl
	/* TTBView.CreateView */ inline __fastcall virtual TTBXPopupView(Classes::TComponent* AOwner, Tb2item::TTBView* AParentView, Tb2item::TTBCustomItem* AParentItem, Controls::TWinControl* AWindow, bool AIsToolbar, bool ACustomizing, bool AUsePriorityList) : Tb2item::TTBPopupView(AOwner, AParentView, AParentItem, AWindow, AIsToolbar, ACustomizing, AUsePriorityList) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBView.Destroy */ inline __fastcall virtual ~TTBXPopupView(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TComponent.Create */ inline __fastcall virtual TTBXPopupView(Classes::TComponent* AOwner) : Tb2item::TTBPopupView(AOwner) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXToolbarView;
class PASCALIMPLEMENTATION TTBXToolbarView : public Tb2toolbar::TTBToolbarView 
{
	typedef Tb2toolbar::TTBToolbarView inherited;
	
protected:
	virtual void __fastcall GetMargins(Tb2item::TTBViewOrientation AOrientation, Types::TRect &Margins);
public:
	#pragma option push -w-inl
	/* TTBToolbarView.Create */ inline __fastcall virtual TTBXToolbarView(Classes::TComponent* AOwner) : Tb2toolbar::TTBToolbarView(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TTBView.CreateView */ inline __fastcall virtual TTBXToolbarView(Classes::TComponent* AOwner, Tb2item::TTBView* AParentView, Tb2item::TTBCustomItem* AParentItem, Controls::TWinControl* AWindow, bool AIsToolbar, bool ACustomizing, bool AUsePriorityList) : Tb2toolbar::TTBToolbarView(AOwner, AParentView, AParentItem, AWindow, AIsToolbar, ACustomizing, AUsePriorityList) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBView.Destroy */ inline __fastcall virtual ~TTBXToolbarView(void) { }
	#pragma option pop
	
};


#pragma option push -b-
enum TTBXItemTransparency { itAuto, itEnable, itDisable };
#pragma option pop

class DELPHICLASS TTBXToolbar;
class PASCALIMPLEMENTATION TTBXToolbar : public Tb2toolbar::TTBCustomToolbar 
{
	typedef Tb2toolbar::TTBCustomToolbar inherited;
	
private:
	Graphics::TColor FEffectiveColor;
	TTBXItemTransparency FItemTransparency;
	int FSnapDistance;
	HIDESBASE MESSAGE void __fastcall CMColorChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMControlChange(Controls::TCMControlChange &Message);
	HIDESBASE MESSAGE void __fastcall CMParentColorChanged(Messages::TMessage &Message);
	void __fastcall SetItemTransparency(const TTBXItemTransparency Value);
	void __fastcall SetSnapDistance(int Value);
	MESSAGE void __fastcall TBMGetViewType(Messages::TMessage &Message);
	MESSAGE void __fastcall TBMGetEffectiveColor(Messages::TMessage &Message);
	MESSAGE void __fastcall TBMThemeChange(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Messages::TWMEraseBkgnd &Message);
	HIDESBASE MESSAGE void __fastcall WMSize(Messages::TWMSize &Message);
	
protected:
	virtual void __fastcall DrawNCArea(const bool DrawToDC, const HDC ADC, const HRGN Clip);
	DYNAMIC TMetaClass* __fastcall GetChevronItemClass(void);
	DYNAMIC TMetaClass* __fastcall GetFloatingWindowParentClass(void);
	virtual void __fastcall GetToolbarInfo(/* out */ Tbxthemes::TTBXToolbarInfo &ToolbarInfo);
	DYNAMIC TMetaClass* __fastcall GetViewClass(void);
	virtual void __fastcall SetParent(Controls::TWinControl* AParent);
	void __fastcall UpdateEffectiveColor(void);
	
public:
	__fastcall virtual TTBXToolbar(Classes::TComponent* AOwner);
	__fastcall virtual ~TTBXToolbar(void);
	bool __fastcall Embedded(void);
	virtual Types::TPoint __fastcall GetFloatingBorderSize();
	void __fastcall UpdateChildColors(void);
	__property Graphics::TColor EffectiveColor = {read=FEffectiveColor, nodefault};
	
__published:
	__property ActivateParent  = {default=1};
	__property Align  = {default=0};
	__property AutoResize  = {default=1};
	__property BorderStyle  = {default=1};
	__property Caption ;
	__property ChevronHint ;
	__property ChevronMoveItems  = {default=1};
	__property ChevronPriorityForNewItems  = {default=0};
	__property CloseButton  = {default=1};
	__property CloseButtonWhenDocked  = {default=0};
	__property CurrentDock ;
	__property DblClickUndock  = {default=0};
	__property DefaultDock ;
	__property DockableTo  = {default=15};
	__property DockMode  = {default=0};
	__property DockPos  = {default=-1};
	__property DockRow  = {default=0};
	__property DragHandleStyle  = {default=2};
	__property FloatingMode  = {default=0};
	__property Font ;
	__property FullSize  = {default=0};
	__property HideWhenInactive  = {default=1};
	__property Images ;
	__property Items ;
	__property TTBXItemTransparency ItemTransparency = {read=FItemTransparency, write=SetItemTransparency, default=0};
	__property LastDock ;
	__property LinkSubitems ;
	__property MenuBar  = {default=0};
	__property Options  = {default=0};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ProcessShortCuts  = {default=0};
	__property Resizable  = {default=1};
	__property ShowCaption  = {default=1};
	__property ShowHint ;
	__property ShrinkMode  = {default=2};
	__property SmoothDrag  = {default=1};
	__property int SnapDistance = {read=FSnapDistance, write=SetSnapDistance, default=0};
	__property Stretch  = {default=0};
	__property SystemFont  = {default=1};
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=0};
	__property UpdateActions  = {default=1};
	__property UseLastDock  = {default=1};
	__property Visible  = {default=1};
	__property Color  = {default=536870911};
	__property OnClose ;
	__property OnCloseQuery ;
	__property OnContextPopup ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnMouseDown ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnMove ;
	__property OnRecreated ;
	__property OnRecreating ;
	__property OnDockChanged ;
	__property OnDockChanging ;
	__property OnDockChangingHidden ;
	__property OnResize ;
	__property OnShortCut ;
	__property OnVisibleChanged ;
	__property OnGetBaseSize ;
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTBXToolbar(HWND ParentWindow) : Tb2toolbar::TTBCustomToolbar(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXChevronItem;
class PASCALIMPLEMENTATION TTBXChevronItem : public Tb2toolbar::TTBChevronItem 
{
	typedef Tb2toolbar::TTBChevronItem inherited;
	
public:
	virtual void __fastcall GetPopupPosition(Tb2item::TTBView* ParentView, Tb2item::TTBPopupWindow* PopupWindow, Tb2item::TTBPopupPositionRec &PopupPositionRec);
	virtual TMetaClass* __fastcall GetPopupWindowClass(void);
	virtual TMetaClass* __fastcall GetItemViewerClass(Tb2item::TTBView* AView);
public:
	#pragma option push -w-inl
	/* TTBChevronItem.Create */ inline __fastcall virtual TTBXChevronItem(Classes::TComponent* AOwner) : Tb2toolbar::TTBChevronItem(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TTBCustomItem.Destroy */ inline __fastcall virtual ~TTBXChevronItem(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXChevronItemViewer;
class PASCALIMPLEMENTATION TTBXChevronItemViewer : public Tb2item::TTBItemViewer 
{
	typedef Tb2item::TTBItemViewer inherited;
	
protected:
	virtual void __fastcall Paint(const Graphics::TCanvas* Canvas, const Types::TRect &ClientAreaRect, bool IsHoverItem, bool IsPushed, bool UseDisabledShadow);
public:
	#pragma option push -w-inl
	/* TTBItemViewer.Create */ inline __fastcall virtual TTBXChevronItemViewer(Tb2item::TTBView* AView, Tb2item::TTBCustomItem* AItem, int AGroupLevel) : Tb2item::TTBItemViewer(AView, AItem, AGroupLevel) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBItemViewer.Destroy */ inline __fastcall virtual ~TTBXChevronItemViewer(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXChevronPopupWindow;
class PASCALIMPLEMENTATION TTBXChevronPopupWindow : public TTBXPopupWindow 
{
	typedef TTBXPopupWindow inherited;
	
public:
	#pragma option push -w-inl
	/* TTBXPopupWindow.Destroy */ inline __fastcall virtual ~TTBXChevronPopupWindow(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TTBPopupWindow.CreatePopupWindow */ inline __fastcall virtual TTBXChevronPopupWindow(Classes::TComponent* AOwner, const Tb2item::TTBView* AParentView, const Tb2item::TTBCustomItem* AItem, const bool ACustomizing) : TTBXPopupWindow(AOwner, AParentView, AItem, ACustomizing) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomControl.Create */ inline __fastcall virtual TTBXChevronPopupWindow(Classes::TComponent* AOwner) : TTBXPopupWindow(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTBXChevronPopupWindow(HWND ParentWindow) : TTBXPopupWindow(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXRootItem;
class PASCALIMPLEMENTATION TTBXRootItem : public Tb2item::TTBRootItem 
{
	typedef Tb2item::TTBRootItem inherited;
	
private:
	#pragma pack(push, 1)
	Types::TRect FPopupControlRect;
	#pragma pack(pop)
	
	
protected:
	virtual Tb2item::TTBPopupWindow* __fastcall CreatePopupEx(bool SelectFirstItem, const Types::TRect &AControlRect, Tb2item::TTBPopupAlignment Alignment);
	virtual TMetaClass* __fastcall GetPopupWindowClass(void);
	virtual void __fastcall GetPopupPosition(Tb2item::TTBView* ParentView, Tb2item::TTBPopupWindow* PopupWindow, Tb2item::TTBPopupPositionRec &PopupPositionRec);
	void __fastcall OpenPopupEx(const bool SelectFirstItem, const bool TrackRightButton, const Types::TRect &ControlRect, const Tb2item::TTBPopupAlignment Alignment);
	void __fastcall PopupEx(const Types::TRect &ControlRect, bool TrackRightButton, Tb2item::TTBPopupAlignment Alignment = (Tb2item::TTBPopupAlignment)(0x0));
public:
	#pragma option push -w-inl
	/* TTBCustomItem.Create */ inline __fastcall virtual TTBXRootItem(Classes::TComponent* AOwner) : Tb2item::TTBRootItem(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBCustomItem.Destroy */ inline __fastcall virtual ~TTBXRootItem(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXPopupMenu;
class PASCALIMPLEMENTATION TTBXPopupMenu : public Tb2item::TTBPopupMenu 
{
	typedef Tb2item::TTBPopupMenu inherited;
	
private:
	bool FToolBoxPopup;
	MESSAGE void __fastcall TBMGetViewType(Messages::TMessage &Message);
	
protected:
	DYNAMIC TMetaClass* __fastcall GetRootItemClass(void);
	
public:
	void __fastcall PopupEx(const Types::TRect &ControlRect);
	__property bool ToolBoxPopup = {read=FToolBoxPopup, write=FToolBoxPopup, default=0};
public:
	#pragma option push -w-inl
	/* TTBPopupMenu.Create */ inline __fastcall virtual TTBXPopupMenu(Classes::TComponent* AOwner) : Tb2item::TTBPopupMenu(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBPopupMenu.Destroy */ inline __fastcall virtual ~TTBXPopupMenu(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXFloatingWindowParent;
class PASCALIMPLEMENTATION TTBXFloatingWindowParent : public Tb2dock::TTBFloatingWindowParent 
{
	typedef Tb2dock::TTBFloatingWindowParent inherited;
	
private:
	bool FCloseButtonHover;
	int FSnapDistance;
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Messages::TMessage &Message);
	MESSAGE void __fastcall WMNCMouseLeave(Messages::TMessage &Message);
	MESSAGE void __fastcall WMNCMouseMove(Messages::TWMNCHitMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanging(Messages::TWMWindowPosMsg &Message);
	
protected:
	void __fastcall CancelNCHover(void);
	DYNAMIC void __fastcall DrawNCArea(const bool DrawToDC, const HDC ADC, const HRGN Clip, Tb2dock::TTBToolWindowNCRedrawWhat RedrawWhat);
	__property bool CloseButtonHover = {read=FCloseButtonHover, nodefault};
	
public:
	__property int SnapDistance = {read=FSnapDistance, write=FSnapDistance, default=0};
public:
	#pragma option push -w-inl
	/* TTBFloatingWindowParent.Create */ inline __fastcall virtual TTBXFloatingWindowParent(Classes::TComponent* AOwner) : Tb2dock::TTBFloatingWindowParent(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBFloatingWindowParent.Destroy */ inline __fastcall virtual ~TTBXFloatingWindowParent(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTBXFloatingWindowParent(Classes::TComponent* AOwner, int Dummy) : Tb2dock::TTBFloatingWindowParent(AOwner, Dummy) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTBXFloatingWindowParent(HWND ParentWindow) : Tb2dock::TTBFloatingWindowParent(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXDock;
class PASCALIMPLEMENTATION TTBXDock : public Tb2dock::TTBDock 
{
	typedef Tb2dock::TTBDock inherited;
	
private:
	bool FMoving;
	bool FResizing;
	bool FUseParentBackground;
	HIDESBASE MESSAGE void __fastcall CMColorChanged(Messages::TMessage &Message);
	MESSAGE void __fastcall TBMGetEffectiveColor(Messages::TMessage &Message);
	MESSAGE void __fastcall TBMThemeChange(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Messages::TWMEraseBkgnd &Message);
	HIDESBASE MESSAGE void __fastcall WMMove(Messages::TWMMove &Message);
	HIDESBASE MESSAGE void __fastcall WMSize(Messages::TWMSize &Message);
	
protected:
	virtual bool __fastcall ThemedBackground(void);
	virtual void __fastcall DrawBackground(HDC DC, const Types::TRect &DrawRect);
	DYNAMIC void __fastcall Resize(void);
	void __fastcall SetUseParentBackground(bool Value);
	virtual bool __fastcall UsingBackground(void);
	
public:
	__fastcall virtual TTBXDock(Classes::TComponent* AOwner);
	__fastcall virtual ~TTBXDock(void);
	
__published:
	__property Color  = {default=536870911};
	__property bool UseParentBackground = {read=FUseParentBackground, write=SetUseParentBackground, default=0};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTBXDock(HWND ParentWindow) : Tb2dock::TTBDock(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
#define TBXVersion  (2.100000E+00)
#define TBXVersionString "2.1"
#define TBXVersionText "TBX version 2.1"
static const Word TBM_THEMECHANGE = 0x53a;
static const Word TBM_GETVIEWTYPE = 0x4ed;
static const Word TBM_GETEFFECTIVECOLOR = 0x4ee;
extern PACKAGE Tbxthemes::TTBXTheme* CurrentTheme;
extern PACKAGE void __fastcall AddThemeNotification(System::TObject* AObject);
extern PACKAGE void __fastcall RemoveThemeNotification(System::TObject* AObject);
extern PACKAGE Graphics::TColor __fastcall GetEffectiveColor(Controls::TControl* C);
extern PACKAGE void __fastcall DrawParentBackground(Controls::TControl* Control, HDC DC, const Types::TRect &R);
extern PACKAGE int __fastcall GetViewType(Tb2item::TTBView* View);
extern PACKAGE int __fastcall GetWinViewType(Controls::TControl* Window);
extern PACKAGE bool __fastcall IsFloating(int ViewType);
extern PACKAGE int __fastcall GetPopupMargin(Tb2item::TTBItemViewer* ItemViewer);
extern PACKAGE void __fastcall AddToList(Classes::TList* &List, void * Item);
extern PACKAGE void __fastcall RemoveFromList(Classes::TList* &List, void * Item);
extern PACKAGE int __fastcall GetStateFlags(const Tbxthemes::TTBXItemInfo &ItemInfo);
extern PACKAGE Graphics::TColor __fastcall GetTBXTextColor(int StateFlags);
extern PACKAGE void __fastcall DrawTBXCaption(Graphics::TCanvas* Canvas, const Types::TRect &Rect, const AnsiString Text, unsigned Format, int StateFlags);
extern PACKAGE void __fastcall DrawTBXImage(Graphics::TCanvas* Canvas, const Types::TRect &Rect, Imglist::TCustomImageList* ImageList, int ImageIndex, int StateFlags);
extern PACKAGE void __fastcall AddTBXColor(Graphics::TColor &AColor, const AnsiString AName);
extern PACKAGE AnsiString __fastcall TBXColorToString(Graphics::TColor Color);
extern PACKAGE bool __fastcall TBXIdentToColor(const AnsiString Ident, int &Color);
extern PACKAGE Graphics::TColor __fastcall TBXStringToColor(AnsiString S);
extern PACKAGE void __fastcall TBXGetColorValues(Classes::TGetStrProc Proc);
extern PACKAGE void __fastcall TBXSetTheme(const AnsiString AThemeName);
extern PACKAGE AnsiString __fastcall TBXCurrentTheme();

}	/* namespace Tbx */
using namespace Tbx;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TBX
