// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TBXStatusBars.pas' rev: 6.00

#ifndef TBXStatusBarsHPP
#define TBXStatusBarsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Menus.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <TB2Item.hpp>	// Pascal unit
#include <TBXThemes.hpp>	// Pascal unit
#include <TBX.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tbxstatusbars
{
//-- type declarations -------------------------------------------------------
typedef Shortint TPercent;

class DELPHICLASS TTBXStatusPanel;
class DELPHICLASS TTBXCustomStatusBar;
class PASCALIMPLEMENTATION TTBXStatusPanel : public Classes::TCollectionItem 
{
	typedef Classes::TCollectionItem inherited;
	
private:
	Classes::TAlignment FAlignment;
	AnsiString FCaption;
	Controls::TControl* FControl;
	bool FEnabled;
	bool FFramed;
	Tbx::TFontSettings* FFontSettings;
	AnsiString FHint;
	Imglist::TImageIndex FImageIndex;
	int FMaxSize;
	int FSize;
	TPercent FStretchPriority;
	int FTag;
	Tbx::TTextWrapping FTextTruncation;
	TPercent FViewPriority;
	void __fastcall FontSettingsChanged(System::TObject* Sender);
	void __fastcall SetAlignment(Classes::TAlignment Value);
	void __fastcall SetCaption(const AnsiString Value);
	void __fastcall SetControl(Controls::TControl* Value);
	void __fastcall SetEnabled(bool Value);
	void __fastcall SetFramed(bool Value);
	void __fastcall SetImageIndex(Imglist::TImageIndex Value);
	void __fastcall SetMaxSize(int Value);
	void __fastcall SetSize(int Value);
	void __fastcall SetStretchPriority(TPercent Value);
	void __fastcall SetTextTruncation(Tbx::TTextTruncation Value);
	void __fastcall SetViewPriority(TPercent Value);
	void __fastcall SetFontSettings(const Tbx::TFontSettings* Value);
	
protected:
	#pragma pack(push, 1)
	Types::TRect CachedBounds;
	#pragma pack(pop)
	
	int CachedSize;
	bool CachedVisible;
	bool CachedGripper;
	TTBXCustomStatusBar* __fastcall StatusBar(void);
	virtual AnsiString __fastcall GetDisplayName();
	
public:
	__fastcall virtual TTBXStatusPanel(Classes::TCollection* Collection);
	__fastcall virtual ~TTBXStatusPanel(void);
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	__property Types::TRect BoundsRect = {read=CachedBounds};
	__property bool Visible = {read=CachedVisible, nodefault};
	
__published:
	__property Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
	__property AnsiString Caption = {read=FCaption, write=SetCaption};
	__property Controls::TControl* Control = {read=FControl, write=SetControl};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
	__property bool Framed = {read=FFramed, write=SetFramed, default=1};
	__property Tbx::TFontSettings* FontSettings = {read=FFontSettings, write=SetFontSettings};
	__property AnsiString Hint = {read=FHint, write=FHint};
	__property Imglist::TImageIndex ImageIndex = {read=FImageIndex, write=SetImageIndex, default=-1};
	__property int MaxSize = {read=FMaxSize, write=SetMaxSize, default=0};
	__property TPercent ViewPriority = {read=FViewPriority, write=SetViewPriority, default=100};
	__property int Size = {read=FSize, write=SetSize, default=50};
	__property TPercent StretchPriority = {read=FStretchPriority, write=SetStretchPriority, default=0};
	__property int Tag = {read=FTag, write=FTag, nodefault};
	__property Tbx::TTextTruncation TextTruncation = {read=FTextTruncation, write=SetTextTruncation, default=0};
};


class DELPHICLASS TTBXStatusPanels;
class PASCALIMPLEMENTATION TTBXStatusPanels : public Classes::TCollection 
{
	typedef Classes::TCollection inherited;
	
public:
	TTBXStatusPanel* operator[](int Index) { return Items[Index]; }
	
private:
	TTBXCustomStatusBar* FStatusBar;
	HIDESBASE TTBXStatusPanel* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TTBXStatusPanel* Value);
	
protected:
	DYNAMIC Classes::TPersistent* __fastcall GetOwner(void);
	virtual void __fastcall Update(Classes::TCollectionItem* Item);
	
public:
	__fastcall TTBXStatusPanels(TTBXCustomStatusBar* AStatusBar);
	HIDESBASE TTBXStatusPanel* __fastcall Add(void);
	TTBXStatusPanel* __fastcall FindPanel(Controls::TControl* AControl);
	__property TTBXCustomStatusBar* StatusBar = {read=FStatusBar};
	__property TTBXStatusPanel* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
public:
	#pragma option push -w-inl
	/* TCollection.Destroy */ inline __fastcall virtual ~TTBXStatusPanels(void) { }
	#pragma option pop
	
};


typedef void __fastcall (__closure *TSBAdjustContentRect)(TTBXCustomStatusBar* Sender, TTBXStatusPanel* Panel, Types::TRect &ARect);

typedef void __fastcall (__closure *TSBAdjustFont)(TTBXCustomStatusBar* Sender, TTBXStatusPanel* Panel, Graphics::TFont* AFont);

typedef void __fastcall (__closure *TSBPanelEvent)(TTBXCustomStatusBar* Sender, TTBXStatusPanel* Panel);

class PASCALIMPLEMENTATION TTBXCustomStatusBar : public Controls::TCustomControl 
{
	typedef Controls::TCustomControl inherited;
	
private:
	TTBXStatusPanels* FPanels;
	Imglist::TChangeLink* FImageChangeLink;
	Imglist::TCustomImageList* FImages;
	bool FSimplePanel;
	AnsiString FSimpleText;
	bool FSizeGrip;
	int FUpdateCount;
	bool FUseSystemFont;
	TSBAdjustContentRect FOnAdjustContentRect;
	TSBAdjustFont FOnAdjustFont;
	TSBPanelEvent FOnPanelClick;
	TSBPanelEvent FOnPanelDblClick;
	bool FFixAlign;
	MESSAGE void __fastcall CMControlChange(Controls::TCMControlChange &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Forms::TCMHintShow &Message);
	HIDESBASE MESSAGE void __fastcall CMVisibleChanged(Messages::TMessage &Message);
	void __fastcall ImageListChange(System::TObject* Sender);
	void __fastcall SetImages(Imglist::TCustomImageList* Value);
	void __fastcall SetPanels(TTBXStatusPanels* Value);
	void __fastcall SetSimplePanel(bool Value);
	void __fastcall SetSimpleText(const AnsiString Value);
	void __fastcall SetSizeGrip(bool Value);
	void __fastcall SetUseSystemFont(bool Value);
	MESSAGE void __fastcall TBMThemeChange(void *Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Messages::TWMEraseBkgnd &Message);
	HIDESBASE MESSAGE void __fastcall WMNCHitTest(Messages::TWMNCHitTest &Message);
	
protected:
	Tbxthemes::TTBXMargins CachedPanelMargins;
	virtual void __fastcall AdjustPanelContentRect(TTBXStatusPanel* APanel, Types::TRect &ARect);
	virtual void __fastcall AlignControls(Controls::TControl* AControl, Types::TRect &Rect);
	void __fastcall BeginUpdate(void);
	DYNAMIC void __fastcall Change(void);
	DYNAMIC void __fastcall ChangeScale(int M, int D);
	DYNAMIC void __fastcall Click(void);
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd(void);
	DYNAMIC void __fastcall DblClick(void);
	virtual void __fastcall DoAdjustFont(TTBXStatusPanel* APanel, Graphics::TFont* AFont);
	virtual void __fastcall DoPanelClick(TTBXStatusPanel* APanel);
	virtual void __fastcall DoPanelDblClick(TTBXStatusPanel* APanel);
	void __fastcall EndUpdate(void);
	Types::TRect __fastcall GetGripperRect();
	virtual void __fastcall Loaded(void);
	bool __fastcall IsSizeGripVisible(void);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	virtual void __fastcall Paint(void);
	virtual void __fastcall PaintPanel(const Types::TRect &ARect, TTBXStatusPanel* APanel, bool IsLast);
	DYNAMIC void __fastcall Resize(void);
	virtual void __fastcall UpdateCache(void);
	virtual void __fastcall UpdatePanels(void);
	
public:
	__fastcall virtual TTBXCustomStatusBar(Classes::TComponent* AOwner);
	__fastcall virtual ~TTBXCustomStatusBar(void);
	TTBXStatusPanel* __fastcall GetPanelAt(const Types::TPoint &Pt)/* overload */;
	TTBXStatusPanel* __fastcall GetPanelAt(int X, int Y)/* overload */;
	Types::TRect __fastcall GetPanelRect(TTBXStatusPanel* APanel);
	DYNAMIC void __fastcall FlipChildren(bool AllLevels);
	__property Align  = {default=2};
	__property bool FixAlign = {read=FFixAlign, write=FFixAlign, default=0};
	__property DoubleBuffered  = {default=1};
	__property Imglist::TCustomImageList* Images = {read=FImages, write=SetImages};
	__property TTBXStatusPanels* Panels = {read=FPanels, write=SetPanels};
	__property bool SimplePanel = {read=FSimplePanel, write=SetSimplePanel, default=0};
	__property AnsiString SimpleText = {read=FSimpleText, write=SetSimpleText};
	__property bool SizeGrip = {read=FSizeGrip, write=SetSizeGrip, default=1};
	__property bool UseSystemFont = {read=FUseSystemFont, write=SetUseSystemFont, nodefault};
	__property TSBAdjustContentRect OnAdjustContentRect = {read=FOnAdjustContentRect, write=FOnAdjustContentRect};
	__property TSBAdjustFont OnAdjustFont = {read=FOnAdjustFont, write=FOnAdjustFont};
	__property TSBPanelEvent OnPanelClick = {read=FOnPanelClick, write=FOnPanelClick};
	__property TSBPanelEvent OnPanelDblClick = {read=FOnPanelDblClick, write=FOnPanelDblClick};
	
__published:
	__property Height  = {default=22};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTBXCustomStatusBar(HWND ParentWindow) : Controls::TCustomControl(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXStatusBar;
class PASCALIMPLEMENTATION TTBXStatusBar : public TTBXCustomStatusBar 
{
	typedef TTBXCustomStatusBar inherited;
	
__published:
	__property Action ;
	__property Align  = {default=2};
	__property Anchors  = {default=3};
	__property Constraints ;
	__property Ctl3D ;
	__property DoubleBuffered  = {default=1};
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property FixAlign  = {default=0};
	__property Font ;
	__property Images ;
	__property Panels ;
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property SimplePanel  = {default=0};
	__property SimpleText ;
	__property SizeGrip  = {default=1};
	__property ShowHint ;
	__property UseSystemFont ;
	__property Visible  = {default=1};
	__property OnAdjustContentRect ;
	__property OnClick ;
	__property OnContextPopup ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnMouseDown ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnPanelClick ;
	__property OnPanelDblClick ;
	__property OnResize ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TTBXCustomStatusBar.Create */ inline __fastcall virtual TTBXStatusBar(Classes::TComponent* AOwner) : TTBXCustomStatusBar(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBXCustomStatusBar.Destroy */ inline __fastcall virtual ~TTBXStatusBar(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTBXStatusBar(HWND ParentWindow) : TTBXCustomStatusBar(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Tbxstatusbars */
using namespace Tbxstatusbars;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TBXStatusBars
