// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TB2Toolbar.pas' rev: 6.00

#ifndef TB2ToolbarHPP
#define TB2ToolbarHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <TB2Dock.hpp>	// Pascal unit
#include <TB2Item.hpp>	// Pascal unit
#include <ActnList.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
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

namespace Tb2toolbar
{
//-- type declarations -------------------------------------------------------
typedef TMetaClass*TTBChevronItemClass;

typedef TMetaClass*TTBToolbarViewClass;

class DELPHICLASS TTBToolbarView;
class DELPHICLASS TTBCustomToolbar;
class DELPHICLASS TTBChevronItem;
class PASCALIMPLEMENTATION TTBChevronItem : public Tb2item::TTBCustomItem 
{
	typedef Tb2item::TTBCustomItem inherited;
	
private:
	TTBCustomToolbar* FToolbar;
	AnsiString __fastcall GetDefaultHint();
	
public:
	__fastcall virtual TTBChevronItem(Classes::TComponent* AOwner);
	virtual Tb2item::TTBView* __fastcall GetChevronParentView(void);
	virtual TMetaClass* __fastcall GetItemViewerClass(Tb2item::TTBView* AView);
public:
	#pragma option push -w-inl
	/* TTBCustomItem.Destroy */ inline __fastcall virtual ~TTBChevronItem(void) { }
	#pragma option pop
	
};


#pragma option push -b-
enum TTBChevronPriorityForNewItems { tbcpHighest, tbcpLowest };
#pragma option pop

typedef void __fastcall (__closure *TToolbarGetBaseSizeEvent)(TTBCustomToolbar* Toolbar, Types::TPoint &ASize);

class PASCALIMPLEMENTATION TTBCustomToolbar : public Tb2dock::TTBCustomDockableWindow 
{
	typedef Tb2dock::TTBCustomDockableWindow inherited;
	
private:
	#pragma pack(push, 1)
	Types::TPoint FBaseSize;
	#pragma pack(pop)
	
	TTBChevronItem* FChevronItem;
	bool FChevronMoveItems;
	TTBChevronPriorityForNewItems FChevronPriorityForNewItems;
	int FDisableAlignArrange;
	int FFloatingWidth;
	bool FIgnoreMouseLeave;
	Tb2item::TTBRootItem* FItem;
	int FLastWrappedLines;
	bool FMenuBar;
	Forms::TShortCutEvent FOnShortCut;
	bool FProcessShortCuts;
	bool FMainWindowHookInstalled;
	Tb2dock::TTBShrinkMode FShrinkMode;
	void *FSizeData;
	bool FSystemFont;
	bool FUpdateActions;
	TToolbarGetBaseSizeEvent FOnGetBaseSize;
	void __fastcall CancelHover(void);
	int __fastcall CalcChevronOffset(const Tb2dock::TTBDock* ADock, const Tb2item::TTBViewOrientation AOrientation);
	int __fastcall CalcWrapOffset(const Tb2dock::TTBDock* ADock);
	Tb2item::TTBControlItem* __fastcall CreateWrapper(int Index, Controls::TControl* Ctl);
	Tb2item::TTBControlItem* __fastcall FindWrapper(Controls::TControl* Ctl);
	AnsiString __fastcall GetChevronHint();
	Imglist::TCustomImageList* __fastcall GetImages(void);
	Tb2item::TTBCustomItem* __fastcall GetItems(void);
	Tb2item::TTBCustomItem* __fastcall GetLinkSubitems(void);
	Tb2item::TTBItemOptions __fastcall GetOptions(void);
	void __fastcall InstallMainWindowHook(void);
	bool __fastcall IsChevronHintStored(void);
	/*         class method */ static bool __fastcall MainWindowHook(TMetaClass* vmt, Messages::TMessage &Message);
	void __fastcall SetChevronHint(const AnsiString Value);
	void __fastcall SetChevronMoveItems(bool Value);
	void __fastcall SetChevronPriorityForNewItems(TTBChevronPriorityForNewItems Value);
	void __fastcall SetFloatingWidth(int Value);
	void __fastcall SetImages(Imglist::TCustomImageList* Value);
	void __fastcall SetLinkSubitems(Tb2item::TTBCustomItem* Value);
	void __fastcall SetMainWindowHook(void);
	void __fastcall SetMenuBar(bool Value);
	void __fastcall SetOptions(Tb2item::TTBItemOptions Value);
	void __fastcall SetProcessShortCuts(bool Value);
	void __fastcall SetShrinkMode(Tb2dock::TTBShrinkMode Value);
	void __fastcall SetSystemFont(bool Value);
	void __fastcall UninstallMainWindowHook(void);
	void __fastcall UpdateViewProperties(void);
	HIDESBASE MESSAGE void __fastcall CMColorChanged(Messages::TMessage &Message);
	MESSAGE void __fastcall CMControlChange(Controls::TCMControlChange &Message);
	HIDESBASE MESSAGE void __fastcall CMControlListChange(Controls::TCMControlListChange &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMDialogChar(Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall CMDialogKey(Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Forms::TCMHintShow &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMShowHintChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMWinIniChange(Messages::TWMSettingChange &Message);
	HIDESBASE MESSAGE void __fastcall WMCancelMode(Messages::TWMNoParams &Message);
	MESSAGE void __fastcall WMGetObject(Messages::TMessage &Message);
	MESSAGE void __fastcall WMMouseLeave(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMNCMouseMove(Messages::TWMNCHitMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMSetCursor(Messages::TWMSetCursor &Message);
	HIDESBASE MESSAGE void __fastcall WMSysCommand(Messages::TWMSysCommand &Message);
	
protected:
	Tb2item::TTBCustomItem* FMDIButtonsItem;
	Tb2item::TTBCustomItem* FMDISystemMenuItem;
	TTBToolbarView* FView;
	virtual void __fastcall AlignControls(Controls::TControl* AControl, Types::TRect &Rect);
	DYNAMIC void __fastcall BuildPotentialSizesList(Classes::TList* SizesList);
	virtual void __fastcall ControlExistsAtPos(const Types::TPoint &P, bool &ControlExists);
	virtual Types::TPoint __fastcall DoArrange(bool CanMoveControls, Tb2dock::TTBDockType PreviousDockType, bool NewFloating, Tb2dock::TTBDock* NewDock);
	DYNAMIC void __fastcall DoContextPopup(const Types::TPoint &MousePos, bool &Handled);
	virtual void __fastcall GetBaseSize(Types::TPoint &ASize);
	DYNAMIC void __fastcall GetChildren(Classes::TGetChildProc Proc, Classes::TComponent* Root);
	void __fastcall GetMinBarSize(Types::TPoint &MinimumSize);
	virtual void __fastcall GetMinShrinkSize(int &AMinimumSize);
	virtual Tb2dock::TTBShrinkMode __fastcall GetShrinkMode(void);
	DYNAMIC TMetaClass* __fastcall GetChevronItemClass(void);
	DYNAMIC TMetaClass* __fastcall GetItemClass(void);
	DYNAMIC TMetaClass* __fastcall GetViewClass(void);
	virtual void __fastcall Loaded(void);
	DYNAMIC void __fastcall MouseDown(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Paint(void);
	DYNAMIC void __fastcall ResizeBegin(Tb2dock::TTBSizeHandle ASizeHandle);
	DYNAMIC void __fastcall ResizeTrack(Types::TRect &Rect, const Types::TRect &OrigRect);
	DYNAMIC void __fastcall ResizeTrackAccept(void);
	DYNAMIC void __fastcall ResizeEnd(void);
	DYNAMIC void __fastcall SetChildOrder(Classes::TComponent* Child, int Order);
	__property bool SystemFont = {read=FSystemFont, write=SetSystemFont, default=1};
	__property Forms::TShortCutEvent OnShortCut = {read=FOnShortCut, write=FOnShortCut};
	
public:
	__fastcall virtual TTBCustomToolbar(Classes::TComponent* AOwner);
	__fastcall virtual ~TTBCustomToolbar(void);
	HIDESBASE void __fastcall BeginUpdate(void);
	HIDESBASE void __fastcall EndUpdate(void);
	void __fastcall CreateWrappersForAllControls(void);
	DYNAMIC void __fastcall GetTabOrderList(Classes::TList* List);
	virtual void __fastcall InitiateAction(void);
	bool __fastcall IsShortCut(Messages::TWMKey &Message);
	bool __fastcall KeyboardOpen(char Key, bool RequirePrimaryAccel);
	DYNAMIC void __fastcall ReadPositionData(const Tb2dock::TTBReadPositionData &Data);
	DYNAMIC void __fastcall WritePositionData(const Tb2dock::TTBWritePositionData &Data);
	__property AnsiString ChevronHint = {read=GetChevronHint, write=SetChevronHint, stored=IsChevronHintStored};
	__property bool ChevronMoveItems = {read=FChevronMoveItems, write=SetChevronMoveItems, default=1};
	__property TTBChevronPriorityForNewItems ChevronPriorityForNewItems = {read=FChevronPriorityForNewItems, write=SetChevronPriorityForNewItems, default=0};
	__property int FloatingWidth = {read=FFloatingWidth, write=SetFloatingWidth, default=0};
	__property Imglist::TCustomImageList* Images = {read=GetImages, write=SetImages};
	__property Tb2item::TTBRootItem* Items = {read=FItem};
	__property Tb2item::TTBCustomItem* LinkSubitems = {read=GetLinkSubitems, write=SetLinkSubitems};
	__property Tb2item::TTBItemOptions Options = {read=GetOptions, write=SetOptions, default=0};
	__property bool MenuBar = {read=FMenuBar, write=SetMenuBar, default=0};
	__property bool ProcessShortCuts = {read=FProcessShortCuts, write=SetProcessShortCuts, default=0};
	__property Tb2dock::TTBShrinkMode ShrinkMode = {read=FShrinkMode, write=SetShrinkMode, default=2};
	__property bool UpdateActions = {read=FUpdateActions, write=FUpdateActions, default=1};
	__property TTBToolbarView* View = {read=FView};
	__property TToolbarGetBaseSizeEvent OnGetBaseSize = {read=FOnGetBaseSize, write=FOnGetBaseSize};
	
__published:
	__property Hint  = {stored=false};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTBCustomToolbar(HWND ParentWindow) : Tb2dock::TTBCustomDockableWindow(ParentWindow) { }
	#pragma option pop
	
private:
	void *__ITBItems;	/* Tb2item::ITBItems */
	
public:
	operator ITBItems*(void) { return (ITBItems*)&__ITBItems; }
	
};


class PASCALIMPLEMENTATION TTBToolbarView : public Tb2item::TTBView 
{
	typedef Tb2item::TTBView inherited;
	
private:
	TTBCustomToolbar* FToolbar;
	
protected:
	virtual void __fastcall AutoSize(int AWidth, int AHeight);
	virtual void __fastcall DoUpdatePositions(Types::TPoint &ASize);
	virtual Tb2item::TTBCustomItem* __fastcall GetChevronItem(void);
	virtual Tb2item::TTBCustomItem* __fastcall GetMDIButtonsItem(void);
	virtual Tb2item::TTBCustomItem* __fastcall GetMDISystemMenuItem(void);
	
public:
	__fastcall virtual TTBToolbarView(Classes::TComponent* AOwner);
	virtual Graphics::TFont* __fastcall GetFont(void);
	virtual void __fastcall InvalidatePositions(void);
public:
	#pragma option push -w-inl
	/* TTBView.CreateView */ inline __fastcall virtual TTBToolbarView(Classes::TComponent* AOwner, Tb2item::TTBView* AParentView, Tb2item::TTBCustomItem* AParentItem, Controls::TWinControl* AWindow, bool AIsToolbar, bool ACustomizing, bool AUsePriorityList) : Tb2item::TTBView(AOwner, AParentView, AParentItem, AWindow, AIsToolbar, ACustomizing, AUsePriorityList) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBView.Destroy */ inline __fastcall virtual ~TTBToolbarView(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBToolbar;
class PASCALIMPLEMENTATION TTBToolbar : public TTBCustomToolbar 
{
	typedef TTBCustomToolbar inherited;
	
__published:
	__property ActivateParent  = {default=1};
	__property Align  = {default=0};
	__property Anchors  = {default=3};
	__property AutoResize  = {default=1};
	__property BorderStyle  = {default=1};
	__property Caption ;
	__property ChevronHint ;
	__property ChevronMoveItems  = {default=1};
	__property ChevronPriorityForNewItems  = {default=0};
	__property CloseButton  = {default=1};
	__property CloseButtonWhenDocked  = {default=0};
	__property Color  = {default=-2147483633};
	__property CurrentDock ;
	__property DefaultDock ;
	__property DockableTo  = {default=15};
	__property DockMode  = {default=0};
	__property DockPos  = {default=-1};
	__property DockRow  = {default=0};
	__property DragHandleStyle  = {default=2};
	__property FloatingMode  = {default=0};
	__property FloatingWidth  = {default=0};
	__property Font ;
	__property FullSize  = {default=0};
	__property HideWhenInactive  = {default=1};
	__property Images ;
	__property Items ;
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
	__property Stretch  = {default=0};
	__property SystemFont  = {default=1};
	__property TabOrder  = {default=-1};
	__property UpdateActions  = {default=1};
	__property UseLastDock  = {default=1};
	__property Visible  = {default=1};
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
	/* TTBCustomToolbar.Create */ inline __fastcall virtual TTBToolbar(Classes::TComponent* AOwner) : TTBCustomToolbar(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBCustomToolbar.Destroy */ inline __fastcall virtual ~TTBToolbar(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTBToolbar(HWND ParentWindow) : TTBCustomToolbar(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTBChevronItemViewer;
class PASCALIMPLEMENTATION TTBChevronItemViewer : public Tb2item::TTBItemViewer 
{
	typedef Tb2item::TTBItemViewer inherited;
	
protected:
	virtual void __fastcall Paint(const Graphics::TCanvas* Canvas, const Types::TRect &ClientAreaRect, bool IsSelected, bool IsPushed, bool UseDisabledShadow);
public:
	#pragma option push -w-inl
	/* TTBItemViewer.Create */ inline __fastcall virtual TTBChevronItemViewer(Tb2item::TTBView* AView, Tb2item::TTBCustomItem* AItem, int AGroupLevel) : Tb2item::TTBItemViewer(AView, AItem, AGroupLevel) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBItemViewer.Destroy */ inline __fastcall virtual ~TTBChevronItemViewer(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
static const Shortint tbChevronSize = 0xc;

}	/* namespace Tb2toolbar */
using namespace Tb2toolbar;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TB2Toolbar
