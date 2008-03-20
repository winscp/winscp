// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TBXLists.pas' rev: 6.00

#ifndef TBXListsHPP
#define TBXListsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ImgList.hpp>	// Pascal unit
#include <TBXUxThemes.hpp>	// Pascal unit
#include <TBXThemes.hpp>	// Pascal unit
#include <TBX.hpp>	// Pascal unit
#include <TB2Item.hpp>	// Pascal unit
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

namespace Tbxlists
{
//-- type declarations -------------------------------------------------------
typedef Word TSBIncrement;

#pragma option push -b-
enum TSBZone { sbzEmpty, sbzPrev, sbzPagePrev, sbzHandle, sbzPageNext, sbzNext };
#pragma option pop

typedef void __fastcall (__closure *TSBAutoScrollEvent)(System::TObject* Sender, int &Direction, int &Interval);

class DELPHICLASS TTBXScrollBar;
class PASCALIMPLEMENTATION TTBXScrollBar : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	#pragma pack(push, 1)
	Types::TRect FBounds;
	#pragma pack(pop)
	
	int FLeft;
	HWND FHandle;
	int FHeight;
	TSBIncrement FIncrement;
	Forms::TScrollBarKind FKind;
	int FPosition;
	int FRange;
	int FRight;
	int FTop;
	int FWidth;
	int FWindow;
	Classes::TNotifyEvent FOnChange;
	TSBAutoScrollEvent FOnAutoScroll;
	Classes::TNotifyEvent FOnRedrawRequest;
	void __fastcall SetBounds(const Types::TRect &Value);
	void __fastcall SetKind(Forms::TScrollBarKind Value);
	void __fastcall SetPosition(int Value);
	void __fastcall SetRange(int Value);
	HWND __fastcall GetHandle(void);
	
protected:
	int AutoScrollDirection;
	bool AutoScrolling;
	int AutoScrollInterval;
	Types::TRect Zones[6];
	TSBZone MouseDownZone;
	#pragma pack(push, 1)
	Types::TPoint MouseDownPoint;
	#pragma pack(pop)
	
	int MouseDownPosition;
	#pragma pack(push, 1)
	Types::TPoint LastMousePoint;
	#pragma pack(pop)
	
	HWND PrevCapture;
	bool UserChange;
	void __fastcall AdjustPosition(int &NewPosition);
	void __fastcall CreateWnd(void);
	void __fastcall DestroyWnd(void);
	TSBZone __fastcall GetZone(int X, int Y);
	int __fastcall GetEffectiveWindow(void);
	virtual bool __fastcall GetEnabled(void);
	void __fastcall HandleZoneClick(TSBZone AZone);
	virtual void __fastcall MouseDown(Controls::TMouseButton Button, int X, int Y);
	virtual void __fastcall MouseMove(int X, int Y);
	virtual void __fastcall MouseUp(Controls::TMouseButton Button, int X, int Y);
	void __fastcall PaintButton(Graphics::TCanvas* Canvas, const Types::TRect &Rect, int Direction, bool Pushed, bool Enabled);
	void __fastcall PaintHandle(Graphics::TCanvas* Canvas, const Types::TRect &Rect, bool Pushed, bool Enabled);
	void __fastcall PaintTrack(Graphics::TCanvas* Canvas, const Types::TRect &Rect, bool IsNextZone, bool Pushed, bool Enabled);
	void __fastcall PaintTo(Graphics::TCanvas* Canvas);
	void __fastcall SBWndProc(Messages::TMessage &Message);
	void __fastcall StartAutoScroll(int Direction, int Interval);
	void __fastcall StopAutoScroll(void);
	void __fastcall StartTimer(int ID, int Elapse);
	void __fastcall StopTimer(int ID);
	virtual void __fastcall TimerElapsed(int ID, int &NewElapse);
	void __fastcall UpdateZones(void);
	__property HWND Handle = {read=GetHandle, nodefault};
	
public:
	__fastcall TTBXScrollBar(void);
	__fastcall virtual ~TTBXScrollBar(void);
	virtual void __fastcall Redraw(void);
	void __fastcall UpdatePosition(int NewPosition);
	__property Forms::TScrollBarKind Kind = {read=FKind, write=SetKind, nodefault};
	__property Types::TRect Bounds = {read=FBounds, write=SetBounds};
	__property int Left = {read=FLeft, nodefault};
	__property int Height = {read=FHeight, nodefault};
	__property TSBIncrement Increment = {read=FIncrement, write=FIncrement, nodefault};
	__property int Position = {read=FPosition, write=SetPosition, nodefault};
	__property int Range = {read=FRange, write=SetRange, nodefault};
	__property int Right = {read=FRight, nodefault};
	__property int Top = {read=FTop, nodefault};
	__property int Width = {read=FWidth, nodefault};
	__property int Window = {read=FWindow, write=FWindow, nodefault};
	__property TSBAutoScrollEvent OnAutoScroll = {read=FOnAutoScroll, write=FOnAutoScroll};
	__property Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property Classes::TNotifyEvent OnRedrawRequest = {read=FOnRedrawRequest, write=FOnRedrawRequest};
};


class DELPHICLASS TTBXCustomList;
typedef void __fastcall (__closure *TTBXLMeasureHeight)(TTBXCustomList* Sender, Graphics::TCanvas* ACanvas, int &AHeight);

typedef void __fastcall (__closure *TTBXLMeasureWidth)(TTBXCustomList* Sender, Graphics::TCanvas* ACanvas, int AIndex, int &AWidth);

typedef void __fastcall (__closure *TTBXLPaintEvent)(TTBXCustomList* Sender, Graphics::TCanvas* ACanvas, const Types::TRect &ARect, int AIndex, int AHoverIndex, bool &DrawDefault);

typedef void __fastcall (__closure *TTBXLAdjustImageIndex)(TTBXCustomList* Sender, int AItemIndex, int &ImageIndex);

class DELPHICLASS TTBXCustomListViewer;
class PASCALIMPLEMENTATION TTBXCustomList : public Tbx::TTBXCustomItem 
{
	typedef Tbx::TTBXCustomItem inherited;
	
private:
	Classes::TList* FViewers;
	int FItemIndex;
	int FMinWidth;
	int FMaxWidth;
	int FMaxVisibleItems;
	bool FShowImages;
	Classes::TNotifyEvent FOnChange;
	TTBXLPaintEvent FOnClearItem;
	TTBXLPaintEvent FOnDrawItem;
	TTBXLAdjustImageIndex FOnAdjustImageIndex;
	TTBXLMeasureHeight FOnMeasureHeight;
	TTBXLMeasureWidth FOnMeasureWidth;
	void __fastcall SetItemIndex(int Value);
	
protected:
	virtual bool __fastcall DoClearItem(Graphics::TCanvas* ACanvas, const Types::TRect &ARect, int AIndex, int AHoverIndex);
	virtual bool __fastcall DoDrawItem(Graphics::TCanvas* ACanvas, Types::TRect &ARect, int AIndex, int AHoverIndex);
	virtual void __fastcall DoMeasureHeight(Graphics::TCanvas* ACanvas, int &AHeight);
	virtual void __fastcall DoMeasureWidth(Graphics::TCanvas* ACanvas, int AIndex, int &AWidth);
	virtual void __fastcall DrawItem(Graphics::TCanvas* ACanvas, TTBXCustomListViewer* AViewer, const Types::TRect &ARect, int AIndex, int AHoverIndex);
	virtual int __fastcall GetImageIndex(int ItemIndex);
	virtual TMetaClass* __fastcall GetItemViewerClass(Tb2item::TTBView* AView);
	virtual AnsiString __fastcall GetItemText(int Index) = 0 ;
	virtual int __fastcall GetCount(void) = 0 ;
	virtual void __fastcall HandleChange(void);
	virtual void __fastcall HandleHover(int AIndex);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	
public:
	__fastcall virtual TTBXCustomList(Classes::TComponent* AOwner);
	void __fastcall MakeVisible(int AIndex);
	__property int ItemIndex = {read=FItemIndex, write=SetItemIndex, default=-1};
	__property int MaxVisibleItems = {read=FMaxVisibleItems, write=FMaxVisibleItems, default=8};
	__property int MaxWidth = {read=FMaxWidth, write=FMaxWidth, default=0};
	__property int MinWidth = {read=FMinWidth, write=FMinWidth, default=32};
	__property bool ShowImages = {read=FShowImages, write=FShowImages, default=0};
	__property TTBXLAdjustImageIndex OnAdjustImageIndex = {read=FOnAdjustImageIndex, write=FOnAdjustImageIndex};
	__property Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property TTBXLPaintEvent OnClearItem = {read=FOnClearItem, write=FOnClearItem};
	__property TTBXLPaintEvent OnDrawItem = {read=FOnDrawItem, write=FOnDrawItem};
	__property TTBXLMeasureHeight OnMeasureHeight = {read=FOnMeasureHeight, write=FOnMeasureHeight};
	__property TTBXLMeasureWidth OnMeasureWidth = {read=FOnMeasureWidth, write=FOnMeasureWidth};
public:
	#pragma option push -w-inl
	/* TTBXCustomItem.Destroy */ inline __fastcall virtual ~TTBXCustomList(void) { }
	#pragma option pop
	
};


class PASCALIMPLEMENTATION TTBXCustomListViewer : public Tbx::TTBXItemViewer 
{
	typedef Tbx::TTBXItemViewer inherited;
	
private:
	int FItemCount;
	int FItemHeight;
	int FHoverIndex;
	int FHeight;
	#pragma pack(push, 1)
	Types::TRect FLastClientRect;
	#pragma pack(pop)
	
	int FWheelAccumulator;
	int FWidth;
	int FOffset;
	int FScrollBarWidth;
	TTBXScrollBar* FScrollBar;
	int FVisibleItems;
	void __fastcall ListChangeHandler(int NewIndex);
	void __fastcall SBAutoScrollHandler(System::TObject* Sender, int &Direction, int &Interval);
	void __fastcall SBChangeHandler(System::TObject* Sender);
	void __fastcall SBRedrawHandler(System::TObject* Sender);
	
protected:
	bool MouseIsDown;
	bool MouseInScrollBar;
	bool IgnoreMouseUp;
	bool IsChanging;
	virtual void __fastcall AdjustAutoScrollHover(int &AIndex, int Direction);
	virtual void __fastcall CalcSize(const Graphics::TCanvas* Canvas, int &AWidth, int &AHeight);
	void __fastcall DrawItems(const Graphics::TCanvas* Canvas, const Types::TRect &ClientAreaRect);
	int __fastcall GetItemIndexAt(int X, int Y);
	Types::TRect __fastcall GetItemRect(int Index);
	virtual int __fastcall GetItemHeight(Graphics::TCanvas* ACanvas);
	virtual int __fastcall GetItemWidth(Graphics::TCanvas* ACanvas, int Index);
	virtual void __fastcall HandleAutoScroll(int &Direction, int &Interval);
	virtual void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	void __fastcall MakeVisible(int Index);
	virtual void __fastcall MouseDown(Classes::TShiftState Shift, int X, int Y, bool &MouseDownOnMenu);
	virtual void __fastcall MouseMove(int X, int Y);
	virtual void __fastcall MouseUp(int X, int Y, bool MouseWasDownOnMenu);
	virtual void __fastcall MouseWheel(int WheelDelta, int X, int Y);
	virtual void __fastcall Paint(const Graphics::TCanvas* Canvas, const Types::TRect &ClientAreaRect, bool IsHoverItem, bool IsPushed, bool UseDisabledShadow);
	void __fastcall UpdateItems(void);
	__property int HoverIndex = {read=FHoverIndex, write=FHoverIndex, nodefault};
	__property int Offset = {read=FOffset, nodefault};
	__property int VisibleItems = {read=FVisibleItems, nodefault};
	
public:
	__fastcall virtual TTBXCustomListViewer(Tb2item::TTBView* AView, Tb2item::TTBCustomItem* AItem, int AGroupLevel);
	__fastcall virtual ~TTBXCustomListViewer(void);
};


class DELPHICLASS TTBXStringList;
class PASCALIMPLEMENTATION TTBXStringList : public TTBXCustomList 
{
	typedef TTBXCustomList inherited;
	
private:
	Classes::TStrings* FStrings;
	void __fastcall SetStrings(Classes::TStrings* Value);
	
protected:
	virtual AnsiString __fastcall GetItemText(int Index);
	virtual int __fastcall GetCount(void);
	
public:
	__fastcall virtual TTBXStringList(Classes::TComponent* AOwner);
	__fastcall virtual ~TTBXStringList(void);
	
__published:
	__property ItemIndex  = {default=-1};
	__property MaxVisibleItems  = {default=8};
	__property MaxWidth  = {default=0};
	__property MinWidth  = {default=32};
	__property Classes::TStrings* Strings = {read=FStrings, write=SetStrings};
	__property OnAdjustImageIndex ;
	__property OnChange ;
	__property OnClearItem ;
	__property OnClick ;
	__property OnDrawItem ;
	__property OnMeasureHeight ;
	__property OnMeasureWidth ;
};


typedef TMetaClass*TTBXStringListClass;

//-- var, const, procedure ---------------------------------------------------

}	/* namespace Tbxlists */
using namespace Tbxlists;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TBXLists
