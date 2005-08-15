// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NortonLikeListView.pas' rev: 6.00

#ifndef NortonLikeListViewHPP
#define NortonLikeListViewHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Menus.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <CommCtrl.hpp>	// Pascal unit
#include <ListViewColProperties.hpp>	// Pascal unit
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

namespace Nortonlikelistview
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TSelectMode { smAll, smNone, smInvert };
#pragma option pop

#pragma option push -b-
enum TNortonLikeMode { nlOn, nlOff, nlKeyboard };
#pragma option pop

class DELPHICLASS TCustomNortonLikeListView;
typedef void __fastcall (__closure *TSelectByMaskEvent)(TCustomNortonLikeListView* Control, bool Select);

class PASCALIMPLEMENTATION TCustomNortonLikeListView : public Comctrls::TCustomListView 
{
	typedef Comctrls::TCustomListView inherited;
	
private:
	Listviewcolproperties::TCustomListViewColProperties* FColProperties;
	bool FDontSelectItem;
	bool FDontUnSelectItem;
	int FSelCount;
	TNortonLikeMode FNortonLike;
	TSelectByMaskEvent FOnSelectByMask;
	Comctrls::TListItem* FLastDeletedItem;
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMRButtonDown(Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall WMChar(Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall WMNotify(Messages::TWMNotify &Message);
	HIDESBASE MESSAGE void __fastcall CNNotify(Messages::TWMNotify &Message);
	int __fastcall GetMarkedCount(void);
	Comctrls::TListItem* __fastcall GetMarkedFile(void);
	
protected:
	bool FClearingItems;
	int FUpdatingSelection;
	virtual void __fastcall CreateWnd(void);
	virtual void __fastcall BeginSelectionUpdate(void);
	virtual void __fastcall EndSelectionUpdate(void);
	virtual bool __fastcall CanChangeSelection(Comctrls::TListItem* Item, bool Select);
	virtual void __fastcall ClearItems(void);
	DYNAMIC void __fastcall ColRightClick(Comctrls::TListColumn* Column, const Types::TPoint &Point);
	DYNAMIC void __fastcall Delete(Comctrls::TListItem* Item);
	virtual bool __fastcall DoSelectByMask(bool Select);
	DYNAMIC bool __fastcall ExCanChange(Comctrls::TListItem* Item, int Change, Word NewState, Word OldState);
	DYNAMIC void __fastcall InsertItem(Comctrls::TListItem* Item);
	virtual Listviewcolproperties::TCustomListViewColProperties* __fastcall NewColProperties(void);
	virtual void __fastcall FocusSomething(void);
	void __fastcall FocusItem(Comctrls::TListItem* Item);
	Comctrls::TListItem* __fastcall GetItemFromHItem(const tagLVITEMA &Item);
	virtual bool __fastcall GetValid(void);
	virtual int __fastcall GetSelCount(void);
	void __fastcall DDBeforeDrag(void);
	
public:
	__fastcall virtual TCustomNortonLikeListView(Classes::TComponent* AOwner);
	Comctrls::TListItem* __fastcall ClosestUnselected(Comctrls::TListItem* Item);
	HIDESBASE void __fastcall SelectAll(TSelectMode Mode);
	void __fastcall SelectCurrentItem(bool FocusNext);
	__property Listviewcolproperties::TCustomListViewColProperties* ColProperties = {read=FColProperties, write=FColProperties, stored=false};
	__property MultiSelect  = {default=1};
	__property TNortonLikeMode NortonLike = {read=FNortonLike, write=FNortonLike, default=0};
	__property TSelectByMaskEvent OnSelectByMask = {read=FOnSelectByMask, write=FOnSelectByMask};
	__property int MarkedCount = {read=GetMarkedCount, nodefault};
	__property Comctrls::TListItem* MarkedFile = {read=GetMarkedFile};
	__property bool Valid = {read=GetValid, nodefault};
public:
	#pragma option push -w-inl
	/* TCustomListView.Destroy */ inline __fastcall virtual ~TCustomNortonLikeListView(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCustomNortonLikeListView(HWND ParentWindow) : Comctrls::TCustomListView(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TNortonLikeListView;
class PASCALIMPLEMENTATION TNortonLikeListView : public TCustomNortonLikeListView 
{
	typedef TCustomNortonLikeListView inherited;
	
__published:
	__property Align  = {default=0};
	__property AllocBy  = {default=0};
	__property Anchors  = {default=3};
	__property BiDiMode ;
	__property BorderStyle  = {default=1};
	__property BorderWidth  = {default=0};
	__property Checkboxes  = {default=0};
	__property Color  = {default=-2147483643};
	__property ColumnClick  = {default=1};
	__property Constraints ;
	__property Ctl3D ;
	__property Enabled  = {default=1};
	__property Font ;
	__property FlatScrollBars  = {default=0};
	__property FullDrag  = {default=0};
	__property GridLines  = {default=0};
	__property HideSelection  = {default=1};
	__property HotTrack  = {default=0};
	__property HotTrackStyles  = {default=0};
	__property IconOptions ;
	__property Items ;
	__property LargeImages ;
	__property ReadOnly ;
	__property RowSelect  = {default=0};
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ShowColumnHeaders  = {default=1};
	__property ShowHint ;
	__property SmallImages ;
	__property StateImages ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=0};
	__property ViewStyle  = {default=0};
	__property Visible  = {default=1};
	__property OnChange ;
	__property OnChanging ;
	__property OnClick ;
	__property OnColumnClick ;
	__property OnCustomDraw ;
	__property OwnerDraw  = {default=0};
	__property OnCustomDrawItem ;
	__property OnCustomDrawSubItem ;
	__property OwnerData  = {default=0};
	__property OnGetImageIndex ;
	__property OnCompare ;
	__property OnData ;
	__property OnDataFind ;
	__property OnDataHint ;
	__property OnDataStateChange ;
	__property OnDblClick ;
	__property OnDeletion ;
	__property OnDrawItem ;
	__property OnEdited ;
	__property OnEditing ;
	__property OnEndDock ;
	__property OnEnter ;
	__property OnExit ;
	__property OnInsert ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMouseDown ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnResize ;
	__property OnStartDock ;
	__property OnSelectItem ;
	__property NortonLike  = {default=0};
	__property OnSelectByMask ;
	__property ColProperties ;
public:
	#pragma option push -w-inl
	/* TCustomNortonLikeListView.Create */ inline __fastcall virtual TNortonLikeListView(Classes::TComponent* AOwner) : TCustomNortonLikeListView(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomListView.Destroy */ inline __fastcall virtual ~TNortonLikeListView(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TNortonLikeListView(HWND ParentWindow) : TCustomNortonLikeListView(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Nortonlikelistview */
using namespace Nortonlikelistview;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NortonLikeListView
