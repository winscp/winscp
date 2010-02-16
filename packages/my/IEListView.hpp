// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'IEListView.pas' rev: 6.00

#ifndef IEListViewHPP
#define IEListViewHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Menus.hpp>	// Pascal unit
#include <ListViewColProperties.hpp>	// Pascal unit
#include <NortonLikeListView.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <CommCtrl.hpp>	// Pascal unit
#include <ActiveX.hpp>	// Pascal unit
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

namespace Ielistview
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TIEListViewColProperties;
class PASCALIMPLEMENTATION TIEListViewColProperties : public Listviewcolproperties::TCustomListViewColProperties 
{
	typedef Listviewcolproperties::TCustomListViewColProperties inherited;
	
protected:
	bool __fastcall GetSortAscending(void);
	void __fastcall SetSortColumn(int Value);
	int __fastcall GetSortColumn(void);
	virtual AnsiString __fastcall GetSortStr();
	void __fastcall SetSortAscending(bool Value);
	virtual void __fastcall SetSortStr(AnsiString Value);
	virtual AnsiString __fastcall GetParamsStr();
	virtual void __fastcall SetParamsStr(AnsiString Value);
	
public:
	__fastcall TIEListViewColProperties(Comctrls::TCustomListView* ListView, int ColCount);
	__property bool SortAscending = {read=GetSortAscending, write=SetSortAscending, default=1};
	__property int SortColumn = {read=GetSortColumn, write=SetSortColumn, nodefault};
	__property AnsiString SortStr = {read=GetSortStr, write=SetSortStr, stored=false};
public:
	#pragma option push -w-inl
	/* TPersistent.Destroy */ inline __fastcall virtual ~TIEListViewColProperties(void) { }
	#pragma option pop
	
};


#pragma option push -b-
enum TDateTimeDisplay { dtdDateTimeSec, dtdDateTime, dtdDate };
#pragma option pop

class DELPHICLASS TCustomIEListView;
typedef void __fastcall (__closure *TListViewSecondaryColumnHeaderEvent)(TCustomIEListView* Sender, int Index, int &SecondaryColumn);

class PASCALIMPLEMENTATION TCustomIEListView : public Nortonlikelistview::TCustomNortonLikeListView 
{
	typedef Nortonlikelistview::TCustomNortonLikeListView inherited;
	
private:
	int FSortColumn;
	bool FSortAscending;
	bool FColumnIconPainted;
	bool FShowColumnIcon;
	HWND FHeaderHandle;
	Forms::TCustomForm* FParentForm;
	Graphics::TCanvas* FHeaderCanvas;
	Classes::TNotifyEvent FOnHeaderEndDrag;
	Classes::TNotifyEvent FOnHeaderEndTrack;
	TListViewSecondaryColumnHeaderEvent FOnSecondaryColumnHeader;
	AnsiString FDateTimeFormatStr;
	AnsiString FDateFormatStr;
	TDateTimeDisplay FDateTimeDisplay;
	Controls::TDragImageList* FDragImageList;
	Controls::TImageList* FHeaderImages;
	int __fastcall SecondaryColumnHeaderOffset(Graphics::TCanvas* Canvas, int Index);
	int __fastcall ColumnHeaderIconWidth(void);
	
protected:
	virtual void __fastcall ColPropertiesChange(System::TObject* Sender);
	virtual void __fastcall SetShowColumnIcon(bool Value);
	virtual void __fastcall SetSortColumn(int Value);
	virtual void __fastcall SetSortAscending(bool Value);
	virtual void __fastcall SortItems(void);
	virtual void __fastcall SetViewStyle(Comctrls::TViewStyle Value);
	virtual void __fastcall SetDateTimeDisplay(TDateTimeDisplay Value);
	virtual void __fastcall SetDateTimeFormatString(void);
	virtual void __fastcall HeaderEndDrag(System::TObject* Sender);
	virtual void __fastcall SetHeaderImages(Controls::TImageList* Value);
	virtual int __fastcall SecondaryColumnHeader(int Index, bool &AliasOnly);
	virtual Listviewcolproperties::TCustomListViewColProperties* __fastcall NewColProperties(void);
	virtual bool __fastcall SortAscendingByDefault(int Index);
	virtual void __fastcall CreateWnd(void);
	DYNAMIC void __fastcall ColClick(Comctrls::TListColumn* Column);
	virtual void __fastcall Loaded(void);
	HIDESBASE MESSAGE void __fastcall WMPaint(Messages::TWMPaint &Msg);
	HIDESBASE MESSAGE void __fastcall WMNotify(Messages::TWMNotify &Msg);
	
public:
	__fastcall virtual TCustomIEListView(Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomIEListView(void);
	virtual void __fastcall SetColumnImages(void);
	__property Controls::TImageList* HeaderImages = {read=FHeaderImages, write=SetHeaderImages};
	__property Controls::TDragImageList* DragImageList = {read=FDragImageList};
	__property HWND HeaderHandle = {read=FHeaderHandle, nodefault};
	__property Forms::TCustomForm* ParentForm = {read=FParentForm};
	__property AnsiString DateTimeFormatStr = {read=FDateTimeFormatStr, write=FDateTimeFormatStr, stored=false};
	__property AnsiString DateFormatStr = {read=FDateFormatStr};
	__property int SortColumn = {read=FSortColumn, write=SetSortColumn, nodefault};
	__property bool ShowColumnIcon = {read=FShowColumnIcon, write=SetShowColumnIcon, default=1};
	__property bool SortAscending = {read=FSortAscending, write=SetSortAscending, default=1};
	__property TListViewSecondaryColumnHeaderEvent OnSecondaryColumnHeader = {read=FOnSecondaryColumnHeader, write=FOnSecondaryColumnHeader};
	
__published:
	__property TDateTimeDisplay DateTimeDisplay = {read=FDateTimeDisplay, write=SetDateTimeDisplay, default=0};
	__property Classes::TNotifyEvent OnHeaderEndDrag = {read=FOnHeaderEndDrag, write=FOnHeaderEndDrag};
	__property Classes::TNotifyEvent OnHeaderEndTrack = {read=FOnHeaderEndTrack, write=FOnHeaderEndTrack};
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
	__property ReadOnly  = {default=0};
	__property RowSelect  = {default=0};
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=0};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ShowColumnHeaders  = {default=1};
	__property ShowHint ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property ViewStyle  = {default=0};
	__property Visible  = {default=1};
	__property OnChange ;
	__property OnChanging ;
	__property OnClick ;
	__property OnColumnClick ;
	__property OnColumnRightClick ;
	__property OnCustomDraw ;
	__property OwnerDraw  = {default=0};
	__property OnCustomDrawItem ;
	__property OnCustomDrawSubItem ;
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
	__property NortonLike  = {default=0};
	__property OnSelectByMask ;
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCustomIEListView(HWND ParentWindow) : Nortonlikelistview::TCustomNortonLikeListView(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TIEListView;
class PASCALIMPLEMENTATION TIEListView : public TCustomIEListView 
{
	typedef TCustomIEListView inherited;
	
__published:
	__property Action ;
	__property Align  = {default=0};
	__property AllocBy  = {default=0};
	__property Anchors  = {default=3};
	__property BevelEdges  = {default=15};
	__property BevelInner  = {index=0, default=2};
	__property BevelOuter  = {index=1, default=1};
	__property BevelKind  = {default=0};
	__property BevelWidth  = {default=1};
	__property BiDiMode ;
	__property BorderStyle  = {default=1};
	__property BorderWidth  = {default=0};
	__property Checkboxes  = {default=0};
	__property Color  = {default=-2147483643};
	__property Columns ;
	__property ColumnClick  = {default=1};
	__property Constraints ;
	__property Ctl3D ;
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property Font ;
	__property FlatScrollBars  = {default=0};
	__property FullDrag  = {default=0};
	__property GridLines  = {default=0};
	__property HeaderImages ;
	__property HideSelection  = {default=1};
	__property HotTrack  = {default=0};
	__property HotTrackStyles  = {default=0};
	__property HoverTime  = {default=-1};
	__property IconOptions ;
	__property Items ;
	__property LargeImages ;
	__property MultiSelect  = {default=1};
	__property OwnerData  = {default=0};
	__property OwnerDraw  = {default=0};
	__property ReadOnly  = {default=0};
	__property RowSelect  = {default=0};
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=0};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ShowColumnHeaders  = {default=1};
	__property ShowWorkAreas  = {default=0};
	__property ShowHint ;
	__property SmallImages ;
	__property SortType  = {default=0};
	__property StateImages ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property ViewStyle  = {default=0};
	__property Visible  = {default=1};
	__property OnAdvancedCustomDraw ;
	__property OnAdvancedCustomDrawItem ;
	__property OnAdvancedCustomDrawSubItem ;
	__property OnChange ;
	__property OnChanging ;
	__property OnClick ;
	__property OnColumnClick ;
	__property OnColumnDragged ;
	__property OnColumnRightClick ;
	__property OnCompare ;
	__property OnContextPopup ;
	__property OnCustomDraw ;
	__property OnCustomDrawItem ;
	__property OnCustomDrawSubItem ;
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
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnGetImageIndex ;
	__property OnGetSubItemImage ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnInfoTip ;
	__property OnInsert ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMouseDown ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnResize ;
	__property OnSelectItem ;
	__property OnStartDock ;
	__property OnStartDrag ;
	__property OnSecondaryColumnHeader ;
public:
	#pragma option push -w-inl
	/* TCustomIEListView.Create */ inline __fastcall virtual TIEListView(Classes::TComponent* AOwner) : TCustomIEListView(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomIEListView.Destroy */ inline __fastcall virtual ~TIEListView(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TIEListView(HWND ParentWindow) : TCustomIEListView(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE Controls::TDragImageList* GlobalDragImageList;
extern PACKAGE void __fastcall Register(void);

}	/* namespace Ielistview */
using namespace Ielistview;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// IEListView
