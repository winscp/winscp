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
#include <NortonLikeListView.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <BaseUtils.hpp>	// Pascal unit
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
#pragma option push -b-
enum TDateTimeDisplay { dtdDateTimeSec, dtdDateTime, dtdDate };
#pragma option pop

class DELPHICLASS TIEListView;
class PASCALIMPLEMENTATION TIEListView : public Nortonlikelistview::TCustomNortonLikeListView 
{
	typedef Nortonlikelistview::TCustomNortonLikeListView inherited;
	
private:
	int FSortColumn;
	bool FSortAscending;
	bool FColumnIconPainted;
	bool FShowColumnIcon;
	HWND FHeaderHandle;
	Forms::TCustomForm* FParentForm;
	AnsiString FMask;
	Classes::TNotifyEvent FOnHeaderEndDrag;
	Classes::TNotifyEvent FOnHeaderEndTrack;
	AnsiString FDateTimeFormatStr;
	AnsiString FDateFormatStr;
	TDateTimeDisplay FDateTimeDisplay;
	Controls::TDragImageList* FDragImageList;
	Controls::TImageList* FHeaderImages;
	
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
	virtual void __fastcall SetMask(AnsiString Value);
	virtual void __fastcall SetHeaderImages(Controls::TImageList* Value);
	virtual void __fastcall CreateWnd(void);
	DYNAMIC void __fastcall ColClick(Comctrls::TListColumn* Column);
	virtual void __fastcall Loaded(void);
	HIDESBASE MESSAGE void __fastcall WMPaint(Messages::TWMPaint &Msg);
	HIDESBASE MESSAGE void __fastcall WMNotify(Messages::TWMNotify &Msg);
	
public:
	__fastcall virtual TIEListView(Classes::TComponent* AOwner);
	__fastcall virtual ~TIEListView(void);
	virtual void __fastcall SetColumnImages(void);
	DYNAMIC AnsiString __fastcall NormalizeMask(AnsiString Mask);
	__property Controls::TImageList* HeaderImages = {read=FHeaderImages, write=SetHeaderImages};
	__property Controls::TDragImageList* DragImageList = {read=FDragImageList};
	__property bool ColumnIconPainted = {read=FColumnIconPainted, write=FColumnIconPainted, stored=false, nodefault};
	__property HWND HeaderHandle = {read=FHeaderHandle, nodefault};
	__property Forms::TCustomForm* ParentForm = {read=FParentForm};
	__property AnsiString DateTimeFormatStr = {read=FDateTimeFormatStr, write=FDateTimeFormatStr, stored=false};
	__property AnsiString DateFormatStr = {read=FDateFormatStr};
	__property AnsiString Mask = {read=FMask, write=SetMask};
	__property int SortColumn = {read=FSortColumn, write=SetSortColumn, nodefault};
	__property bool ShowColumnIcon = {read=FShowColumnIcon, write=SetShowColumnIcon, default=1};
	__property bool SortAscending = {read=FSortAscending, write=SetSortAscending, default=1};
	
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
	__property NortonLike  = {default=1};
	__property OnSelectByMask ;
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TIEListView(HWND ParentWindow) : Nortonlikelistview::TCustomNortonLikeListView(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE Controls::TDragImageList* GlobalDragImageList;

}	/* namespace Ielistview */
using namespace Ielistview;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// IEListView
