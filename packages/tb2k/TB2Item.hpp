// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TB2Item.pas' rev: 6.00

#ifndef TB2ItemHPP
#define TB2ItemHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <TB2Anim.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <ActnList.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <CommCtrl.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
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

namespace Tb2item
{
//-- type declarations -------------------------------------------------------
typedef TMetaClass*TTBCustomItemClass;

typedef TMetaClass*TTBCustomItemActionLinkClass;

typedef TMetaClass*TTBItemViewerClass;

typedef TMetaClass*TTBPopupWindowClass;

#pragma option push -b-
enum TTBDoneAction { tbdaNone, tbdaCancel, tbdaClickItem, tbdaOpenSystemMenu, tbdaHelpContext, tbdaHelpKeyword };
#pragma option pop

struct TTBDoneActionData;
typedef TTBDoneActionData *PTBDoneActionData;

class DELPHICLASS TTBCustomItem;
class DELPHICLASS TTBCustomItemActionLink;
class PASCALIMPLEMENTATION TTBCustomItemActionLink : public Actnlist::TActionLink 
{
	typedef Actnlist::TActionLink inherited;
	
protected:
	TTBCustomItem* FClient;
	virtual void __fastcall AssignClient(System::TObject* AClient);
	virtual bool __fastcall IsAutoCheckLinked(void);
	virtual bool __fastcall IsCaptionLinked(void);
	virtual bool __fastcall IsCheckedLinked(void);
	virtual bool __fastcall IsEnabledLinked(void);
	virtual bool __fastcall IsHelpContextLinked(void);
	virtual bool __fastcall IsHelpLinked(void);
	virtual bool __fastcall IsHintLinked(void);
	virtual bool __fastcall IsImageIndexLinked(void);
	virtual bool __fastcall IsShortCutLinked(void);
	virtual bool __fastcall IsVisibleLinked(void);
	virtual bool __fastcall IsOnExecuteLinked(void);
	virtual void __fastcall SetAutoCheck(bool Value);
	virtual void __fastcall SetCaption(const AnsiString Value);
	virtual void __fastcall SetChecked(bool Value);
	virtual void __fastcall SetEnabled(bool Value);
	virtual void __fastcall SetHelpContext(Classes::THelpContext Value);
	virtual void __fastcall SetHelpKeyword(const AnsiString Value);
	virtual void __fastcall SetHint(const AnsiString Value);
	virtual void __fastcall SetImageIndex(int Value);
	virtual void __fastcall SetShortCut(Classes::TShortCut Value);
	virtual void __fastcall SetVisible(bool Value);
	virtual void __fastcall SetOnExecute(Classes::TNotifyEvent Value);
public:
	#pragma option push -w-inl
	/* TBasicActionLink.Create */ inline __fastcall virtual TTBCustomItemActionLink(System::TObject* AClient) : Actnlist::TActionLink(AClient) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TBasicActionLink.Destroy */ inline __fastcall virtual ~TTBCustomItemActionLink(void) { }
	#pragma option pop
	
};


#pragma option push -b-
enum TTBItemDisplayMode { nbdmDefault, nbdmTextOnly, nbdmTextOnlyInMenus, nbdmImageAndText };
#pragma option pop

#pragma option push -b-
enum TTBItemOption { tboDefault, tboDropdownArrow, tboImageAboveCaption, tboLongHintInMenuOnly, tboNoAutoHint, tboNoRotation, tboSameWidth, tboShowHint, tboToolbarStyle, tboToolbarSize };
#pragma option pop

typedef Set<TTBItemOption, tboDefault, tboToolbarSize>  TTBItemOptions;

class DELPHICLASS TTBImageChangeLink;
class PASCALIMPLEMENTATION TTBImageChangeLink : public Imglist::TChangeLink 
{
	typedef Imglist::TChangeLink inherited;
	
private:
	int FLastWidth;
	int FLastHeight;
public:
	#pragma option push -w-inl
	/* TChangeLink.Destroy */ inline __fastcall virtual ~TTBImageChangeLink(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TTBImageChangeLink(void) : Imglist::TChangeLink() { }
	#pragma option pop
	
};


#pragma pack(push, 4)
struct TTBItemData
{
	TTBCustomItem* Item;
} ;
#pragma pack(pop)

typedef TTBItemData TTBItemDataArray[536870911];

typedef TTBItemData *PTBItemDataArray;

#pragma option push -b-
enum TB2Item__1 { tbisSubmenu, tbisSelectable, tbisSeparator, tbisEmbeddedGroup, tbisClicksTransparent, tbisCombo, tbisNoAutoOpen, tbisSubitemsEditable, tbisNoLineBreak, tbisRightAlign, tbisDontSelectFirst, tbisRedrawOnSelChange, tbisRedrawOnMouseOverChange, tbisStretch };
#pragma option pop

typedef Set<TB2Item__1, tbisSubmenu, tbisStretch>  TTBItemStyle;

typedef void __fastcall (__closure *TTBPopupEvent)(TTBCustomItem* Sender, bool FromLink);

class DELPHICLASS TTBItemViewer;
typedef void __fastcall (__closure *TTBSelectEvent)(TTBCustomItem* Sender, TTBItemViewer* Viewer, bool Selecting);

#pragma option push -b-
enum TTBItemChangedAction { tbicInserted, tbicDeleting, tbicSubitemsChanged, tbicSubitemsBeginUpdate, tbicSubitemsEndUpdate, tbicInvalidate, tbicInvalidateAndResize, tbicRecreateItemViewers, tbicNameChanged, tbicSubMenuImagesChanged };
#pragma option pop

class DELPHICLASS TTBPopupWindow;
class DELPHICLASS TTBView;
#pragma option push -b-
enum TTBPopupAlignment { tbpaLeft, tbpaRight, tbpaCenter };
#pragma option pop

struct TTBPopupPositionRec;
typedef void __fastcall (__closure *TTBItemChangedProc)(TTBCustomItem* Sender, bool Relayed, TTBItemChangedAction Action, int Index, TTBCustomItem* Item);

class PASCALIMPLEMENTATION TTBCustomItem : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
public:
	TTBCustomItem* operator[](int Index) { return Items[Index]; }
	
private:
	TTBCustomItemActionLink* FActionLink;
	bool FAutoCheck;
	AnsiString FCaption;
	bool FChecked;
	TTBItemDisplayMode FDisplayMode;
	bool FEnabled;
	TTBItemOptions FEffectiveOptions;
	int FGroupIndex;
	Classes::THelpContext FHelpContext;
	AnsiString FHelpKeyword;
	AnsiString FHint;
	Imglist::TImageIndex FImageIndex;
	Imglist::TCustomImageList* FImages;
	TTBImageChangeLink* FImagesChangeLink;
	TTBItemData *FItems;
	int FItemCount;
	TTBItemStyle FItemStyle;
	Classes::TList* FLinkParents;
	TTBItemOptions FMaskOptions;
	TTBItemOptions FOptions;
	bool FInheritOptions;
	Classes::TList* FNotifyList;
	Classes::TNotifyEvent FOnClick;
	TTBPopupEvent FOnPopup;
	TTBSelectEvent FOnSelect;
	TTBCustomItem* FParent;
	Classes::TComponent* FParentComponent;
	bool FRadioItem;
	Classes::TShortCut FShortCut;
	Imglist::TCustomImageList* FSubMenuImages;
	TTBImageChangeLink* FSubMenuImagesChangeLink;
	TTBCustomItem* FLinkSubitems;
	bool FVisible;
	void __fastcall DoActionChange(System::TObject* Sender);
	bool __fastcall ChangeImages(Imglist::TCustomImageList* &AImages, const Imglist::TCustomImageList* Value, TTBImageChangeLink* &AChangeLink);
	/*         class method */ static void __fastcall ClickWndProc(TMetaClass* vmt, Messages::TMessage &Message);
	TTBCustomItem* __fastcall FindItemWithShortCut(Classes::TShortCut AShortCut, TTBCustomItem* &ATopmostParent);
	TTBItemOptions __fastcall FixOptions(const TTBItemOptions AOptions);
	Classes::TBasicAction* __fastcall GetAction(void);
	TTBCustomItem* __fastcall GetItem(int Index);
	void __fastcall ImageListChangeHandler(System::TObject* Sender);
	void __fastcall InternalNotify(TTBCustomItem* Ancestor, int NestingLevel, TTBItemChangedAction Action, int Index, TTBCustomItem* Item);
	bool __fastcall IsAutoCheckStored(void);
	bool __fastcall IsCaptionStored(void);
	bool __fastcall IsCheckedStored(void);
	bool __fastcall IsEnabledStored(void);
	bool __fastcall IsHelpContextStored(void);
	bool __fastcall IsHintStored(void);
	bool __fastcall IsImageIndexStored(void);
	bool __fastcall IsOnClickStored(void);
	bool __fastcall IsShortCutStored(void);
	bool __fastcall IsVisibleStored(void);
	void __fastcall Notify(TTBItemChangedAction Action, int Index, TTBCustomItem* Item);
	void __fastcall RefreshOptions(void);
	void __fastcall SetAction(Classes::TBasicAction* Value);
	void __fastcall SetCaption(AnsiString Value);
	void __fastcall SetChecked(bool Value);
	void __fastcall SetDisplayMode(TTBItemDisplayMode Value);
	void __fastcall SetEnabled(bool Value);
	void __fastcall SetGroupIndex(int Value);
	void __fastcall SetImageIndex(Imglist::TImageIndex Value);
	void __fastcall SetImages(Imglist::TCustomImageList* Value);
	void __fastcall SetInheritOptions(bool Value);
	void __fastcall SetLinkSubitems(TTBCustomItem* Value);
	void __fastcall SetMaskOptions(TTBItemOptions Value);
	void __fastcall SetOptions(TTBItemOptions Value);
	void __fastcall SetRadioItem(bool Value);
	void __fastcall SetSubMenuImages(Imglist::TCustomImageList* Value);
	void __fastcall SetVisible(bool Value);
	void __fastcall SubMenuImagesChanged(void);
	void __fastcall TurnSiblingsOff(void);
	
protected:
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	virtual void __fastcall Change(bool NeedResize);
	virtual TTBPopupWindow* __fastcall CreatePopup(const TTBView* ParentView, const TTBItemViewer* ParentViewer, const bool PositionAsSubmenu, const bool SelectFirstItem, const bool Customizing, const Types::TPoint &APopupPoint, const TTBPopupAlignment Alignment);
	virtual void __fastcall DoPopup(TTBCustomItem* Sender, bool FromLink);
	virtual void __fastcall EnabledChanged(void);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	virtual TTBView* __fastcall GetChevronParentView(void);
	DYNAMIC void __fastcall GetChildren(Classes::TGetChildProc Proc, Classes::TComponent* Root);
	virtual TMetaClass* __fastcall GetItemViewerClass(TTBView* AView);
	virtual void __fastcall GetPopupPosition(TTBView* ParentView, TTBPopupWindow* PopupWindow, TTBPopupPositionRec &PopupPositionRec);
	virtual TMetaClass* __fastcall GetPopupWindowClass(void);
	void __fastcall IndexError(void);
	virtual void __fastcall Loaded(void);
	virtual bool __fastcall NeedToRecreateViewer(TTBItemViewer* AViewer);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	TTBCustomItem* __fastcall OpenPopup(const bool SelectFirstItem, const bool TrackRightButton, const Types::TPoint &PopupPoint, const TTBPopupAlignment Alignment, const bool ReturnClickedItemOnly);
	void __fastcall RecreateItemViewers(void);
	DYNAMIC void __fastcall SetChildOrder(Classes::TComponent* Child, int Order);
	virtual void __fastcall SetName(const AnsiString NewName);
	DYNAMIC void __fastcall SetParentComponent(Classes::TComponent* Value);
	__property TTBCustomItemActionLink* ActionLink = {read=FActionLink, write=FActionLink};
	__property TTBItemStyle ItemStyle = {read=FItemStyle, write=FItemStyle, nodefault};
	
public:
	__fastcall virtual TTBCustomItem(Classes::TComponent* AOwner);
	__fastcall virtual ~TTBCustomItem(void);
	DYNAMIC bool __fastcall HasParent(void);
	DYNAMIC Classes::TComponent* __fastcall GetParentComponent(void);
	void __fastcall Add(TTBCustomItem* AItem);
	void __fastcall Clear(void);
	virtual void __fastcall Click(void);
	bool __fastcall ContainsItem(TTBCustomItem* AItem);
	void __fastcall Delete(int Index);
	AnsiString __fastcall GetShortCutText();
	int __fastcall IndexOf(TTBCustomItem* AItem);
	virtual void __fastcall InitiateAction(void);
	HIDESBASE void __fastcall Insert(int NewIndex, TTBCustomItem* AItem);
	bool __fastcall IsShortCut(Messages::TWMKey &Message);
	void __fastcall Move(int CurIndex, int NewIndex);
	TTBCustomItem* __fastcall Popup(int X, int Y, bool TrackRightButton, TTBPopupAlignment Alignment = (TTBPopupAlignment)(0x0), bool ReturnClickedItemOnly = false);
	void __fastcall PostClick(void);
	void __fastcall RegisterNotification(TTBItemChangedProc ANotify);
	HIDESBASE void __fastcall Remove(TTBCustomItem* Item);
	void __fastcall UnregisterNotification(TTBItemChangedProc ANotify);
	void __fastcall ViewBeginUpdate(void);
	void __fastcall ViewEndUpdate(void);
	__property Classes::TBasicAction* Action = {read=GetAction, write=SetAction};
	__property bool AutoCheck = {read=FAutoCheck, write=FAutoCheck, stored=IsAutoCheckStored, default=0};
	__property AnsiString Caption = {read=FCaption, write=SetCaption, stored=IsCaptionStored};
	__property int Count = {read=FItemCount, nodefault};
	__property bool Checked = {read=FChecked, write=SetChecked, stored=IsCheckedStored, default=0};
	__property TTBItemDisplayMode DisplayMode = {read=FDisplayMode, write=SetDisplayMode, default=0};
	__property TTBItemOptions EffectiveOptions = {read=FEffectiveOptions, nodefault};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, stored=IsEnabledStored, default=1};
	__property int GroupIndex = {read=FGroupIndex, write=SetGroupIndex, default=0};
	__property Classes::THelpContext HelpContext = {read=FHelpContext, write=FHelpContext, stored=IsHelpContextStored, default=0};
	__property AnsiString HelpKeyword = {read=FHelpKeyword, write=FHelpKeyword, stored=IsHelpContextStored};
	__property AnsiString Hint = {read=FHint, write=FHint, stored=IsHintStored};
	__property Imglist::TImageIndex ImageIndex = {read=FImageIndex, write=SetImageIndex, stored=IsImageIndexStored, default=-1};
	__property Imglist::TCustomImageList* Images = {read=FImages, write=SetImages};
	__property bool InheritOptions = {read=FInheritOptions, write=SetInheritOptions, default=1};
	__property TTBCustomItem* Items[int Index] = {read=GetItem/*, default*/};
	__property TTBCustomItem* LinkSubitems = {read=FLinkSubitems, write=SetLinkSubitems};
	__property TTBItemOptions MaskOptions = {read=FMaskOptions, write=SetMaskOptions, default=0};
	__property TTBItemOptions Options = {read=FOptions, write=SetOptions, default=0};
	__property TTBCustomItem* Parent = {read=FParent};
	__property Classes::TComponent* ParentComponent = {read=FParentComponent, write=FParentComponent};
	__property bool RadioItem = {read=FRadioItem, write=SetRadioItem, default=0};
	__property Classes::TShortCut ShortCut = {read=FShortCut, write=FShortCut, stored=IsShortCutStored, default=0};
	__property Imglist::TCustomImageList* SubMenuImages = {read=FSubMenuImages, write=SetSubMenuImages};
	__property bool Visible = {read=FVisible, write=SetVisible, stored=IsVisibleStored, default=1};
	__property Classes::TNotifyEvent OnClick = {read=FOnClick, write=FOnClick, stored=IsOnClickStored};
	__property TTBPopupEvent OnPopup = {read=FOnPopup, write=FOnPopup};
	__property TTBSelectEvent OnSelect = {read=FOnSelect, write=FOnSelect};
};


#pragma pack(push, 4)
struct TTBDoneActionData
{
	TTBDoneAction DoneAction;
	union
	{
		struct 
		{
			System::SmallStringBase<100>  HelpKeyword;
			
		};
		struct 
		{
			int ContextID;
			
		};
		struct 
		{
			HWND Wnd;
			unsigned Key;
			
		};
		struct 
		{
			TTBCustomItem* ClickItem;
			bool Sound;
			
		};
		
	};
} ;
#pragma pack(pop)

typedef void __fastcall (__closure *TTBInsertItemProc)(Classes::TComponent* AParent, TTBCustomItem* AItem);

class DELPHICLASS ETBItemError;
class PASCALIMPLEMENTATION ETBItemError : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall ETBItemError(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall ETBItemError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall ETBItemError(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall ETBItemError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall ETBItemError(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall ETBItemError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall ETBItemError(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall ETBItemError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~ETBItemError(void) { }
	#pragma option pop
	
};


#pragma pack(push, 4)
struct TTBPopupPositionRec
{
	bool PositionAsSubmenu;
	TTBPopupAlignment Alignment;
	bool Opposite;
	Types::TRect MonitorRect;
	Types::TRect ParentItemRect;
	int NCSizeX;
	int NCSizeY;
	int X;
	int Y;
	int W;
	int H;
	Tb2anim::TTBAnimationDirection AnimDir;
	bool PlaySound;
} ;
#pragma pack(pop)

class DELPHICLASS TTBBaseAccObject;
class PASCALIMPLEMENTATION TTBBaseAccObject : public System::TInterfacedObject 
{
	typedef System::TInterfacedObject inherited;
	
public:
	virtual void __fastcall ClientIsDestroying(void) = 0 ;
	HRESULT __stdcall GetTypeInfoCount(/* out */ int &Count);
	HRESULT __stdcall GetTypeInfo(int Index, int LocaleID, /* out */ void *TypeInfo);
	HRESULT __stdcall GetIDsOfNames(const GUID &IID, void * Names, int NameCount, int LocaleID, void * DispIDs);
	HRESULT __stdcall Invoke(int DispID, const GUID &IID, int LocaleID, Word Flags, void *Params, void * VarResult, void * ExcepInfo, void * ArgErr);
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TTBBaseAccObject(void) : System::TInterfacedObject() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TTBBaseAccObject(void) { }
	#pragma option pop
	
private:
	void *__IDispatch;	/* IDispatch */
	
public:
	operator IDispatch*(void) { return (IDispatch*)&__IDispatch; }
	
};


#pragma option push -b-
enum TB2Item__8 { tbisInvalidated, tbisLineSep };
#pragma option pop

class PASCALIMPLEMENTATION TTBItemViewer : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	#pragma pack(push, 1)
	Types::TRect FBoundsRect;
	#pragma pack(pop)
	
	bool FClipped;
	int FGroupLevel;
	TTBCustomItem* FItem;
	bool FOffEdge;
	bool FShow;
	TTBView* FView;
	void __fastcall AccSelect(const bool AExecute);
	int __fastcall GetIndex(void);
	
protected:
	TTBBaseAccObject* FAccObjectInstance;
	virtual void __fastcall CalcSize(const Graphics::TCanvas* Canvas, int &AWidth, int &AHeight);
	DYNAMIC bool __fastcall CaptionShown(void);
	virtual bool __fastcall DoExecute(void);
	virtual void __fastcall DrawItemCaption(const Graphics::TCanvas* Canvas, const Types::TRect &ARect, const AnsiString ACaption, bool ADrawDisabledShadow, unsigned AFormat);
	virtual void __fastcall Entering(TTBItemViewer* OldSelected);
	virtual int __fastcall GetAccRole(void);
	virtual bool __fastcall GetAccValue(WideString &Value);
	virtual AnsiString __fastcall GetCaptionText();
	virtual void __fastcall GetCursor(const Types::TPoint &Pt, HICON &ACursor);
	Imglist::TCustomImageList* __fastcall GetImageList(void);
	bool __fastcall ImageShown(void);
	bool __fastcall IsRotated(void);
	virtual bool __fastcall IsToolbarSize(void);
	virtual bool __fastcall IsPtInButtonPart(int X, int Y);
	virtual void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	virtual void __fastcall Leaving(void);
	virtual void __fastcall LosingCapture(void);
	virtual void __fastcall MouseDown(Classes::TShiftState Shift, int X, int Y, bool &MouseDownOnMenu);
	virtual void __fastcall MouseMove(int X, int Y);
	virtual void __fastcall MouseUp(int X, int Y, bool MouseWasDownOnMenu);
	virtual void __fastcall MouseWheel(int WheelDelta, int X, int Y);
	virtual void __fastcall Paint(const Graphics::TCanvas* Canvas, const Types::TRect &ClientAreaRect, bool IsSelected, bool IsPushed, bool UseDisabledShadow);
	void __fastcall PostAccSelect(const bool AExecute);
	virtual bool __fastcall UsesSameWidth(void);
	
public:
	System::Set<TB2Item__8, tbisInvalidated, tbisLineSep>  State;
	__property Types::TRect BoundsRect = {read=FBoundsRect};
	__property bool Clipped = {read=FClipped, nodefault};
	__property int Index = {read=GetIndex, nodefault};
	__property TTBCustomItem* Item = {read=FItem};
	__property bool OffEdge = {read=FOffEdge, nodefault};
	__property bool Show = {read=FShow, nodefault};
	__property TTBView* View = {read=FView};
	__fastcall virtual TTBItemViewer(TTBView* AView, TTBCustomItem* AItem, int AGroupLevel);
	__fastcall virtual ~TTBItemViewer(void);
	void __fastcall Execute(bool AGivePriority);
	_di_IDispatch __fastcall GetAccObject();
	AnsiString __fastcall GetHintText();
	bool __fastcall IsAccessible(void);
	virtual bool __fastcall IsToolbarStyle(void);
	Types::TPoint __fastcall ScreenToClient(const Types::TPoint &P);
};


typedef TTBItemViewer* TTBItemViewerArray[536870911];

typedef TTBItemViewer* *PTBItemViewerArray;

#pragma option push -b-
enum TTBViewOrientation { tbvoHorizontal, tbvoVertical, tbvoFloating };
#pragma option pop

#pragma option push -b-
enum TB2Item__9 { tbetMouseDown, tbetExecuteSelected, tbetFromMSAA };
#pragma option pop

typedef Set<TB2Item__9, tbetMouseDown, tbetFromMSAA>  TTBEnterToolbarLoopOptions;

#pragma option push -b-
enum TB2Item__01 { vsModal, vsMouseInWindow, vsDrawInOrder, vsOppositePopup, vsIgnoreFirstMouseUp, vsShowAccels, vsDropDownMenus, vsNoAnimation };
#pragma option pop

typedef Set<TB2Item__01, vsModal, vsNoAnimation>  TTBViewState;

#pragma option push -b-
enum TB2Item__11 { vsMenuBar, vsUseHiddenAccels, vsAlwaysShowHints };
#pragma option pop

typedef Set<TB2Item__11, vsMenuBar, vsAlwaysShowHints>  TTBViewStyle;

#pragma option push -b-
enum TTBViewTimerID { tiOpen, tiClose, tiScrollUp, tiScrollDown };
#pragma option pop

typedef TMetaClass*TTBViewClass;

class PASCALIMPLEMENTATION TTBView : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	System::Set<TTBViewTimerID, tiOpen, tiScrollDown>  FActiveTimers;
	Graphics::TColor FBackgroundColor;
	#pragma pack(push, 1)
	Types::TPoint FBaseSize;
	#pragma pack(pop)
	
	bool FCapture;
	HWND FCaptureWnd;
	int FChevronOffset;
	TTBView* FChevronParentView;
	int FChevronSize;
	TTBCustomItem* FCurParentItem;
	bool FCustomizing;
	TTBDoneActionData FDoneActionData;
	int FInternalViewersAtEnd;
	int FInternalViewersAtFront;
	bool FIsPopup;
	bool FIsToolbar;
	int FMaxHeight;
	#pragma pack(push, 1)
	Types::TRect FMonitorRect;
	#pragma pack(pop)
	
	bool FMouseOverSelected;
	bool FNewViewersGetHighestPriority;
	TTBItemViewer* FOpenViewer;
	TTBView* FOpenViewerView;
	TTBPopupWindow* FOpenViewerWindow;
	TTBView* FParentView;
	TTBCustomItem* FParentItem;
	Classes::TList* FPriorityList;
	TTBViewOrientation FOrientation;
	int FScrollOffset;
	TTBItemViewer* FSelected;
	bool FSelectedViaMouse;
	bool FShowDownArrow;
	bool FShowUpArrow;
	TTBViewState FState;
	TTBViewStyle FStyle;
	int FUpdating;
	bool FUsePriorityList;
	bool FValidated;
	int FViewerCount;
	TTBItemViewer* *FViewers;
	Controls::TWinControl* FWindow;
	int FWrapOffset;
	void __fastcall DeletingViewer(TTBItemViewer* Viewer);
	void __fastcall DrawItem(TTBItemViewer* Viewer, Graphics::TCanvas* DrawTo, bool Offscreen);
	void __fastcall FreeViewers(void);
	void __fastcall ImagesChanged(void);
	int __fastcall InsertItemViewers(const int NewIndex, const TTBCustomItem* AItem, const int AGroupLevel, const bool AddToPriorityList, const bool TopOfPriorityList);
	void __fastcall ItemNotification(TTBCustomItem* Ancestor, bool Relayed, TTBItemChangedAction Action, int Index, TTBCustomItem* Item);
	void __fastcall LinkNotification(TTBCustomItem* Ancestor, bool Relayed, TTBItemChangedAction Action, int Index, TTBCustomItem* Item);
	void __fastcall RecreateItemViewer(const int I);
	void __fastcall Scroll(bool ADown);
	void __fastcall SetCustomizing(bool Value);
	void __fastcall SetSelected(TTBItemViewer* Value);
	void __fastcall SetUsePriorityList(bool Value);
	void __fastcall StartTimer(const TTBViewTimerID ATimer, const int Interval);
	void __fastcall StopAllTimers(void);
	void __fastcall StopTimer(const TTBViewTimerID ATimer);
	void __fastcall UpdateCurParentItem(void);
	
protected:
	TTBBaseAccObject* FAccObjectInstance;
	virtual void __fastcall AutoSize(int AWidth, int AHeight);
	bool __fastcall CalculatePositions(const bool CanMoveControls, const TTBViewOrientation AOrientation, int AWrapOffset, int AChevronOffset, int AChevronSize, Types::TPoint &ABaseSize, Types::TPoint &TotalSize, int &AWrappedLines);
	virtual void __fastcall DoUpdatePositions(Types::TPoint &ASize);
	virtual TTBCustomItem* __fastcall GetChevronItem(void);
	virtual void __fastcall GetMargins(TTBViewOrientation AOrientation, Types::TRect &Margins);
	virtual TTBCustomItem* __fastcall GetMDIButtonsItem(void);
	virtual TTBCustomItem* __fastcall GetMDISystemMenuItem(void);
	TTBView* __fastcall GetParentToolbarView(void);
	TTBView* __fastcall GetRootView(void);
	bool __fastcall HandleWMGetObject(Messages::TMessage &Message);
	void __fastcall InitiateActions(void);
	virtual void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	void __fastcall SetAccelsVisibility(bool AShowAccels);
	void __fastcall SetState(TTBViewState AState);
	__property TTBDoneActionData DoneActionData = {read=FDoneActionData, write=FDoneActionData};
	__property bool ShowDownArrow = {read=FShowDownArrow, nodefault};
	__property bool ShowUpArrow = {read=FShowUpArrow, nodefault};
	
public:
	__fastcall virtual TTBView(Classes::TComponent* AOwner, TTBView* AParentView, TTBCustomItem* AParentItem, Controls::TWinControl* AWindow, bool AIsToolbar, bool ACustomizing, bool AUsePriorityList);
	__fastcall virtual ~TTBView(void);
	void __fastcall BeginUpdate(void);
	void __fastcall CancelCapture(void);
	void __fastcall CancelChildPopups(void);
	void __fastcall CancelMode(void);
	void __fastcall CloseChildPopups(void);
	bool __fastcall ContainsView(TTBView* AView);
	void __fastcall DrawSubitems(Graphics::TCanvas* ACanvas);
	void __fastcall EndModal(void);
	void __fastcall EndModalWithClick(TTBItemViewer* AViewer);
	void __fastcall EndModalWithHelp(int AContextID)/* overload */;
	void __fastcall EndModalWithHelp(AnsiString HelpKeyword)/* overload */;
	void __fastcall EndModalWithSystemMenu(HWND AWnd, unsigned AKey);
	void __fastcall EndUpdate(void);
	void __fastcall EnterToolbarLoop(TTBEnterToolbarLoopOptions Options);
	void __fastcall ExecuteSelected(bool AGivePriority);
	TTBItemViewer* __fastcall Find(TTBCustomItem* Item);
	TTBItemViewer* __fastcall FirstSelectable(void);
	_di_IDispatch __fastcall GetAccObject();
	HWND __fastcall GetCaptureWnd(void);
	virtual Graphics::TFont* __fastcall GetFont(void);
	void __fastcall GetOffEdgeControlList(const Classes::TList* List);
	void __fastcall GivePriority(TTBItemViewer* AViewer);
	TTBItemViewer* __fastcall HighestPriorityViewer(void);
	void __fastcall Invalidate(TTBItemViewer* AViewer);
	virtual void __fastcall InvalidatePositions(void);
	int __fastcall IndexOf(TTBItemViewer* AViewer);
	bool __fastcall IsModalEnding(void);
	TTBItemViewer* __fastcall NextSelectable(TTBItemViewer* CurViewer, bool GoForward);
	TTBItemViewer* __fastcall NextSelectableWithAccel(TTBItemViewer* CurViewer, char Key, bool RequirePrimaryAccel, bool &IsOnlyItemWithAccel);
	void __fastcall NotifyFocusEvent(void);
	bool __fastcall OpenChildPopup(const bool SelectFirstItem);
	void __fastcall RecreateAllViewers(void);
	void __fastcall ScrollSelectedIntoView(void);
	void __fastcall Select(TTBItemViewer* Value, bool ViaMouse);
	void __fastcall SetCapture(void);
	void __fastcall TryValidatePositions(void);
	void __fastcall UpdateSelection(const Types::PPoint P, const bool AllowNewSelection);
	Types::TPoint __fastcall UpdatePositions();
	void __fastcall ValidatePositions(void);
	TTBItemViewer* __fastcall ViewerFromPoint(const Types::TPoint &P);
	__property Graphics::TColor BackgroundColor = {read=FBackgroundColor, write=FBackgroundColor, nodefault};
	__property Types::TPoint BaseSize = {read=FBaseSize};
	__property bool Capture = {read=FCapture, nodefault};
	__property int ChevronOffset = {read=FChevronOffset, write=FChevronOffset, nodefault};
	__property int ChevronSize = {read=FChevronSize, write=FChevronSize, nodefault};
	__property bool Customizing = {read=FCustomizing, write=SetCustomizing, nodefault};
	__property bool IsPopup = {read=FIsPopup, nodefault};
	__property bool IsToolbar = {read=FIsToolbar, nodefault};
	__property bool MouseOverSelected = {read=FMouseOverSelected, nodefault};
	__property bool NewViewersGetHighestPriority = {read=FNewViewersGetHighestPriority, write=FNewViewersGetHighestPriority, nodefault};
	__property TTBView* ParentView = {read=FParentView};
	__property TTBCustomItem* ParentItem = {read=FParentItem};
	__property TTBItemViewer* OpenViewer = {read=FOpenViewer};
	__property TTBView* OpenViewerView = {read=FOpenViewerView};
	__property TTBViewOrientation Orientation = {read=FOrientation, write=FOrientation, nodefault};
	__property TTBItemViewer* Selected = {read=FSelected, write=SetSelected};
	__property bool SelectedViaMouse = {read=FSelectedViaMouse, nodefault};
	__property TTBViewState State = {read=FState, nodefault};
	__property TTBViewStyle Style = {read=FStyle, write=FStyle, nodefault};
	__property bool UsePriorityList = {read=FUsePriorityList, write=SetUsePriorityList, nodefault};
	__property PTBItemViewerArray Viewers = {read=FViewers};
	__property int ViewerCount = {read=FViewerCount, nodefault};
	__property Controls::TWinControl* Window = {read=FWindow};
	__property int WrapOffset = {read=FWrapOffset, write=FWrapOffset, nodefault};
public:
	#pragma option push -w-inl
	/* TComponent.Create */ inline __fastcall virtual TTBView(Classes::TComponent* AOwner) : Classes::TComponent(AOwner) { }
	#pragma option pop
	
};


typedef TMetaClass*TTBRootItemClass;

class DELPHICLASS TTBRootItem;
class PASCALIMPLEMENTATION TTBRootItem : public TTBCustomItem 
{
	typedef TTBCustomItem inherited;
	
public:
	#pragma option push -w-inl
	/* TTBCustomItem.Create */ inline __fastcall virtual TTBRootItem(Classes::TComponent* AOwner) : TTBCustomItem(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBCustomItem.Destroy */ inline __fastcall virtual ~TTBRootItem(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBItem;
class PASCALIMPLEMENTATION TTBItem : public TTBCustomItem 
{
	typedef TTBCustomItem inherited;
	
__published:
	__property Action ;
	__property AutoCheck  = {default=0};
	__property Caption ;
	__property Checked  = {default=0};
	__property DisplayMode  = {default=0};
	__property Enabled  = {default=1};
	__property GroupIndex  = {default=0};
	__property HelpContext  = {default=0};
	__property HelpKeyword ;
	__property Hint ;
	__property ImageIndex  = {default=-1};
	__property Images ;
	__property InheritOptions  = {default=1};
	__property MaskOptions  = {default=0};
	__property Options  = {default=0};
	__property RadioItem  = {default=0};
	__property ShortCut  = {default=0};
	__property Visible  = {default=1};
	__property OnClick ;
	__property OnSelect ;
public:
	#pragma option push -w-inl
	/* TTBCustomItem.Create */ inline __fastcall virtual TTBItem(Classes::TComponent* AOwner) : TTBCustomItem(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBCustomItem.Destroy */ inline __fastcall virtual ~TTBItem(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBGroupItem;
class PASCALIMPLEMENTATION TTBGroupItem : public TTBCustomItem 
{
	typedef TTBCustomItem inherited;
	
public:
	__fastcall virtual TTBGroupItem(Classes::TComponent* AOwner);
	
__published:
	__property InheritOptions  = {default=1};
	__property LinkSubitems ;
	__property MaskOptions  = {default=0};
	__property Options  = {default=0};
public:
	#pragma option push -w-inl
	/* TTBCustomItem.Destroy */ inline __fastcall virtual ~TTBGroupItem(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBSubmenuItem;
class PASCALIMPLEMENTATION TTBSubmenuItem : public TTBCustomItem 
{
	typedef TTBCustomItem inherited;
	
private:
	bool __fastcall GetDropdownCombo(void);
	void __fastcall SetDropdownCombo(bool Value);
	
public:
	__fastcall virtual TTBSubmenuItem(Classes::TComponent* AOwner);
	
__published:
	__property Action ;
	__property AutoCheck  = {default=0};
	__property Caption ;
	__property Checked  = {default=0};
	__property DisplayMode  = {default=0};
	__property bool DropdownCombo = {read=GetDropdownCombo, write=SetDropdownCombo, default=0};
	__property Enabled  = {default=1};
	__property GroupIndex  = {default=0};
	__property HelpContext  = {default=0};
	__property HelpKeyword ;
	__property Hint ;
	__property ImageIndex  = {default=-1};
	__property Images ;
	__property InheritOptions  = {default=1};
	__property LinkSubitems ;
	__property MaskOptions  = {default=0};
	__property Options  = {default=0};
	__property RadioItem  = {default=0};
	__property ShortCut  = {default=0};
	__property SubMenuImages ;
	__property Visible  = {default=1};
	__property OnClick ;
	__property OnPopup ;
	__property OnSelect ;
public:
	#pragma option push -w-inl
	/* TTBCustomItem.Destroy */ inline __fastcall virtual ~TTBSubmenuItem(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBSeparatorItem;
class PASCALIMPLEMENTATION TTBSeparatorItem : public TTBCustomItem 
{
	typedef TTBCustomItem inherited;
	
private:
	bool FBlank;
	void __fastcall SetBlank(bool Value);
	
protected:
	virtual TMetaClass* __fastcall GetItemViewerClass(TTBView* AView);
	
public:
	__fastcall virtual TTBSeparatorItem(Classes::TComponent* AOwner);
	
__published:
	__property bool Blank = {read=FBlank, write=SetBlank, default=0};
	__property Hint ;
	__property Visible  = {default=1};
public:
	#pragma option push -w-inl
	/* TTBCustomItem.Destroy */ inline __fastcall virtual ~TTBSeparatorItem(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBSeparatorItemViewer;
class PASCALIMPLEMENTATION TTBSeparatorItemViewer : public TTBItemViewer 
{
	typedef TTBItemViewer inherited;
	
protected:
	virtual void __fastcall CalcSize(const Graphics::TCanvas* Canvas, int &AWidth, int &AHeight);
	virtual void __fastcall Paint(const Graphics::TCanvas* Canvas, const Types::TRect &ClientAreaRect, bool IsSelected, bool IsPushed, bool UseDisabledShadow);
	virtual bool __fastcall UsesSameWidth(void);
public:
	#pragma option push -w-inl
	/* TTBItemViewer.Create */ inline __fastcall virtual TTBSeparatorItemViewer(TTBView* AView, TTBCustomItem* AItem, int AGroupLevel) : TTBItemViewer(AView, AItem, AGroupLevel) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBItemViewer.Destroy */ inline __fastcall virtual ~TTBSeparatorItemViewer(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBControlItem;
class PASCALIMPLEMENTATION TTBControlItem : public TTBCustomItem 
{
	typedef TTBCustomItem inherited;
	
private:
	Controls::TControl* FControl;
	bool FDontFreeControl;
	void __fastcall SetControl(Controls::TControl* Value);
	
protected:
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	
public:
	__fastcall virtual TTBControlItem(Classes::TComponent* AOwner);
	__fastcall TTBControlItem(Classes::TComponent* AOwner, Controls::TControl* AControl);
	__fastcall virtual ~TTBControlItem(void);
	__property bool DontFreeControl = {read=FDontFreeControl, write=FDontFreeControl, nodefault};
	
__published:
	__property Controls::TControl* Control = {read=FControl, write=SetControl};
};


class DELPHICLASS TTBPopupView;
class PASCALIMPLEMENTATION TTBPopupView : public TTBView 
{
	typedef TTBView inherited;
	
protected:
	virtual void __fastcall AutoSize(int AWidth, int AHeight);
	
public:
	virtual Graphics::TFont* __fastcall GetFont(void);
public:
	#pragma option push -w-inl
	/* TTBView.CreateView */ inline __fastcall virtual TTBPopupView(Classes::TComponent* AOwner, TTBView* AParentView, TTBCustomItem* AParentItem, Controls::TWinControl* AWindow, bool AIsToolbar, bool ACustomizing, bool AUsePriorityList) : TTBView(AOwner, AParentView, AParentItem, AWindow, AIsToolbar, ACustomizing, AUsePriorityList) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBView.Destroy */ inline __fastcall virtual ~TTBPopupView(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TComponent.Create */ inline __fastcall virtual TTBPopupView(Classes::TComponent* AOwner) : TTBView(AOwner) { }
	#pragma option pop
	
};


__interface ITBPopupWindow;
typedef System::DelphiInterface<ITBPopupWindow> _di_ITBPopupWindow;
__interface INTERFACE_UUID("{E45CBE74-1ECF-44CB-B064-6D45B1924708}") ITBPopupWindow  : public IInterface 
{
	
};

class PASCALIMPLEMENTATION TTBPopupWindow : public Controls::TCustomControl 
{
	typedef Controls::TCustomControl inherited;
	
private:
	bool FAccelsVisibilitySet;
	Tb2anim::TTBAnimationDirection FAnimationDirection;
	TTBView* FView;
	HIDESBASE MESSAGE void __fastcall CMHintShow(Forms::TCMHintShow &Message);
	HIDESBASE MESSAGE void __fastcall CMShowingChanged(Messages::TMessage &Message);
	MESSAGE void __fastcall WMClose(Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Messages::TWMEraseBkgnd &Message);
	MESSAGE void __fastcall WMGetObject(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Messages::TWMNCCalcSize &Message);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMPaint(Messages::TWMPaint &Message);
	MESSAGE void __fastcall WMPrint(Messages::TMessage &Message);
	MESSAGE void __fastcall WMPrintClient(Messages::TMessage &Message);
	MESSAGE void __fastcall WMTB2kStepAnimation(Messages::TMessage &Message);
	MESSAGE void __fastcall WMTB2kAnimationEnded(Messages::TMessage &Message);
	
protected:
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd(void);
	virtual void __fastcall DestroyWindowHandle(void);
	DYNAMIC Types::TPoint __fastcall GetNCSize();
	DYNAMIC TMetaClass* __fastcall GetViewClass(void);
	virtual void __fastcall Paint(void);
	virtual void __fastcall PaintScrollArrows(void);
	__property Tb2anim::TTBAnimationDirection AnimationDirection = {read=FAnimationDirection, nodefault};
	
public:
	__fastcall virtual TTBPopupWindow(Classes::TComponent* AOwner, const TTBView* AParentView, const TTBCustomItem* AItem, const bool ACustomizing);
	__fastcall virtual ~TTBPopupWindow(void);
	virtual void __fastcall BeforeDestruction(void);
	__property TTBView* View = {read=FView};
public:
	#pragma option push -w-inl
	/* TCustomControl.Create */ inline __fastcall virtual TTBPopupWindow(Classes::TComponent* AOwner) : Controls::TCustomControl(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTBPopupWindow(HWND ParentWindow) : Controls::TCustomControl(ParentWindow) { }
	#pragma option pop
	
private:
	void *__ITBPopupWindow;	/* Tb2item::ITBPopupWindow */
	
public:
	operator ITBPopupWindow*(void) { return (ITBPopupWindow*)&__ITBPopupWindow; }
	
};


__interface ITBItems;
typedef System::DelphiInterface<ITBItems> _di_ITBItems;
__interface INTERFACE_UUID("{A5C0D7CC-3EC4-4090-A0F8-3D03271877EA}") ITBItems  : public IInterface 
{
	
public:
	virtual TTBCustomItem* __fastcall GetItems(void) = 0 ;
};

class DELPHICLASS TTBItemContainer;
class PASCALIMPLEMENTATION TTBItemContainer : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	TTBRootItem* FItem;
	Imglist::TCustomImageList* __fastcall GetImages(void);
	TTBCustomItem* __fastcall GetItems(void);
	void __fastcall SetImages(Imglist::TCustomImageList* Value);
	
protected:
	DYNAMIC void __fastcall GetChildren(Classes::TGetChildProc Proc, Classes::TComponent* Root);
	
public:
	__fastcall virtual TTBItemContainer(Classes::TComponent* AOwner);
	__fastcall virtual ~TTBItemContainer(void);
	__property TTBRootItem* Items = {read=FItem};
	
__published:
	__property Imglist::TCustomImageList* Images = {read=GetImages, write=SetImages};
private:
	void *__ITBItems;	/* Tb2item::ITBItems */
	
public:
	operator ITBItems*(void) { return (ITBItems*)&__ITBItems; }
	
};


class DELPHICLASS TTBPopupMenu;
class PASCALIMPLEMENTATION TTBPopupMenu : public Menus::TPopupMenu 
{
	typedef Menus::TPopupMenu inherited;
	
private:
	TTBRootItem* FItem;
	Imglist::TCustomImageList* __fastcall GetImages(void);
	TTBCustomItem* __fastcall GetItems(void);
	TTBCustomItem* __fastcall GetLinkSubitems(void);
	TTBItemOptions __fastcall GetOptions(void);
	void __fastcall RootItemClick(System::TObject* Sender);
	HIDESBASE void __fastcall SetImages(Imglist::TCustomImageList* Value);
	void __fastcall SetLinkSubitems(TTBCustomItem* Value);
	void __fastcall SetOptions(TTBItemOptions Value);
	
protected:
	DYNAMIC void __fastcall GetChildren(Classes::TGetChildProc Proc, Classes::TComponent* Root);
	DYNAMIC TMetaClass* __fastcall GetRootItemClass(void);
	DYNAMIC void __fastcall SetChildOrder(Classes::TComponent* Child, int Order);
	
public:
	__fastcall virtual TTBPopupMenu(Classes::TComponent* AOwner);
	__fastcall virtual ~TTBPopupMenu(void);
	DYNAMIC bool __fastcall IsShortCut(Messages::TWMKey &Message);
	virtual void __fastcall Popup(int X, int Y);
	TTBCustomItem* __fastcall PopupEx(int X, int Y, bool ReturnClickedItemOnly = false);
	
__published:
	__property Imglist::TCustomImageList* Images = {read=GetImages, write=SetImages};
	__property TTBRootItem* Items = {read=FItem};
	__property TTBCustomItem* LinkSubitems = {read=GetLinkSubitems, write=SetLinkSubitems};
	__property TTBItemOptions Options = {read=GetOptions, write=SetOptions, default=0};
private:
	void *__ITBItems;	/* Tb2item::ITBItems */
	
public:
	operator ITBItems*(void) { return (ITBItems*)&__ITBItems; }
	
};


class DELPHICLASS TTBCustomImageList;
class PASCALIMPLEMENTATION TTBCustomImageList : public Controls::TImageList 
{
	typedef Controls::TImageList inherited;
	
private:
	Imglist::TCustomImageList* FCheckedImages;
	Imglist::TChangeLink* FCheckedImagesChangeLink;
	Imglist::TCustomImageList* FDisabledImages;
	Imglist::TChangeLink* FDisabledImagesChangeLink;
	Imglist::TCustomImageList* FHotImages;
	Imglist::TChangeLink* FHotImagesChangeLink;
	Graphics::TBitmap* FImagesBitmap;
	Graphics::TColor FImagesBitmapMaskColor;
	void __fastcall ChangeImages(Imglist::TCustomImageList* &AImageList, Imglist::TCustomImageList* Value, Imglist::TChangeLink* AChangeLink);
	void __fastcall ImageListChanged(System::TObject* Sender);
	void __fastcall ImagesBitmapChanged(System::TObject* Sender);
	void __fastcall SetCheckedImages(Imglist::TCustomImageList* Value);
	void __fastcall SetDisabledImages(Imglist::TCustomImageList* Value);
	void __fastcall SetHotImages(Imglist::TCustomImageList* Value);
	void __fastcall SetImagesBitmap(Graphics::TBitmap* Value);
	void __fastcall SetImagesBitmapMaskColor(Graphics::TColor Value);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	__property Imglist::TCustomImageList* CheckedImages = {read=FCheckedImages, write=SetCheckedImages};
	__property Imglist::TCustomImageList* DisabledImages = {read=FDisabledImages, write=SetDisabledImages};
	__property Imglist::TCustomImageList* HotImages = {read=FHotImages, write=SetHotImages};
	__property Graphics::TBitmap* ImagesBitmap = {read=FImagesBitmap, write=SetImagesBitmap};
	__property Graphics::TColor ImagesBitmapMaskColor = {read=FImagesBitmapMaskColor, write=SetImagesBitmapMaskColor, default=16711935};
	
public:
	__fastcall virtual TTBCustomImageList(Classes::TComponent* AOwner);
	__fastcall virtual ~TTBCustomImageList(void);
	virtual void __fastcall DrawState(Graphics::TCanvas* Canvas, int X, int Y, int Index, bool Enabled, bool Selected, bool Checked);
public:
	#pragma option push -w-inl
	/* TCustomImageList.CreateSize */ inline __fastcall TTBCustomImageList(int AWidth, int AHeight) : Controls::TImageList(AWidth, AHeight) { }
	#pragma option pop
	
};


class DELPHICLASS TTBImageList;
class PASCALIMPLEMENTATION TTBImageList : public TTBCustomImageList 
{
	typedef TTBCustomImageList inherited;
	
__published:
	__property CheckedImages ;
	__property DisabledImages ;
	__property HotImages ;
	__property ImagesBitmap ;
	__property ImagesBitmapMaskColor  = {default=16711935};
public:
	#pragma option push -w-inl
	/* TTBCustomImageList.Create */ inline __fastcall virtual TTBImageList(Classes::TComponent* AOwner) : TTBCustomImageList(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBCustomImageList.Destroy */ inline __fastcall virtual ~TTBImageList(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomImageList.CreateSize */ inline __fastcall TTBImageList(int AWidth, int AHeight) : TTBCustomImageList(AWidth, AHeight) { }
	#pragma option pop
	
};


class DELPHICLASS TTBModalHandler;
class PASCALIMPLEMENTATION TTBModalHandler : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	bool FCreatedWnd;
	bool FInited;
	HWND FWnd;
	TTBPopupWindow* FRootPopup;
	void __fastcall WndProc(Messages::TMessage &Msg);
	
public:
	__fastcall TTBModalHandler(HWND AExistingWnd);
	__fastcall virtual ~TTBModalHandler(void);
	void __fastcall Loop(const TTBView* RootView, const bool AMouseDown, const bool AExecuteSelected, const bool AFromMSAA, const bool TrackRightButton);
	__property TTBPopupWindow* RootPopup = {read=FRootPopup, write=FRootPopup};
	__property HWND Wnd = {read=FWnd, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
static const Word WM_TB2K_POPUPSHOWING = 0x62a;
static const Shortint TPS_ANIMSTART = 0x1;
static const Shortint TPS_ANIMFINISHED = 0x2;
static const Shortint TPS_NOANIM = 0x3;
static const int tbMenuBkColor = 0x80000004;
static const int tbMenuTextColor = 0x80000007;
static const Shortint tbMenuVerticalMargin = 0x4;
static const Shortint tbMenuImageTextSpace = 0x1;
static const Shortint tbMenuLeftTextMargin = 0x2;
static const Shortint tbMenuRightTextMargin = 0x3;
static const Shortint tbMenuSeparatorOffset = 0xc;
static const Shortint tbMenuScrollArrowHeight = 0x13;
static const Shortint tbDropdownArrowWidth = 0x8;
static const Shortint tbDropdownArrowMargin = 0x3;
static const Shortint tbDropdownComboArrowWidth = 0xb;
static const Shortint tbDropdownComboMargin = 0x2;
static const Shortint tbLineSpacing = 0x6;
static const Shortint tbLineSepOffset = 0x1;
static const Shortint tbDockedLineSepOffset = 0x4;
static const Word WM_TB2K_CLICKITEM = 0x500;
extern PACKAGE Graphics::TFont* ToolbarFont;
extern PACKAGE TTBCustomItem* __fastcall ProcessDoneAction(const TTBDoneActionData &DoneActionData, const bool ReturnClickedItemOnly);
extern PACKAGE void __fastcall TBInitToolbarSystemFont(void);

}	/* namespace Tb2item */
using namespace Tb2item;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TB2Item
