// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DragDrop.pas' rev: 6.00

#ifndef DragDropHPP
#define DragDropHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Grids.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <ActiveX.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <ShellAPI.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------
#include <oleidl.h>

namespace Dragdrop
{
//-- type declarations -------------------------------------------------------
;

;

typedef tagFORMATETC  TFormatEtc;

typedef tagSTGMEDIUM  TStgMedium;

#pragma option push -b-
enum TDropEffect { deCopy, deMove, deLink };
#pragma option pop

#pragma option push -b-
enum TDragResult { drInvalid, drCancelled, drCopy, drMove, drLink };
#pragma option pop

typedef Set<TDropEffect, deCopy, deLink>  TDropEffectSet;

#pragma option push -b-
enum TDragDetectStatus { ddsNone, ddsLeft, ddsRight, ddsCancelled, ddsDrag };
#pragma option pop

#pragma option push -b-
enum TRenderDataOn { rdoEnter, rdoEnterAndDropSync, rdoEnterAndDropAsync, rdoDropSync, rdoDropAsync, rdoNever };
#pragma option pop

#pragma option push -b-
enum TSrcCompatibilityCheck { CheckLindex, CheckdwAspect };
#pragma option pop

typedef Set<TSrcCompatibilityCheck, CheckLindex, CheckdwAspect>  TSrcCompatibilityCheckSet;

typedef Word TScrollInterval;

#pragma option push -b-
enum TScrollDirection { sdUp, sdDown, sdLeft, sdRight };
#pragma option pop

typedef void __fastcall (__closure *TOnDragEnter)(_di_IDataObject DataObj, int grfKeyState, const Types::TPoint &pt, int &dwEffect, bool &Accept);

typedef void __fastcall (__closure *TOnDragLeave)(void);

typedef void __fastcall (__closure *TOnDragOver)(int grfKeyState, const Types::TPoint &pt, int &dwEffect);

typedef void __fastcall (__closure *TOnDrop)(_di_IDataObject DataObj, int grfKeyState, const Types::TPoint &pt, int &dwEffect);

typedef void __fastcall (__closure *TOnQueryContinueDrag)(BOOL fEscapePressed, int grfKeyState, HRESULT &Result);

typedef void __fastcall (__closure *TOnGiveFeedback)(int dwEffect, HRESULT &Result);

typedef void __fastcall (__closure *TOnDragDetect)(int grfKeyState, const Types::TPoint &DetectStart, const Types::TPoint &pt, TDragDetectStatus DragDetectStatus);

typedef void __fastcall (__closure *TOnProcessDropped)(System::TObject* Sender, int grfKeyState, const Types::TPoint &pt, int dwEffect);

typedef void __fastcall (__closure *TOnBeforeScrolling)(System::TObject* Sender, const Types::TPoint &pt, TScrollInterval &Interval, TScrollDirection ScrollDirection, bool &ScrollPage);

typedef void __fastcall (__closure *TOnMenuPopup)(System::TObject* Sender, HMENU AMenu, _di_IDataObject DataObj, int AMinCustCmd, int grfKeyState, const Types::TPoint &pt);

typedef void __fastcall (__closure *TOnMenuExecCmd)(System::TObject* Sender, HMENU AMenu, _di_IDataObject DataObj, int Command, int &dwEffect, bool &Succeeded);

typedef void __fastcall (__closure *TOnMenuDestroy)(System::TObject* Sender, HMENU AMenu);

typedef DynamicArray<tagFORMATETC >  TFormatEtcArray;

#pragma pack(push, 1)
struct TDetectRec
{
	
} ;
#pragma pack(pop)

class DELPHICLASS TFormatEtcList;
class PASCALIMPLEMENTATION TFormatEtcList : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	int FCount;
	DynamicArray<tagFORMATETC >  FList;
	tagFORMATETC __fastcall Get(int Index);
	void __fastcall Put(int Index, const tagFORMATETC &Item);
	
public:
	__fastcall TFormatEtcList(void);
	__fastcall virtual ~TFormatEtcList(void);
	int __fastcall Add(const tagFORMATETC &Item);
	void __fastcall Clear(void);
	void __fastcall Delete(int Index);
	TFormatEtcList* __fastcall Clone(void);
	__property int Count = {read=FCount, nodefault};
	__property tagFORMATETC Items[int Index] = {read=Get, write=Put};
};


class DELPHICLASS TDDInterfacedObject;
class PASCALIMPLEMENTATION TDDInterfacedObject : public System::TInterfacedObject 
{
	typedef System::TInterfacedObject inherited;
	
public:
	HIDESBASE HRESULT __stdcall QueryInterface(const GUID &IID, /* out */ void *Obj);
	HIDESBASE int __stdcall _AddRef(void);
	HIDESBASE int __stdcall _Release(void);
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TDDInterfacedObject(void) : System::TInterfacedObject() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TDDInterfacedObject(void) { }
	#pragma option pop
	
};


class DELPHICLASS TEnumFormatEtc;
class PASCALIMPLEMENTATION TEnumFormatEtc : public TDDInterfacedObject 
{
	typedef TDDInterfacedObject inherited;
	
protected:
	TFormatEtcList* FFormatEtcList;
	int FIndex;
	
public:
	__fastcall TEnumFormatEtc(TFormatEtcList* FormatEtcList);
	__fastcall virtual ~TEnumFormatEtc(void);
	HRESULT __stdcall Next(int celt, /* out */ void *elt, PLongint pceltFetched);
	HRESULT __stdcall Skip(int celt);
	HRESULT __stdcall Reset(void);
	HRESULT __stdcall Clone(/* out */ _di_IEnumFORMATETC &Enum);
private:
	void *__IEnumFORMATETC;	/* IEnumFORMATETC */
	
public:
	operator IEnumFORMATETC*(void) { return (IEnumFORMATETC*)&__IEnumFORMATETC; }
	
};


class DELPHICLASS TDataObject;
class PASCALIMPLEMENTATION TDataObject : public TDDInterfacedObject 
{
	typedef TDDInterfacedObject inherited;
	
protected:
	TFormatEtcList* FFormatEtcList;
	bool FCheckLindex;
	bool FCheckdwAspect;
	
public:
	__fastcall TDataObject(void);
	__fastcall virtual ~TDataObject(void);
	HRESULT __stdcall GetData(const tagFORMATETC &formatetcIn, /* out */ tagSTGMEDIUM &medium);
	HRESULT __stdcall GetDataHere(const tagFORMATETC &formatetc, /* out */ tagSTGMEDIUM &medium);
	HRESULT __stdcall QueryGetData(const tagFORMATETC &formatetc);
	HRESULT __stdcall GetCanonicalFormatEtc(const tagFORMATETC &formatetc, /* out */ tagFORMATETC &formatetcOut);
	HRESULT __stdcall SetData(const tagFORMATETC &formatetc, tagSTGMEDIUM &medium, BOOL fRelease);
	HRESULT __stdcall EnumFormatEtc(int dwDirection, /* out */ _di_IEnumFORMATETC &enumFormatEtc);
	HRESULT __stdcall DAdvise(const tagFORMATETC &formatetc, int advf, const _di_IAdviseSink advSink, /* out */ int &dwConnection);
	HRESULT __stdcall DUnadvise(int dwConnection);
	HRESULT __stdcall EnumDAdvise(/* out */ _di_IEnumSTATDATA &enumAdvise);
	virtual HRESULT __fastcall RenderData(const tagFORMATETC &FormatEtc, tagSTGMEDIUM &StgMedium) = 0 ;
	
protected:
	virtual bool __fastcall AllowData(const tagFORMATETC &FormatEtc);
private:
	void *__IDataObject;	/* IDataObject */
	
public:
	operator IDataObject*(void) { return (IDataObject*)&__IDataObject; }
	
};


class DELPHICLASS TDropSource;
class DELPHICLASS TDragDrop;
class DELPHICLASS TScrollDetectOptions;
class DELPHICLASS TScrollDetectArea;
class PASCALIMPLEMENTATION TScrollDetectArea : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;
	
private:
	Classes::TPersistent* FControl;
	Word FMargin;
	Word FRange;
	Classes::TNotifyEvent FOnChange;
	void __fastcall SetValue(int Index, Word Value);
	
protected:
	DYNAMIC void __fastcall Change(void);
	virtual void __fastcall AssignTo(Classes::TPersistent* Dest);
	__property Classes::TPersistent* Control = {read=FControl};
	
public:
	__fastcall TScrollDetectArea(Classes::TPersistent* Control);
	__property Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	
__published:
	__property Word Margin = {read=FMargin, write=SetValue, index=0, default=0};
	__property Word Range = {read=FRange, write=SetValue, index=1, default=10};
public:
	#pragma option push -w-inl
	/* TPersistent.Destroy */ inline __fastcall virtual ~TScrollDetectArea(void) { }
	#pragma option pop
	
};


class PASCALIMPLEMENTATION TScrollDetectOptions : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;
	
private:
	TDragDrop* FControl;
	TScrollInterval FScrollDelay;
	TScrollInterval FStartDelay;
	TScrollDetectArea* FLeft;
	TScrollDetectArea* FTop;
	TScrollDetectArea* FRight;
	TScrollDetectArea* FBottom;
	Classes::TNotifyEvent FOnChange;
	bool FHorzScrolling;
	bool FVertScrolling;
	bool FHorzPageScroll;
	bool FVertPageScroll;
	void __fastcall SetValue(int index, TScrollInterval Value);
	
protected:
	DYNAMIC void __fastcall Change(void);
	virtual void __fastcall AssignTo(Classes::TPersistent* Dest);
	__property TDragDrop* Control = {read=FControl};
	
public:
	__fastcall TScrollDetectOptions(TDragDrop* Control);
	__fastcall virtual ~TScrollDetectOptions(void);
	__property Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	
__published:
	__property TScrollInterval ScrollDelay = {read=FScrollDelay, write=SetValue, index=0, default=100};
	__property TScrollInterval StartDelay = {read=FStartDelay, write=SetValue, index=1, default=750};
	__property TScrollDetectArea* AreaLeft = {read=FLeft, write=FLeft};
	__property TScrollDetectArea* AreaTop = {read=FTop, write=FTop};
	__property TScrollDetectArea* AreaRight = {read=FRight, write=FRight};
	__property TScrollDetectArea* AreaBottom = {read=FBottom, write=FBottom};
	__property bool HorzScrolling = {read=FHorzScrolling, write=FHorzScrolling, default=0};
	__property bool VertScrolling = {read=FVertScrolling, write=FVertScrolling, default=0};
	__property bool HorzPageScroll = {read=FHorzPageScroll, write=FHorzPageScroll, default=0};
	__property bool VertPageScroll = {read=FVertPageScroll, write=FVertPageScroll, default=0};
};


class DELPHICLASS TDropTarget;
class PASCALIMPLEMENTATION TDropTarget : public TDDInterfacedObject 
{
	typedef TDDInterfacedObject inherited;
	
private:
	bool FAccept;
	Extctrls::TTimer* HorzStartTimer;
	Extctrls::TTimer* HorzScrollTimer;
	Extctrls::TTimer* VertStartTimer;
	Extctrls::TTimer* VertScrollTimer;
	int FVScrollCode;
	int FHScrollCode;
	void __fastcall InitScroll(bool VerticalScroll, int ScrollCode);
	void __fastcall TermScroll(bool VerticalScroll);
	void __fastcall DetermineScrollDir(bool VertScrolling, int &ScrollCode);
	void __fastcall OnStartTimer(System::TObject* Sender);
	void __fastcall OnScrollTimer(System::TObject* Sender);
	
protected:
	TDragDrop* FOwner;
	virtual void __fastcall SuggestDropEffect(int grfKeyState, int &dwEffect);
	virtual void __fastcall AcceptDataObject(_di_IDataObject DataObj, bool &Accept);
	virtual void __fastcall RenderDropped(_di_IDataObject DataObj, int grfKeyState, const Types::TPoint &pt, int &dwEffect);
	
public:
	__fastcall TDropTarget(TDragDrop* AOwner);
	__fastcall virtual ~TDropTarget(void);
	HRESULT __stdcall DragEnter(const _di_IDataObject dataObj, int grfKeyState, const Types::TPoint pt, int &dwEffect);
	HRESULT __stdcall DragOver(int grfKeyState, const Types::TPoint pt, int &dwEffect);
	HRESULT __stdcall DragLeave(void);
	HRESULT __stdcall Drop(const _di_IDataObject dataObj, int grfKeyState, const Types::TPoint pt, int &dwEffect);
private:
	void *__IDropTarget;	/* IDropTarget */
	
public:
	operator IDropTarget*(void) { return (IDropTarget*)&__IDropTarget; }
	
};


class PASCALIMPLEMENTATION TDragDrop : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	bool FAutoDetectDnD;
	Byte FDragDetectDelta;
	bool FAcceptOwnDnD;
	bool FBTF;
	bool FContextMenu;
	Controls::TWinControl* FDragDropControl;
	bool FRegistered;
	bool FOwnerIsSource;
	bool FShowPopUpMenu;
	TDropEffectSet FTargetEffectsSet;
	int FTargetEffects;
	TOnQueryContinueDrag FOnQueryContinueDrag;
	TOnGiveFeedback FOnGiveFeedback;
	TOnDragEnter FOnDragEnter;
	TOnDragLeave FOnDragLeave;
	TOnDragOver FOnDragOver;
	TOnDrop FOnDrop;
	TDropEffectSet FSourceEffectsSet;
	int FSourceEffects;
	TOnProcessDropped FOnProcessDropped;
	void *OldWndProc;
	void *WndProcPtr;
	TOnDragDetect FOnDragDetect;
	TDragDetectStatus FDragDetectStatus;
	#pragma pack(push, 1)
	Types::TPoint FDragDetectStart;
	#pragma pack(pop)
	
	TRenderDataOn FRenderDataOn;
	_di_IDataObject FDataObj;
	int FgrfKeyState;
	#pragma pack(push, 1)
	Types::TPoint Fpt;
	#pragma pack(pop)
	
	int FdwEffect;
	HICON FCHCopy;
	HICON FCHMove;
	HICON FCHLink;
	HICON FCHScrollCopy;
	HICON FCHScrollMove;
	HICON FCHScrollLink;
	bool FMessageHooked;
	int FAvailableDropEffects;
	int FTargetScrolling;
	TSrcCompatibilityCheckSet FSrcCompatibilityCheck;
	TScrollDetectOptions* FScrollDetectOptions;
	TOnBeforeScrolling FOnBeforeScrolling;
	Classes::TNotifyEvent FOnAfterScrolling;
	int FPressedButton;
	TDragDrop* FInternalSource;
	TOnMenuPopup FOnMenuPopup;
	TOnMenuExecCmd FOnMenuExecCmd;
	TOnMenuDestroy FOnMenuDestroy;
	TOnProcessDropped FOnMenuSucceeded;
	TOnProcessDropped FOnDropHandlerSucceeded;
	void __fastcall WndMethod(Messages::TMessage &Msg);
	void __fastcall SetDragDropControl(Controls::TWinControl* WinControl);
	void __fastcall SetSourceEffects(TDropEffectSet Values);
	void __fastcall SetTargetEffects(TDropEffectSet Values);
	
protected:
	TDropTarget* FDropTarget;
	virtual void __fastcall Loaded(void);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	virtual TDataObject* __fastcall CreateDataObject(void) = 0 ;
	virtual void __fastcall DoMenuPopup(System::TObject* Sender, HMENU AMenu, _di_IDataObject DataObj, int AMinCustCmd, int grfKeyState, const Types::TPoint &pt);
	virtual bool __fastcall DoMenuExecCmd(System::TObject* Sender, HMENU AMenu, _di_IDataObject DataObj, int Command, int &dwEffect);
	virtual void __fastcall DoMenuDestroy(System::TObject* Sender, HMENU AMenu);
	virtual bool __fastcall DropHandler(const _di_IDataObject dataObj, int grfKeyState, const Types::TPoint &pt, int &dwEffect);
	__property TOnProcessDropped OnDropHandlerSucceeded = {read=FOnDropHandlerSucceeded, write=FOnDropHandlerSucceeded};
	
public:
	__fastcall virtual TDragDrop(Classes::TComponent* AOwner);
	__fastcall virtual ~TDragDrop(void);
	bool __fastcall RegisterTarget(void);
	bool __fastcall UnRegisterTarget(void);
	void __fastcall HookMessageHandler(void);
	void __fastcall UnhookMessageHandler(bool ForceUnhook);
	TDragResult __fastcall ExecuteOperation(TDataObject* DataObject);
	TDragResult __fastcall Execute(void);
	virtual bool __fastcall CopyToClipboard(void);
	virtual bool __fastcall GetFromClipboard(void);
	virtual void __fastcall StartDnDDetection(Controls::TMouseButton Button);
	__property bool OwnerIsSource = {read=FOwnerIsSource, nodefault};
	__property bool Registered = {read=FRegistered, default=0};
	__property HICON CHCopy = {read=FCHCopy, write=FCHCopy, default=0};
	__property HICON CHMove = {read=FCHMove, write=FCHMove, default=0};
	__property HICON CHLink = {read=FCHLink, write=FCHLink, default=0};
	__property HICON CHScrollCopy = {read=FCHScrollCopy, write=FCHScrollCopy, default=0};
	__property HICON CHScrollMove = {read=FCHScrollMove, write=FCHScrollMove, default=0};
	__property HICON CHScrollLink = {read=FCHScrollLink, write=FCHScrollLink, default=0};
	__property TDragDetectStatus DragDetectStatus = {read=FDragDetectStatus, nodefault};
	__property int AvailableDropEffects = {read=FAvailableDropEffects, nodefault};
	__property TDragDrop* InternalSource = {read=FInternalSource};
	
__published:
	__property bool AcceptOwnDnD = {read=FAcceptOwnDnD, write=FAcceptOwnDnD, nodefault};
	__property bool AutoDetectDnD = {read=FAutoDetectDnD, write=FAutoDetectDnD, nodefault};
	__property bool BringToFront = {read=FBTF, write=FBTF, nodefault};
	__property Byte DragDetectDelta = {read=FDragDetectDelta, write=FDragDetectDelta, default=10};
	__property Controls::TWinControl* DragDropControl = {read=FDragDropControl, write=SetDragDropControl};
	__property TRenderDataOn RenderDataOn = {read=FRenderDataOn, write=FRenderDataOn, default=3};
	__property TScrollDetectOptions* ScrollDetectOptions = {read=FScrollDetectOptions, write=FScrollDetectOptions};
	__property TSrcCompatibilityCheckSet SourceCompatibility = {read=FSrcCompatibilityCheck, write=FSrcCompatibilityCheck, nodefault};
	__property TDropEffectSet SourceEffects = {read=FSourceEffectsSet, write=SetSourceEffects, nodefault};
	__property bool TargetPopupMenu = {read=FShowPopUpMenu, write=FShowPopUpMenu, nodefault};
	__property TDropEffectSet TargetEffects = {read=FTargetEffectsSet, write=SetTargetEffects, nodefault};
	__property Classes::TNotifyEvent OnAfterScrolling = {read=FOnAfterScrolling, write=FOnAfterScrolling};
	__property TOnBeforeScrolling OnBeforeScrolling = {read=FOnBeforeScrolling, write=FOnBeforeScrolling};
	__property TOnDragDetect OnDragDetect = {read=FOnDragDetect, write=FOnDragDetect};
	__property TOnDragEnter OnDragEnter = {read=FOnDragEnter, write=FOnDragEnter};
	__property TOnDragLeave OnDragLeave = {read=FOnDragLeave, write=FOnDragLeave};
	__property TOnDragOver OnDragOver = {read=FOnDragOver, write=FOnDragOver};
	__property TOnDrop OnDrop = {read=FOnDrop, write=FOnDrop};
	__property TOnQueryContinueDrag OnQueryContinueDrag = {read=FOnQueryContinueDrag, write=FOnQueryContinueDrag};
	__property TOnGiveFeedback OnGiveFeedback = {read=FOnGiveFeedback, write=FOnGiveFeedback};
	__property TOnProcessDropped OnProcessDropped = {read=FOnProcessDropped, write=FOnProcessDropped};
	__property TOnMenuPopup OnMenuPopup = {read=FOnMenuPopup, write=FOnMenuPopup};
	__property TOnMenuExecCmd OnMenuExecCmd = {read=FOnMenuExecCmd, write=FOnMenuExecCmd};
	__property TOnMenuDestroy OnMenuDestroy = {read=FOnMenuDestroy, write=FOnMenuDestroy};
	__property TOnProcessDropped OnMenuSucceeded = {read=FOnMenuSucceeded, write=FOnMenuSucceeded};
};


class PASCALIMPLEMENTATION TDropSource : public TDDInterfacedObject 
{
	typedef TDDInterfacedObject inherited;
	
private:
	TDragDrop* FOwner;
	
public:
	__fastcall TDropSource(TDragDrop* AOwner);
	__fastcall virtual ~TDropSource(void);
	HRESULT __stdcall QueryContinueDrag(BOOL fEscapePressed, int grfKeyState);
	HRESULT __stdcall GiveFeedback(int dwEffect);
private:
	void *__IDropSource;	/* IDropSource */
	
public:
	operator IDropSource*(void) { return (IDropSource*)&__IDropSource; }
	
};


//-- var, const, procedure ---------------------------------------------------
static const Shortint DROPEFFECT_None = 0x0;
static const Shortint DROPEFFECT_Copy = 0x1;
static const Shortint DROPEFFECT_Move = 0x2;
static const Shortint DROPEFFECT_Link = 0x4;
static const unsigned DROPEFFECT_Scroll = 0x80000000;
static const Shortint TYMED_HGLOBAL = 0x1;
static const Shortint TYMED_FILE = 0x2;
static const Shortint TYMED_ISTREAM = 0x4;
static const Shortint TYMED_ISTORAGE = 0x8;
static const Shortint TYMED_GDI = 0x10;
static const Shortint TYMED_MFPICT = 0x20;
static const Shortint TYMED_ENHMF = 0x40;
static const Shortint TYMED_NULL = 0x0;
static const Shortint DefaultCursor = 0x0;
extern PACKAGE System::ResourceString _MICopyStr;
#define Dragdrop_MICopyStr System::LoadResourceString(&Dragdrop::_MICopyStr)
extern PACKAGE System::ResourceString _MIMoveStr;
#define Dragdrop_MIMoveStr System::LoadResourceString(&Dragdrop::_MIMoveStr)
extern PACKAGE System::ResourceString _MILinkStr;
#define Dragdrop_MILinkStr System::LoadResourceString(&Dragdrop::_MILinkStr)
extern PACKAGE System::ResourceString _MIAbortStr;
#define Dragdrop_MIAbortStr System::LoadResourceString(&Dragdrop::_MIAbortStr)
extern PACKAGE void __fastcall Register(void);

}	/* namespace Dragdrop */
using namespace Dragdrop;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DragDrop
