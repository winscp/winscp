// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TB2ExtItems.pas' rev: 6.00

#ifndef TB2ExtItemsHPP
#define TB2ExtItemsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ImgList.hpp>	// Pascal unit
#include <TB2Item.hpp>	// Pascal unit
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

namespace Tb2extitems
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TTBEditItemOption { tboUseEditWhenVertical };
#pragma option pop

typedef Set<TTBEditItemOption, tboUseEditWhenVertical, tboUseEditWhenVertical>  TTBEditItemOptions;

typedef void __fastcall (__closure *TTBAcceptTextEvent)(System::TObject* Sender, AnsiString &NewText, bool &Accept);

class DELPHICLASS TTBEditItem;
class DELPHICLASS TTBEditItemViewer;
typedef void __fastcall (__closure *TTBBeginEditEvent)(TTBEditItem* Sender, TTBEditItemViewer* Viewer, Stdctrls::TEdit* EditControl);

class DELPHICLASS TTBEditAction;
class PASCALIMPLEMENTATION TTBEditAction : public Actnlist::TAction 
{
	typedef Actnlist::TAction inherited;
	
private:
	TTBEditItemOptions FEditOptions;
	AnsiString FEditCaption;
	int FEditWidth;
	TTBAcceptTextEvent FOnAcceptText;
	AnsiString FText;
	void __fastcall SetEditCaption(AnsiString Value);
	void __fastcall SetEditOptions(TTBEditItemOptions Value);
	void __fastcall SetEditWidth(int Value);
	void __fastcall SetOnAcceptText(TTBAcceptTextEvent Value);
	void __fastcall SetText(AnsiString Value);
	
public:
	__fastcall virtual TTBEditAction(Classes::TComponent* AOwner);
	
__published:
	__property AnsiString EditCaption = {read=FEditCaption, write=SetEditCaption};
	__property TTBEditItemOptions EditOptions = {read=FEditOptions, write=SetEditOptions, default=0};
	__property int EditWidth = {read=FEditWidth, write=SetEditWidth, default=64};
	__property AnsiString Text = {read=FText, write=SetText};
	__property TTBAcceptTextEvent OnAcceptText = {read=FOnAcceptText, write=SetOnAcceptText};
public:
	#pragma option push -w-inl
	/* TCustomAction.Destroy */ inline __fastcall virtual ~TTBEditAction(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBEditItemActionLink;
class PASCALIMPLEMENTATION TTBEditItemActionLink : public Tb2item::TTBCustomItemActionLink 
{
	typedef Tb2item::TTBCustomItemActionLink inherited;
	
protected:
	virtual void __fastcall AssignClient(System::TObject* AClient);
	virtual bool __fastcall IsEditCaptionLinked(void);
	virtual bool __fastcall IsEditOptionsLinked(void);
	virtual bool __fastcall IsEditWidthLinked(void);
	virtual bool __fastcall IsOnAcceptTextLinked(void);
	virtual bool __fastcall IsTextLinked(void);
	virtual void __fastcall SetEditCaption(const AnsiString Value);
	virtual void __fastcall SetEditOptions(TTBEditItemOptions Value);
	virtual void __fastcall SetEditWidth(const int Value);
	virtual void __fastcall SetOnAcceptText(TTBAcceptTextEvent Value);
	virtual void __fastcall SetText(const AnsiString Value);
public:
	#pragma option push -w-inl
	/* TBasicActionLink.Create */ inline __fastcall virtual TTBEditItemActionLink(System::TObject* AClient) : Tb2item::TTBCustomItemActionLink(AClient) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TBasicActionLink.Destroy */ inline __fastcall virtual ~TTBEditItemActionLink(void) { }
	#pragma option pop
	
};


class PASCALIMPLEMENTATION TTBEditItem : public Tb2item::TTBCustomItem 
{
	typedef Tb2item::TTBCustomItem inherited;
	
private:
	Stdctrls::TEditCharCase FCharCase;
	AnsiString FEditCaption;
	TTBEditItemOptions FEditOptions;
	int FEditWidth;
	bool FExtendedAccept;
	int FMaxLength;
	TTBAcceptTextEvent FOnAcceptText;
	TTBBeginEditEvent FOnBeginEdit;
	AnsiString FText;
	bool __fastcall IsEditCaptionStored(void);
	bool __fastcall IsEditOptionsStored(void);
	bool __fastcall IsEditWidthStored(void);
	bool __fastcall IsTextStored(void);
	void __fastcall SetCharCase(Stdctrls::TEditCharCase Value);
	void __fastcall SetEditCaption(AnsiString Value);
	void __fastcall SetEditOptions(TTBEditItemOptions Value);
	void __fastcall SetEditWidth(int Value);
	void __fastcall SetMaxLength(int Value);
	void __fastcall SetText(AnsiString Value);
	
protected:
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	virtual bool __fastcall DoAcceptText(AnsiString &NewText);
	virtual void __fastcall DoBeginEdit(TTBEditItemViewer* Viewer);
	virtual void __fastcall DoTextChanging(const AnsiString OldText, AnsiString &NewText, int Reason);
	virtual void __fastcall DoTextChanged(int Reason);
	DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
	virtual TMetaClass* __fastcall GetItemViewerClass(Tb2item::TTBView* AView);
	virtual bool __fastcall NeedToRecreateViewer(Tb2item::TTBItemViewer* AViewer);
	__property bool ExtendedAccept = {read=FExtendedAccept, write=FExtendedAccept, default=0};
	void __fastcall SetTextEx(AnsiString Value, int Reason);
	
public:
	__fastcall virtual TTBEditItem(Classes::TComponent* AOwner);
	HIDESBASE void __fastcall Clear(void);
	virtual void __fastcall Click(void);
	
__published:
	__property Action ;
	__property AutoCheck  = {default=0};
	__property Caption ;
	__property Stdctrls::TEditCharCase CharCase = {read=FCharCase, write=SetCharCase, default=0};
	__property Checked  = {default=0};
	__property DisplayMode  = {default=0};
	__property AnsiString EditCaption = {read=FEditCaption, write=SetEditCaption, stored=IsEditCaptionStored};
	__property TTBEditItemOptions EditOptions = {read=FEditOptions, write=SetEditOptions, stored=IsEditOptionsStored, nodefault};
	__property int EditWidth = {read=FEditWidth, write=SetEditWidth, stored=IsEditWidthStored, nodefault};
	__property int MaxLength = {read=FMaxLength, write=SetMaxLength, default=0};
	__property Enabled  = {default=1};
	__property GroupIndex  = {default=0};
	__property HelpContext  = {default=0};
	__property HelpKeyword ;
	__property Hint ;
	__property ImageIndex  = {default=-1};
	__property RadioItem  = {default=0};
	__property ShortCut  = {default=0};
	__property AnsiString Text = {read=FText, write=SetText, stored=IsTextStored};
	__property Visible  = {default=1};
	__property TTBAcceptTextEvent OnAcceptText = {read=FOnAcceptText, write=FOnAcceptText};
	__property TTBBeginEditEvent OnBeginEdit = {read=FOnBeginEdit, write=FOnBeginEdit};
	__property OnClick ;
	__property OnSelect ;
public:
	#pragma option push -w-inl
	/* TTBCustomItem.Destroy */ inline __fastcall virtual ~TTBEditItem(void) { }
	#pragma option pop
	
};


typedef TMetaClass*TEditClass;

#pragma option push -b-
enum TB2ExtItems__5 { ecsContinueLoop, ecsAccept, ecsClose };
#pragma option pop

class PASCALIMPLEMENTATION TTBEditItemViewer : public Tb2item::TTBItemViewer 
{
	typedef Tb2item::TTBItemViewer inherited;
	
private:
	Stdctrls::TEdit* FEditControl;
	System::Set<TB2ExtItems__5, ecsContinueLoop, ecsClose>  FEditControlStatus;
	bool __fastcall EditLoop(const HWND CapHandle);
	void __fastcall EditWndProc(Messages::TMessage &Message);
	void __fastcall MouseBeginEdit(void);
	
protected:
	virtual void __fastcall CalcSize(const Graphics::TCanvas* Canvas, int &AWidth, int &AHeight);
	DYNAMIC bool __fastcall CaptionShown(void);
	virtual bool __fastcall DoExecute(void);
	virtual int __fastcall GetAccRole(void);
	virtual bool __fastcall GetAccValue(WideString &Value);
	virtual AnsiString __fastcall GetCaptionText();
	virtual void __fastcall GetCursor(const Types::TPoint &Pt, HICON &ACursor);
	virtual TMetaClass* __fastcall GetEditControlClass(void);
	virtual void __fastcall GetEditRect(Types::TRect &R);
	virtual void __fastcall MouseDown(Classes::TShiftState Shift, int X, int Y, bool &MouseDownOnMenu);
	virtual void __fastcall MouseUp(int X, int Y, bool MouseWasDownOnMenu);
	virtual void __fastcall Paint(const Graphics::TCanvas* Canvas, const Types::TRect &ClientAreaRect, bool IsSelected, bool IsPushed, bool UseDisabledShadow);
	virtual bool __fastcall UsesSameWidth(void);
	
public:
	__property Stdctrls::TEdit* EditControl = {read=FEditControl};
public:
	#pragma option push -w-inl
	/* TTBItemViewer.Create */ inline __fastcall virtual TTBEditItemViewer(Tb2item::TTBView* AView, Tb2item::TTBCustomItem* AItem, int AGroupLevel) : Tb2item::TTBItemViewer(AView, AItem, AGroupLevel) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBItemViewer.Destroy */ inline __fastcall virtual ~TTBEditItemViewer(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
#define EditItemDefaultEditOptions EMPTYSET
static const Shortint EditItemDefaultEditWidth = 0x40;
static const Shortint tcrSetProperty = 0x0;
static const Shortint tcrActionLink = 0x1;
static const Shortint tcrEditControl = 0x2;

}	/* namespace Tb2extitems */
using namespace Tb2extitems;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TB2ExtItems
