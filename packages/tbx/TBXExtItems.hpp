// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TBXExtItems.pas' rev: 6.00

#ifndef TBXExtItemsHPP
#define TBXExtItemsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ImgList.hpp>	// Pascal unit
#include <TBXLists.hpp>	// Pascal unit
#include <TB2ExtItems.hpp>	// Pascal unit
#include <TB2Toolbar.hpp>	// Pascal unit
#include <TB2Item.hpp>	// Pascal unit
#include <TBXThemes.hpp>	// Pascal unit
#include <TBX.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tbxextitems
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TTBXEditChange)(System::TObject* Sender, const AnsiString Text);

class DELPHICLASS TTBXEditItem;
class PASCALIMPLEMENTATION TTBXEditItem : public Tb2extitems::TTBEditItem 
{
	typedef Tb2extitems::TTBEditItem inherited;
	
private:
	Classes::TAlignment FAlignment;
	int FAutoCompleteCounter;
	Tbx::TFontSettings* FEditorFontSettings;
	Tbx::TFontSettings* FFontSettings;
	bool FIsChanging;
	AnsiString FLastEditChange;
	char FPasswordChar;
	bool FReadOnly;
	bool FShowImage;
	TTBXEditChange FOnChange;
	void __fastcall FontSettingsChanged(System::TObject* Sender);
	void __fastcall SetAlignment(Classes::TAlignment Value);
	void __fastcall SetPasswordChar(char Value);
	void __fastcall SetShowImage(const bool Value);
	void __fastcall SetFontSettings(Tbx::TFontSettings* Value);
	
protected:
	virtual bool __fastcall DoAcceptText(AnsiString &NewText);
	virtual bool __fastcall DoAutoComplete(AnsiString &AText);
	virtual void __fastcall DoBeginEdit(Tb2extitems::TTBEditItemViewer* Viewer);
	virtual void __fastcall DoChange(const AnsiString AText);
	virtual void __fastcall DoTextChanged(int Reason);
	virtual int __fastcall GetImageIndex(void);
	virtual TMetaClass* __fastcall GetItemViewerClass(Tb2item::TTBView* AView);
	virtual void __fastcall GetPopupPosition(Tb2item::TTBView* ParentView, Tb2item::TTBPopupWindow* PopupWindow, Tb2item::TTBPopupPositionRec &PopupPositionRec);
	virtual TMetaClass* __fastcall GetPopupWindowClass(void);
	virtual void __fastcall HandleEditChange(Stdctrls::TEdit* Edit);
	
public:
	bool __fastcall StartEditing(Tb2item::TTBView* AView);
	__fastcall virtual TTBXEditItem(Classes::TComponent* AOwner);
	__fastcall virtual ~TTBXEditItem(void);
	
__published:
	__property Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
	__property Tbx::TFontSettings* EditorFontSettings = {read=FEditorFontSettings, write=FEditorFontSettings};
	__property ExtendedAccept  = {default=0};
	__property Tbx::TFontSettings* FontSettings = {read=FFontSettings, write=SetFontSettings};
	__property ImageIndex  = {default=-1};
	__property Images ;
	__property char PasswordChar = {read=FPasswordChar, write=SetPasswordChar, default=0};
	__property bool ReadOnly = {read=FReadOnly, write=FReadOnly, default=0};
	__property bool ShowImage = {read=FShowImage, write=SetShowImage, default=0};
	__property TTBXEditChange OnChange = {read=FOnChange, write=FOnChange};
	__property OnSelect ;
};


class DELPHICLASS TTBXEditItemViewer;
class PASCALIMPLEMENTATION TTBXEditItemViewer : public Tb2extitems::TTBEditItemViewer 
{
	typedef Tb2extitems::TTBEditItemViewer inherited;
	
private:
	void __fastcall EditChangeHandler(System::TObject* Sender);
	tagSIZE __fastcall MeasureEditCaption();
	int __fastcall MeasureTextHeight(void);
	void __fastcall HandleEditChange(Stdctrls::TEdit* Edit);
	
protected:
	Classes::TWndMethod OldWndProc;
	virtual void __fastcall CalcSize(const Graphics::TCanvas* Canvas, int &AWidth, int &AHeight);
	virtual bool __fastcall DoExecute(void);
	virtual bool __fastcall HandleEditMessage(Messages::TMessage &Message);
	virtual int __fastcall GetAccRole(void);
	virtual void __fastcall GetItemInfo(/* out */ Tbxthemes::TTBXItemInfo &ItemInfo, bool IsHoverItem, bool IsPushed, bool UseMenuColor);
	virtual TMetaClass* __fastcall GetEditControlClass(void);
	virtual void __fastcall GetEditInfo(/* out */ Tbxthemes::TTBXEditInfo &EditInfo, const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual int __fastcall GetIndentBefore(void);
	virtual int __fastcall GetIndentAfter(void);
	virtual void __fastcall GetEditRect(Types::TRect &R);
	virtual bool __fastcall IsToolbarSize(void);
	void __fastcall NewEditWndProc(Messages::TMessage &Message);
	virtual void __fastcall Paint(const Graphics::TCanvas* Canvas, const Types::TRect &ClientAreaRect, bool IsHoverItem, bool IsPushed, bool UseDisabledShadow);
	virtual bool __fastcall ShowImage(void);
	
public:
	virtual bool __fastcall IsToolbarStyle(void);
public:
	#pragma option push -w-inl
	/* TTBItemViewer.Create */ inline __fastcall virtual TTBXEditItemViewer(Tb2item::TTBView* AView, Tb2item::TTBCustomItem* AItem, int AGroupLevel) : Tb2extitems::TTBEditItemViewer(AView, AItem, AGroupLevel) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBItemViewer.Destroy */ inline __fastcall virtual ~TTBXEditItemViewer(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXCustomDropDownItem;
class PASCALIMPLEMENTATION TTBXCustomDropDownItem : public TTBXEditItem 
{
	typedef TTBXEditItem inherited;
	
private:
	bool FAlwaysSelectFirst;
	bool FDropDownList;
	
protected:
	virtual Tb2item::TTBPopupWindow* __fastcall CreatePopup(const Tb2item::TTBView* ParentView, const Tb2item::TTBItemViewer* ParentViewer, const bool PositionAsSubmenu, const bool SelectFirstItem, const bool Customizing, const Types::TPoint &APopupPoint, const Tb2item::TTBPopupAlignment Alignment);
	virtual TMetaClass* __fastcall GetItemViewerClass(Tb2item::TTBView* AView);
	
public:
	__fastcall virtual TTBXCustomDropDownItem(Classes::TComponent* AOwner);
	__property bool AlwaysSelectFirst = {read=FAlwaysSelectFirst, write=FAlwaysSelectFirst, default=1};
	__property bool DropDownList = {read=FDropDownList, write=FDropDownList, default=0};
public:
	#pragma option push -w-inl
	/* TTBXEditItem.Destroy */ inline __fastcall virtual ~TTBXCustomDropDownItem(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXDropDownItem;
class PASCALIMPLEMENTATION TTBXDropDownItem : public TTBXCustomDropDownItem 
{
	typedef TTBXCustomDropDownItem inherited;
	
__published:
	__property AlwaysSelectFirst  = {default=1};
	__property DropDownList  = {default=0};
	__property LinkSubitems ;
	__property SubMenuImages ;
public:
	#pragma option push -w-inl
	/* TTBXCustomDropDownItem.Create */ inline __fastcall virtual TTBXDropDownItem(Classes::TComponent* AOwner) : TTBXCustomDropDownItem(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TTBXEditItem.Destroy */ inline __fastcall virtual ~TTBXDropDownItem(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXDropDownItemViewer;
class PASCALIMPLEMENTATION TTBXDropDownItemViewer : public TTBXEditItemViewer 
{
	typedef TTBXEditItemViewer inherited;
	
protected:
	virtual void __fastcall GetCursor(const Types::TPoint &Pt, HICON &ACursor);
	virtual void __fastcall GetEditInfo(/* out */ Tbxthemes::TTBXEditInfo &EditInfo, const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual int __fastcall GetIndentAfter(void);
	virtual bool __fastcall HandleEditMessage(Messages::TMessage &Message);
	virtual bool __fastcall IsPtInButtonPart(int X, int Y);
	virtual void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
public:
	#pragma option push -w-inl
	/* TTBItemViewer.Create */ inline __fastcall virtual TTBXDropDownItemViewer(Tb2item::TTBView* AView, Tb2item::TTBCustomItem* AItem, int AGroupLevel) : TTBXEditItemViewer(AView, AItem, AGroupLevel) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBItemViewer.Destroy */ inline __fastcall virtual ~TTBXDropDownItemViewer(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXComboBoxItem;
typedef void __fastcall (__closure *TTBXCAdjustImageIndex)(TTBXComboBoxItem* Sender, const AnsiString AText, int AIndex, int &ImageIndex);

class PASCALIMPLEMENTATION TTBXComboBoxItem : public TTBXCustomDropDownItem 
{
	typedef TTBXCustomDropDownItem inherited;
	
private:
	bool FAutoComplete;
	Tbxlists::TTBXStringList* FList;
	Classes::TNotifyEvent FOnItemClick;
	TTBXCAdjustImageIndex FOnAdjustImageIndex;
	void __fastcall AdjustImageIndexHandler(Tbxlists::TTBXCustomList* Sender, int AItemIndex, int &ImageIndex);
	int __fastcall GetItemIndex(void);
	int __fastcall GetMaxVisibleItems(void);
	int __fastcall GetMaxWidth(void);
	int __fastcall GetMinWidth(void);
	Classes::TStrings* __fastcall GetStrings(void);
	bool __fastcall GetShowListImages(void);
	Tbxlists::TTBXLPaintEvent __fastcall GetOnClearItem();
	Tbxlists::TTBXLPaintEvent __fastcall GetOnDrawItem();
	Tbxlists::TTBXLMeasureHeight __fastcall GetOnMeasureHeight();
	Tbxlists::TTBXLMeasureWidth __fastcall GetOnMeasureWidth();
	void __fastcall ListChangeHandler(System::TObject* Sender);
	void __fastcall ListClickHandler(System::TObject* Sender);
	void __fastcall SetItemIndex(int Value);
	void __fastcall SetMaxVisibleItems(int Value);
	void __fastcall SetMaxWidth(int Value);
	void __fastcall SetMinWidth(int Value);
	void __fastcall SetOnClearItem(Tbxlists::TTBXLPaintEvent Value);
	void __fastcall SetOnDrawItem(Tbxlists::TTBXLPaintEvent Value);
	void __fastcall SetOnMeasureHeight(Tbxlists::TTBXLMeasureHeight Value);
	void __fastcall SetOnMeasureWidth(Tbxlists::TTBXLMeasureWidth Value);
	void __fastcall SetStrings(Classes::TStrings* Value);
	void __fastcall SetShowListImages(bool Value);
	
protected:
	int CachedImageIndex;
	bool CacheValid;
	bool IsChanging;
	virtual void __fastcall AdjustImageIndex(const AnsiString AText, int AIndex, int &ImageIndex);
	virtual bool __fastcall DoAutoComplete(AnsiString &AText);
	virtual void __fastcall DoListChange(void);
	virtual void __fastcall DoListClick(void);
	virtual void __fastcall DoPopup(Tb2item::TTBCustomItem* Sender, bool FromLink);
	virtual int __fastcall GetImageIndex(void);
	virtual TMetaClass* __fastcall GetItemViewerClass(Tb2item::TTBView* AView);
	virtual TMetaClass* __fastcall GetStringListClass(void);
	virtual void __fastcall HandleEditChange(Stdctrls::TEdit* Edit);
	
public:
	__fastcall virtual TTBXComboBoxItem(Classes::TComponent* AOwner);
	virtual void __fastcall Loaded(void);
	__property int ItemIndex = {read=GetItemIndex, write=SetItemIndex, default=-1};
	
__published:
	__property bool AutoComplete = {read=FAutoComplete, write=FAutoComplete, default=1};
	__property DropDownList  = {default=0};
	__property int MaxListWidth = {read=GetMaxWidth, write=SetMaxWidth, default=0};
	__property int MaxVisibleItems = {read=GetMaxVisibleItems, write=SetMaxVisibleItems, default=8};
	__property int MinListWidth = {read=GetMinWidth, write=SetMinWidth, default=64};
	__property bool ShowListImages = {read=GetShowListImages, write=SetShowListImages, default=0};
	__property Classes::TStrings* Strings = {read=GetStrings, write=SetStrings};
	__property SubMenuImages ;
	__property OnChange ;
	__property TTBXCAdjustImageIndex OnAdjustImageIndex = {read=FOnAdjustImageIndex, write=FOnAdjustImageIndex};
	__property Tbxlists::TTBXLPaintEvent OnClearItem = {read=GetOnClearItem, write=SetOnClearItem};
	__property Tbxlists::TTBXLPaintEvent OnDrawItem = {read=GetOnDrawItem, write=SetOnDrawItem};
	__property Classes::TNotifyEvent OnItemClick = {read=FOnItemClick, write=FOnItemClick};
	__property Tbxlists::TTBXLMeasureHeight OnMeasureHeight = {read=GetOnMeasureHeight, write=SetOnMeasureHeight};
	__property Tbxlists::TTBXLMeasureWidth OnMeasureWidth = {read=GetOnMeasureWidth, write=SetOnMeasureWidth};
	__property OnPopup ;
public:
	#pragma option push -w-inl
	/* TTBXEditItem.Destroy */ inline __fastcall virtual ~TTBXComboBoxItem(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXComboBoxItemViewer;
class PASCALIMPLEMENTATION TTBXComboBoxItemViewer : public TTBXDropDownItemViewer 
{
	typedef TTBXDropDownItemViewer inherited;
	
protected:
	virtual bool __fastcall HandleEditMessage(Messages::TMessage &Message);
public:
	#pragma option push -w-inl
	/* TTBItemViewer.Create */ inline __fastcall virtual TTBXComboBoxItemViewer(Tb2item::TTBView* AView, Tb2item::TTBCustomItem* AItem, int AGroupLevel) : TTBXDropDownItemViewer(AView, AItem, AGroupLevel) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBItemViewer.Destroy */ inline __fastcall virtual ~TTBXComboBoxItemViewer(void) { }
	#pragma option pop
	
};


#pragma option push -b-
enum TTBXLabelOrientation { tbxoAuto, tbxoHorizontal, tbxoVertical };
#pragma option pop

typedef int TNonNegativeInt;

class DELPHICLASS TTBXLabelItem;
class PASCALIMPLEMENTATION TTBXLabelItem : public Tb2item::TTBCustomItem 
{
	typedef Tb2item::TTBCustomItem inherited;
	
private:
	AnsiString FCaption;
	Tbx::TFontSettings* FFontSettings;
	int FMargin;
	bool FShowAccelChar;
	TTBXLabelOrientation FOrientation;
	Tbx::TAdjustFontEvent FOnAdjustFont;
	void __fastcall FontSettingsChanged(System::TObject* Sender);
	void __fastcall SetMargin(int Value);
	void __fastcall SetOrientation(TTBXLabelOrientation Value);
	HIDESBASE void __fastcall SetCaption(const AnsiString Value);
	void __fastcall SetFontSettings(Tbx::TFontSettings* Value);
	void __fastcall SetShowAccelChar(bool Value);
	
protected:
	virtual TMetaClass* __fastcall GetItemViewerClass(Tb2item::TTBView* AView);
	
public:
	__fastcall virtual TTBXLabelItem(Classes::TComponent* AOwner);
	__fastcall virtual ~TTBXLabelItem(void);
	void __fastcall UpdateCaption(const AnsiString Value);
	
__published:
	__property AnsiString Caption = {read=FCaption, write=SetCaption};
	__property Enabled  = {default=1};
	__property Tbx::TFontSettings* FontSettings = {read=FFontSettings, write=SetFontSettings};
	__property int Margin = {read=FMargin, write=SetMargin, default=0};
	__property TTBXLabelOrientation Orientation = {read=FOrientation, write=SetOrientation, default=0};
	__property bool ShowAccelChar = {read=FShowAccelChar, write=SetShowAccelChar, default=1};
	__property Visible  = {default=1};
	__property Tbx::TAdjustFontEvent OnAdjustFont = {read=FOnAdjustFont, write=FOnAdjustFont};
};


class DELPHICLASS TTBXLabelItemViewer;
class PASCALIMPLEMENTATION TTBXLabelItemViewer : public Tb2item::TTBItemViewer 
{
	typedef Tb2item::TTBItemViewer inherited;
	
protected:
	virtual AnsiString __fastcall GetCaptionText();
	virtual bool __fastcall GetIsHoriz(void);
	virtual void __fastcall DoAdjustFont(Graphics::TFont* AFont, int StateFlags);
	virtual void __fastcall CalcSize(const Graphics::TCanvas* Canvas, int &AWidth, int &AHeight);
	virtual void __fastcall Paint(const Graphics::TCanvas* Canvas, const Types::TRect &ClientAreaRect, bool IsHoverItem, bool IsPushed, bool UseDisabledShadow);
	virtual bool __fastcall IsToolbarSize(void);
	
public:
	virtual bool __fastcall IsToolbarStyle(void);
public:
	#pragma option push -w-inl
	/* TTBItemViewer.Create */ inline __fastcall virtual TTBXLabelItemViewer(Tb2item::TTBView* AView, Tb2item::TTBCustomItem* AItem, int AGroupLevel) : Tb2item::TTBItemViewer(AView, AItem, AGroupLevel) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBItemViewer.Destroy */ inline __fastcall virtual ~TTBXLabelItemViewer(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXColorItem;
class PASCALIMPLEMENTATION TTBXColorItem : public Tbx::TTBXCustomItem 
{
	typedef Tbx::TTBXCustomItem inherited;
	
private:
	Graphics::TColor FColor;
	void __fastcall SetColor(Graphics::TColor Value);
	
protected:
	virtual TMetaClass* __fastcall GetItemViewerClass(Tb2item::TTBView* AView);
	
public:
	__fastcall virtual TTBXColorItem(Classes::TComponent* AOwner);
	
__published:
	__property Action ;
	__property AutoCheck  = {default=0};
	__property Caption ;
	__property Checked  = {default=0};
	__property Graphics::TColor Color = {read=FColor, write=SetColor, default=16777215};
	__property DisplayMode  = {default=0};
	__property Enabled  = {default=1};
	__property FontSettings ;
	__property GroupIndex  = {default=0};
	__property HelpContext  = {default=0};
	__property HelpKeyword ;
	__property Hint ;
	__property InheritOptions  = {default=1};
	__property MaskOptions  = {default=0};
	__property MinHeight  = {default=0};
	__property MinWidth  = {default=0};
	__property Options  = {default=0};
	__property RadioItem  = {default=0};
	__property ShortCut  = {default=0};
	__property Visible  = {default=1};
	__property OnAdjustFont ;
	__property OnClick ;
public:
	#pragma option push -w-inl
	/* TTBXCustomItem.Destroy */ inline __fastcall virtual ~TTBXColorItem(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXColorItemViewer;
class PASCALIMPLEMENTATION TTBXColorItemViewer : public Tbx::TTBXItemViewer 
{
	typedef Tbx::TTBXItemViewer inherited;
	
protected:
	virtual void __fastcall DoPaintCaption(Graphics::TCanvas* Canvas, const Types::TRect &ClientAreaRect, Types::TRect &CaptionRect, bool IsTextRotated, bool &PaintDefault);
	virtual bool __fastcall GetImageShown(void);
	DYNAMIC tagSIZE __fastcall GetImageSize();
	virtual void __fastcall DrawItemImage(Graphics::TCanvas* Canvas, const Types::TRect &ARect, const Tbxthemes::TTBXItemInfo &ItemInfo);
	
public:
	__fastcall virtual TTBXColorItemViewer(Tb2item::TTBView* AView, Tb2item::TTBCustomItem* AItem, int AGroupLevel);
public:
	#pragma option push -w-inl
	/* TTBItemViewer.Destroy */ inline __fastcall virtual ~TTBXColorItemViewer(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
static const Shortint tcrNumericProperty = 0x3;
static const Shortint tcrSpinButton = 0x4;
static const Shortint tcrList = 0x5;

}	/* namespace Tbxextitems */
using namespace Tbxextitems;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TBXExtItems
