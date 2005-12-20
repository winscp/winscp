// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TBXToolPals.pas' rev: 6.00

#ifndef TBXToolPalsHPP
#define TBXToolPalsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ImgList.hpp>	// Pascal unit
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

namespace Tbxtoolpals
{
//-- type declarations -------------------------------------------------------
typedef Shortint TRowColCount;

class DELPHICLASS TTBXCustomToolPalette;
typedef void __fastcall (__closure *TTPCalcSize)(TTBXCustomToolPalette* Sender, Graphics::TCanvas* Canvas, int &AWidth, int &AHeight);

typedef void __fastcall (__closure *TTPGetCellVisible)(TTBXCustomToolPalette* Sender, int ACol, int ARow, bool &Visible);

typedef void __fastcall (__closure *TTPGetCellHint)(TTBXCustomToolPalette* Sender, int ACol, int ARow, AnsiString &HintText);

typedef void __fastcall (__closure *TTPDrawCellImage)(TTBXCustomToolPalette* Sender, Graphics::TCanvas* Canvas, const Types::TRect &ARect, int ACol, int ARow, bool Selected, bool Hot, bool Enabled);

typedef void __fastcall (__closure *TTPCellClick)(TTBXCustomToolPalette* Sender, int &ACol, int &ARow, bool &AllowChange);

#pragma option push -b-
enum TBXToolPals__1 { tpoCustomImages, tpoNoAutoSelect };
#pragma option pop

typedef Set<TBXToolPals__1, tpoCustomImages, tpoNoAutoSelect>  TTBXToolPaletteOptions;

class PASCALIMPLEMENTATION TTBXCustomToolPalette : public Tbx::TTBXCustomItem 
{
	typedef Tbx::TTBXCustomItem inherited;
	
private:
	TRowColCount FColCount;
	TTBXToolPaletteOptions FPaletteOptions;
	TRowColCount FRowCount;
	#pragma pack(push, 1)
	Types::TPoint FSelectedCell;
	#pragma pack(pop)
	
	TTPCalcSize FOnCalcImageSize;
	Classes::TNotifyEvent FOnChange;
	TTPCellClick FOnCellClick;
	TTPDrawCellImage FOnDrawCellImage;
	TTPGetCellVisible FOnGetCellVisible;
	TTPGetCellHint FOnGetCellHint;
	void __fastcall SetColCount(TRowColCount Value);
	void __fastcall SetPaletteOptions(TTBXToolPaletteOptions Value);
	void __fastcall SetRowCount(TRowColCount Value);
	void __fastcall SetSelectedCell(const Types::TPoint &Value);
	
protected:
	virtual void __fastcall DoCalcCellSize(Graphics::TCanvas* Canvas, int &AWidth, int &AHeight);
	virtual void __fastcall DoCalcImageSize(Graphics::TCanvas* Canvas, int &AWidth, int &AHeight);
	virtual bool __fastcall DoCellClick(int &ACol, int &ARow);
	virtual void __fastcall DoChange(void);
	virtual void __fastcall DoDrawCellImage(Graphics::TCanvas* Canvas, const Types::TRect &ARect, int ACol, int ARow, const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall DoGetCellVisible(int ACol, int ARow, bool &Visible);
	virtual void __fastcall DoGetHint(const Types::TPoint &ACell, AnsiString &HintText);
	virtual TMetaClass* __fastcall GetItemViewerClass(Tb2item::TTBView* AView);
	virtual void __fastcall HandleClickCell(int ACol, int ARow);
	__property TRowColCount ColCount = {read=FColCount, write=SetColCount, default=1};
	__property TTBXToolPaletteOptions PaletteOptions = {read=FPaletteOptions, write=SetPaletteOptions, nodefault};
	__property TRowColCount RowCount = {read=FRowCount, write=SetRowCount, default=1};
	__property Types::TPoint SelectedCell = {read=FSelectedCell, write=SetSelectedCell};
	__property Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property TTPCalcSize OnCalcImageSize = {read=FOnCalcImageSize, write=FOnCalcImageSize};
	__property TTPCellClick OnCellClick = {read=FOnCellClick, write=FOnCellClick};
	__property TTPDrawCellImage OnDrawCellImage = {read=FOnDrawCellImage, write=FOnDrawCellImage};
	__property TTPGetCellVisible OnGetCellVisible = {read=FOnGetCellVisible, write=FOnGetCellVisible};
	__property TTPGetCellHint OnGetCellHint = {read=FOnGetCellHint, write=FOnGetCellHint};
	
public:
	__fastcall virtual TTBXCustomToolPalette(Classes::TComponent* AOwner);
public:
	#pragma option push -w-inl
	/* TTBXCustomItem.Destroy */ inline __fastcall virtual ~TTBXCustomToolPalette(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXToolPalette;
class PASCALIMPLEMENTATION TTBXToolPalette : public TTBXCustomToolPalette 
{
	typedef TTBXCustomToolPalette inherited;
	
public:
	__property SelectedCell ;
	
__published:
	__property ColCount  = {default=1};
	__property HelpContext  = {default=0};
	__property Images ;
	__property Options  = {default=0};
	__property PaletteOptions ;
	__property RowCount  = {default=1};
	__property Stretch  = {default=0};
	__property Visible  = {default=1};
	__property OnChange ;
	__property OnCalcImageSize ;
	__property OnCellClick ;
	__property OnDrawCellImage ;
	__property OnGetCellHint ;
	__property OnGetCellVisible ;
public:
	#pragma option push -w-inl
	/* TTBXCustomToolPalette.Create */ inline __fastcall virtual TTBXToolPalette(Classes::TComponent* AOwner) : TTBXCustomToolPalette(AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TTBXCustomItem.Destroy */ inline __fastcall virtual ~TTBXToolPalette(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXToolViewer;
class PASCALIMPLEMENTATION TTBXToolViewer : public Tbx::TTBXItemViewer 
{
	typedef Tbx::TTBXItemViewer inherited;
	
private:
	int FCellHeight;
	int FCellWidth;
	int FColCount;
	int FRowCount;
	#pragma pack(push, 1)
	Types::TPoint FHotCell;
	#pragma pack(pop)
	
	
protected:
	int Indent;
	bool MouseIsDown;
	virtual void __fastcall CalcCellSize(Graphics::TCanvas* Canvas, int &AWidth, int &AHeight);
	virtual void __fastcall CalcImageSize(Graphics::TCanvas* Canvas, int &AWidth, int &AHeight);
	virtual void __fastcall CalcSize(const Graphics::TCanvas* Canvas, int &AWidth, int &AHeight);
	int __fastcall GetImageIndex(int Col, int Row);
	bool __fastcall GetCellAt(int X, int Y, /* out */ int &Col, /* out */ int &Row);
	virtual Types::TRect __fastcall GetCellRect(const Types::TRect &ClientAreaRect, int Col, int Row);
	AnsiString __fastcall GetHint(int Col, int Row);
	MESSAGE void __fastcall CMHintShow(Forms::TCMHintShow &Message);
	void __fastcall DrawCell(Graphics::TCanvas* Canvas, const Types::TRect &CellRect, int Col, int Row, Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall DrawCellImage(Graphics::TCanvas* Canvas, const Types::TRect &ARect, int Col, int Row, const Tbxthemes::TTBXItemInfo &ItemInfo);
	virtual void __fastcall Entering(Tb2item::TTBItemViewer* OldSelected);
	void __fastcall InvalidateCell(int ACol, int ARow);
	virtual bool __fastcall IsCellVisible(const Types::TPoint &Cell);
	virtual void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	virtual void __fastcall MouseDown(Classes::TShiftState Shift, int X, int Y, bool &MouseDownOnMenu);
	virtual void __fastcall MouseMove(int X, int Y);
	virtual void __fastcall MouseUp(int X, int Y, bool MouseWasDownOnMenu);
	virtual void __fastcall Paint(const Graphics::TCanvas* Canvas, const Types::TRect &ClientAreaRect, bool IsHoverItem, bool IsPushed, bool UseDisabledShadow);
	__property int CellHeight = {read=FCellHeight, nodefault};
	__property int CellWidth = {read=FCellWidth, nodefault};
	__property int ColCount = {read=FColCount, nodefault};
	__property Types::TPoint HotCell = {read=FHotCell};
	__property int RowCount = {read=FRowCount, nodefault};
	
public:
	__fastcall virtual TTBXToolViewer(Tb2item::TTBView* AView, Tb2item::TTBCustomItem* AItem, int AGroupLevel);
public:
	#pragma option push -w-inl
	/* TTBItemViewer.Destroy */ inline __fastcall virtual ~TTBXToolViewer(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXCustomColorSet;
typedef void __fastcall (__closure *TCSGetColorInfo)(TTBXCustomColorSet* Sender, int Col, int Row, Graphics::TColor &Color, AnsiString &Name);

class PASCALIMPLEMENTATION TTBXCustomColorSet : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	Classes::TList* FPalettes;
	int FColCount;
	int FRowCount;
	TCSGetColorInfo FOnGetColorInfo;
	void __fastcall SetColCount(int Value);
	void __fastcall SetRowCount(int Value);
	
protected:
	virtual void __fastcall UpdateSize(int NewColCount, int NewRowCount);
	virtual AnsiString __fastcall ColorToString(Graphics::TColor Color);
	virtual void __fastcall GetColorInfo(int Col, int Row, /* out */ Graphics::TColor &Color, /* out */ AnsiString &ColorName);
	
public:
	__fastcall virtual TTBXCustomColorSet(Classes::TComponent* AOwner);
	__fastcall virtual ~TTBXCustomColorSet(void);
	Graphics::TColor __fastcall GetColor(int Col, int Row);
	AnsiString __fastcall GetName(int Col, int Row);
	__property int ColCount = {read=FColCount, write=SetColCount, nodefault};
	__property int RowCount = {read=FRowCount, write=SetRowCount, nodefault};
	__property TCSGetColorInfo OnGetColorInfo = {read=FOnGetColorInfo, write=FOnGetColorInfo};
};


class DELPHICLASS TTBXColorSet;
class PASCALIMPLEMENTATION TTBXColorSet : public TTBXCustomColorSet 
{
	typedef TTBXCustomColorSet inherited;
	
__published:
	__property ColCount ;
	__property RowCount ;
	__property OnGetColorInfo ;
public:
	#pragma option push -w-inl
	/* TTBXCustomColorSet.Create */ inline __fastcall virtual TTBXColorSet(Classes::TComponent* AOwner) : TTBXCustomColorSet(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TTBXCustomColorSet.Destroy */ inline __fastcall virtual ~TTBXColorSet(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBXColorPalette;
class PASCALIMPLEMENTATION TTBXColorPalette : public TTBXCustomToolPalette 
{
	typedef TTBXCustomToolPalette inherited;
	
private:
	Graphics::TColor FColor;
	TTBXCustomColorSet* FColorSet;
	void __fastcall SetColorSet(TTBXCustomColorSet* Value);
	void __fastcall SetColor(Graphics::TColor Value);
	
protected:
	virtual void __fastcall DoCalcImageSize(Graphics::TCanvas* Canvas, int &AWidth, int &AHeight);
	virtual void __fastcall DoChange(void);
	virtual void __fastcall DoGetCellVisible(int ACol, int ARow, bool &Visible);
	virtual void __fastcall DoGetHint(const Types::TPoint &ACell, AnsiString &HintText);
	virtual void __fastcall DoDrawCellImage(Graphics::TCanvas* Canvas, const Types::TRect &ARect, int ACol, int ARow, const Tbxthemes::TTBXItemInfo &ItemInfo);
	TTBXCustomColorSet* __fastcall GetColorSet(void);
	virtual Graphics::TColor __fastcall GetCellColor(int ACol, int ARow);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	
public:
	__fastcall virtual TTBXColorPalette(Classes::TComponent* AOwner);
	Types::TPoint __fastcall FindCell(Graphics::TColor AColor);
	AnsiString __fastcall ColorToString(Graphics::TColor AColor);
	
__published:
	__property Graphics::TColor Color = {read=FColor, write=SetColor, default=536870911};
	__property TTBXCustomColorSet* ColorSet = {read=FColorSet, write=SetColorSet};
	__property HelpContext  = {default=0};
	__property InheritOptions  = {default=1};
	__property MaskOptions  = {default=0};
	__property Options  = {default=128};
	__property PaletteOptions ;
	__property Stretch  = {default=0};
	__property Visible  = {default=1};
	__property OnChange ;
	__property OnCellClick ;
	__property OnGetCellHint ;
public:
	#pragma option push -w-inl
	/* TTBXCustomItem.Destroy */ inline __fastcall virtual ~TTBXColorPalette(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Tbxtoolpals */
using namespace Tbxtoolpals;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TBXToolPals
