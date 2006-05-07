// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ListViewColProperties.pas' rev: 6.00

#ifndef ListViewColPropertiesHPP
#define ListViewColPropertiesHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ComCtrls.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Listviewcolproperties
{
//-- type declarations -------------------------------------------------------
typedef DynamicArray<int >  ListViewColProperties__2;

typedef DynamicArray<bool >  ListViewColProperties__3;

class DELPHICLASS TCustomListViewColProperties;
class PASCALIMPLEMENTATION TCustomListViewColProperties : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;
	
private:
	bool FChanged;
	int FMaxWidth;
	int FMinWidth;
	Classes::TNotifyEvent FOnChange;
	int FUpdating;
	DynamicArray<int >  FWidths;
	DynamicArray<bool >  FVisible;
	Comctrls::TListColumns* __fastcall GetColumns(void);
	int __fastcall GetCount(void);
	AnsiString __fastcall GetOrderStr();
	void __fastcall CheckBounds(int Index);
	void __fastcall SetMaxWidth(int Value);
	void __fastcall SetMinWidth(int Value);
	void __fastcall SetWidthsStr(AnsiString Value);
	AnsiString __fastcall GetWidthsStr();
	void __fastcall SetOrderStr(AnsiString Value);
	
protected:
	Comctrls::TCustomListView* FListView;
	bool FListViewManaged;
	Classes::TAlignment __fastcall GetAlignments(int Index);
	int __fastcall GetOrder(int Index);
	virtual AnsiString __fastcall GetParamsStr();
	bool __fastcall GetVisible(int Index);
	int __fastcall GetWidths(int Index);
	void __fastcall SetAlignments(int Index, Classes::TAlignment Value);
	void __fastcall SetVisible(int Index, bool Value);
	void __fastcall SetWidths(int Index, int Value);
	void __fastcall SetOrder(int Index, int Value);
	AnsiString __fastcall GetCaptions(int Index);
	virtual void __fastcall Changed(void);
	virtual void __fastcall SetCaptions(int Index, AnsiString Value);
	virtual void __fastcall SetParamsStr(AnsiString Value);
	void __fastcall UpdateFromListView(void);
	__property Comctrls::TListColumns* Columns = {read=GetColumns, stored=false};
	__property int Count = {read=GetCount, stored=false, nodefault};
	
public:
	__fastcall TCustomListViewColProperties(Comctrls::TCustomListView* ListView, int ColCount);
	void __fastcall EndUpdate(void);
	void __fastcall BeginUpdate(void);
	void __fastcall ListViewWndCreated(void);
	__property Classes::TAlignment Alignments[int Index] = {read=GetAlignments, write=SetAlignments};
	__property AnsiString Captions[int Index] = {read=GetCaptions, write=SetCaptions};
	__property int MaxWidth = {read=FMaxWidth, write=SetMaxWidth, default=1000};
	__property int MinWidth = {read=FMinWidth, write=SetMinWidth, default=20};
	__property int Widths[int Index] = {read=GetWidths, write=SetWidths};
	__property bool Visible[int Index] = {read=GetVisible, write=SetVisible};
	void __fastcall RecreateColumns(void);
	__property Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property int Order[int Index] = {read=GetOrder, write=SetOrder};
	__property AnsiString OrderStr = {read=GetOrderStr, write=SetOrderStr, stored=false};
	__property AnsiString ParamsStr = {read=GetParamsStr, write=SetParamsStr, stored=false};
	__property AnsiString WidthsStr = {read=GetWidthsStr, write=SetWidthsStr, stored=false};
public:
	#pragma option push -w-inl
	/* TPersistent.Destroy */ inline __fastcall virtual ~TCustomListViewColProperties(void) { }
	#pragma option pop
	
};


class DELPHICLASS TListViewColProperties;
class PASCALIMPLEMENTATION TListViewColProperties : public TCustomListViewColProperties 
{
	typedef TCustomListViewColProperties inherited;
	
__published:
	__property MaxWidth  = {default=1000};
	__property MinWidth  = {default=20};
public:
	#pragma option push -w-inl
	/* TCustomListViewColProperties.Create */ inline __fastcall TListViewColProperties(Comctrls::TCustomListView* ListView, int ColCount) : TCustomListViewColProperties(ListView, ColCount) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TPersistent.Destroy */ inline __fastcall virtual ~TListViewColProperties(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
static const Word DefaultListViewMaxWidth = 0x3e8;
static const Shortint DefaultListViewMinWidth = 0x14;
extern PACKAGE AnsiString __fastcall CutToChar(AnsiString &Str, char Ch, bool Trim);

}	/* namespace Listviewcolproperties */
using namespace Listviewcolproperties;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ListViewColProperties
