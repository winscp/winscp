// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DirViewColProperties.pas' rev: 6.00

#ifndef DirViewColPropertiesHPP
#define DirViewColPropertiesHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ListViewColProperties.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dirviewcolproperties
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TCustomDirViewColProperties;
class PASCALIMPLEMENTATION TCustomDirViewColProperties : public Listviewcolproperties::TCustomListViewColProperties 
{
	typedef Listviewcolproperties::TCustomListViewColProperties inherited;
	
private:
	bool __fastcall GetSortAscending(void);
	bool __fastcall GetSortByExtension(void);
	void __fastcall SetSortColumn(int Value);
	int __fastcall GetSortColumn(void);
	AnsiString __fastcall GetSortStr();
	void __fastcall SetSortAscending(bool Value);
	void __fastcall SetSortByExtension(bool Value);
	void __fastcall SetSortStr(AnsiString Value);
	
public:
	__property bool SortAscending = {read=GetSortAscending, write=SetSortAscending, default=1};
	__property bool SortByExtension = {read=GetSortByExtension, write=SetSortByExtension, default=0};
	__property int SortColumn = {read=GetSortColumn, write=SetSortColumn, nodefault};
	__property AnsiString SortStr = {read=GetSortStr, write=SetSortStr, stored=false};
	
protected:
	virtual AnsiString __fastcall GetParamsStr();
	virtual void __fastcall SetParamsStr(AnsiString Value);
public:
	#pragma option push -w-inl
	/* TCustomListViewColProperties.Create */ inline __fastcall TCustomDirViewColProperties(Comctrls::TCustomListView* ListView, int ColCount) : Listviewcolproperties::TCustomListViewColProperties(ListView, ColCount) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TPersistent.Destroy */ inline __fastcall virtual ~TCustomDirViewColProperties(void) { }
	#pragma option pop
	
};


#pragma option push -b-
enum TDirViewCol { dvName, dvSize, dvType, dvChanged, dvAttr, dvExt };
#pragma option pop

class DELPHICLASS TDirViewColProperties;
class PASCALIMPLEMENTATION TDirViewColProperties : public TCustomDirViewColProperties 
{
	typedef TCustomDirViewColProperties inherited;
	
private:
	bool __fastcall StoreAlignment(int Index);
	bool __fastcall StoreCaption(int Index);
	bool __fastcall StoreWidth(int Index);
	TDirViewCol __fastcall GetDirOrder(int Index);
	TDirViewCol __fastcall GetSortDirColumn(void);
	void __fastcall SetDirOrder(int Index, TDirViewCol Value);
	void __fastcall SetSortDirColumn(TDirViewCol Value);
	
public:
	__fastcall TDirViewColProperties(Comctrls::TCustomListView* DirView);
	
__published:
	__property MaxWidth  = {default=1000};
	__property MinWidth  = {default=20};
	__property SortAscending  = {default=1};
	__property SortByExtension  = {default=0};
	__property TDirViewCol SortDirColumn = {read=GetSortDirColumn, write=SetSortDirColumn, default=0};
	__property AnsiString NameCaption = {read=GetCaptions, write=SetCaptions, stored=StoreCaption, index=0};
	__property int NameWidth = {read=GetWidths, write=SetWidths, stored=StoreWidth, index=0, nodefault};
	__property bool NameVisible = {read=GetVisible, write=SetVisible, index=0, default=1};
	__property Classes::TAlignment NameAlignment = {read=GetAlignments, write=SetAlignments, stored=StoreAlignment, index=0, nodefault};
	__property AnsiString SizeCaption = {read=GetCaptions, write=SetCaptions, stored=StoreCaption, index=1};
	__property int SizeWidth = {read=GetWidths, write=SetWidths, stored=StoreWidth, index=1, nodefault};
	__property bool SizeVisible = {read=GetVisible, write=SetVisible, index=1, default=1};
	__property Classes::TAlignment SizeAlignment = {read=GetAlignments, write=SetAlignments, stored=StoreAlignment, index=1, nodefault};
	__property AnsiString TypeCaption = {read=GetCaptions, write=SetCaptions, stored=StoreCaption, index=2};
	__property int TypeWidth = {read=GetWidths, write=SetWidths, stored=StoreWidth, index=2, nodefault};
	__property bool TypeVisible = {read=GetVisible, write=SetVisible, index=2, default=1};
	__property Classes::TAlignment TypeAlignment = {read=GetAlignments, write=SetAlignments, stored=StoreAlignment, index=2, nodefault};
	__property AnsiString ChangedCaption = {read=GetCaptions, write=SetCaptions, stored=StoreCaption, index=3};
	__property int ChangedWidth = {read=GetWidths, write=SetWidths, stored=StoreWidth, index=3, nodefault};
	__property bool ChangedVisible = {read=GetVisible, write=SetVisible, index=3, default=1};
	__property Classes::TAlignment ChangedAlignment = {read=GetAlignments, write=SetAlignments, stored=StoreAlignment, index=3, nodefault};
	__property AnsiString AttrCaption = {read=GetCaptions, write=SetCaptions, stored=StoreCaption, index=4};
	__property int AttrWidth = {read=GetWidths, write=SetWidths, stored=StoreWidth, index=4, nodefault};
	__property bool AttrVisible = {read=GetVisible, write=SetVisible, index=4, default=1};
	__property Classes::TAlignment AttrAlignment = {read=GetAlignments, write=SetAlignments, stored=StoreAlignment, index=4, nodefault};
	__property AnsiString ExtCaption = {read=GetCaptions, write=SetCaptions, stored=StoreCaption, index=5};
	__property int ExtWidth = {read=GetWidths, write=SetWidths, stored=StoreWidth, index=5, nodefault};
	__property bool ExtVisible = {read=GetVisible, write=SetVisible, index=5, default=1};
	__property Classes::TAlignment ExtAlignment = {read=GetAlignments, write=SetAlignments, stored=StoreAlignment, index=5, nodefault};
	__property TDirViewCol Column1 = {read=GetDirOrder, write=SetDirOrder, index=0, default=0};
	__property TDirViewCol Column2 = {read=GetDirOrder, write=SetDirOrder, index=1, default=1};
	__property TDirViewCol Column3 = {read=GetDirOrder, write=SetDirOrder, index=2, default=2};
	__property TDirViewCol Column4 = {read=GetDirOrder, write=SetDirOrder, index=3, default=3};
	__property TDirViewCol Column5 = {read=GetDirOrder, write=SetDirOrder, index=4, default=4};
	__property TDirViewCol Column6 = {read=GetDirOrder, write=SetDirOrder, index=5, default=5};
public:
	#pragma option push -w-inl
	/* TPersistent.Destroy */ inline __fastcall virtual ~TDirViewColProperties(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::ResourceString _SDirViewNameCol;
#define Dirviewcolproperties_SDirViewNameCol System::LoadResourceString(&Dirviewcolproperties::_SDirViewNameCol)
extern PACKAGE System::ResourceString _SDirViewSizeCol;
#define Dirviewcolproperties_SDirViewSizeCol System::LoadResourceString(&Dirviewcolproperties::_SDirViewSizeCol)
extern PACKAGE System::ResourceString _SDirViewTypeCol;
#define Dirviewcolproperties_SDirViewTypeCol System::LoadResourceString(&Dirviewcolproperties::_SDirViewTypeCol)
extern PACKAGE System::ResourceString _SDirViewChangedCol;
#define Dirviewcolproperties_SDirViewChangedCol System::LoadResourceString(&Dirviewcolproperties::_SDirViewChangedCol)
extern PACKAGE System::ResourceString _SDirViewAttrCol;
#define Dirviewcolproperties_SDirViewAttrCol System::LoadResourceString(&Dirviewcolproperties::_SDirViewAttrCol)
extern PACKAGE System::ResourceString _SDirViewExtCol;
#define Dirviewcolproperties_SDirViewExtCol System::LoadResourceString(&Dirviewcolproperties::_SDirViewExtCol)
static const Shortint DirViewColumns = 0x6;
extern PACKAGE void *DefaultDirViewCaptions[6];
extern PACKAGE int DefaultDirViewWidths[6];
extern PACKAGE Classes::TAlignment DefaultDirViewAlignments[6];
extern PACKAGE bool DefaultDirViewVisible[6];

}	/* namespace Dirviewcolproperties */
using namespace Dirviewcolproperties;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DirViewColProperties
