// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UnixDirViewColProperties.pas' rev: 6.00

#ifndef UnixDirViewColPropertiesHPP
#define UnixDirViewColPropertiesHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ListViewColProperties.hpp>	// Pascal unit
#include <DirViewColProperties.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Unixdirviewcolproperties
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TUnixDirViewCol { uvName, uvSize, uvChanged, uvRights, uvOwner, uvGroup, uvExt };
#pragma option pop

class DELPHICLASS TUnixDirViewColProperties;
class PASCALIMPLEMENTATION TUnixDirViewColProperties : public Dirviewcolproperties::TCustomDirViewColProperties 
{
	typedef Dirviewcolproperties::TCustomDirViewColProperties inherited;
	
private:
	bool __fastcall StoreAlignment(int Index);
	bool __fastcall StoreCaption(int Index);
	bool __fastcall StoreWidth(int Index);
	TUnixDirViewCol __fastcall GetDirOrder(int Index);
	TUnixDirViewCol __fastcall GetSortDirColumn(void);
	void __fastcall SetDirOrder(int Index, TUnixDirViewCol Value);
	void __fastcall SetSortDirColumn(TUnixDirViewCol Value);
	
public:
	__fastcall TUnixDirViewColProperties(Comctrls::TCustomListView* DirView);
	
__published:
	__property MaxWidth  = {default=1000};
	__property MinWidth  = {default=20};
	__property SortAscending  = {default=1};
	__property SortByExtension  = {default=0};
	__property TUnixDirViewCol SortDirColumn = {read=GetSortDirColumn, write=SetSortDirColumn, default=0};
	__property AnsiString NameCaption = {read=GetCaptions, write=SetCaptions, stored=StoreCaption, index=0};
	__property int NameWidth = {read=GetWidths, write=SetWidths, stored=StoreWidth, index=0, nodefault};
	__property bool NameVisible = {read=GetVisible, write=SetVisible, index=0, default=1};
	__property Classes::TAlignment NameAlignment = {read=GetAlignments, write=SetAlignments, stored=StoreAlignment, index=0, nodefault};
	__property AnsiString SizeCaption = {read=GetCaptions, write=SetCaptions, stored=StoreCaption, index=1};
	__property int SizeWidth = {read=GetWidths, write=SetWidths, stored=StoreWidth, index=1, nodefault};
	__property bool SizeVisible = {read=GetVisible, write=SetVisible, index=1, default=1};
	__property Classes::TAlignment SizeAlignment = {read=GetAlignments, write=SetAlignments, stored=StoreAlignment, index=1, nodefault};
	__property AnsiString ChangedCaption = {read=GetCaptions, write=SetCaptions, stored=StoreCaption, index=2};
	__property int ChangedWidth = {read=GetWidths, write=SetWidths, stored=StoreWidth, index=2, nodefault};
	__property bool ChangedVisible = {read=GetVisible, write=SetVisible, index=2, default=1};
	__property Classes::TAlignment ChangedAlignment = {read=GetAlignments, write=SetAlignments, stored=StoreAlignment, index=2, nodefault};
	__property AnsiString RightsCaption = {read=GetCaptions, write=SetCaptions, stored=StoreCaption, index=3};
	__property int RightsWidth = {read=GetWidths, write=SetWidths, stored=StoreWidth, index=3, nodefault};
	__property bool RightsVisible = {read=GetVisible, write=SetVisible, index=3, default=1};
	__property Classes::TAlignment RightsAlignment = {read=GetAlignments, write=SetAlignments, stored=StoreAlignment, index=3, nodefault};
	__property AnsiString OwnerCaption = {read=GetCaptions, write=SetCaptions, stored=StoreCaption, index=4};
	__property int OwnerWidth = {read=GetWidths, write=SetWidths, stored=StoreWidth, index=4, nodefault};
	__property bool OwnerVisible = {read=GetVisible, write=SetVisible, index=4, default=1};
	__property Classes::TAlignment OwnerAlignment = {read=GetAlignments, write=SetAlignments, stored=StoreAlignment, index=4, nodefault};
	__property AnsiString GroupCaption = {read=GetCaptions, write=SetCaptions, stored=StoreCaption, index=5};
	__property int GroupWidth = {read=GetWidths, write=SetWidths, stored=StoreWidth, index=5, nodefault};
	__property bool GroupVisible = {read=GetVisible, write=SetVisible, index=5, default=1};
	__property Classes::TAlignment GroupAlignment = {read=GetAlignments, write=SetAlignments, stored=StoreAlignment, index=5, nodefault};
	__property AnsiString ExtCaption = {read=GetCaptions, write=SetCaptions, stored=StoreCaption, index=6};
	__property int ExtWidth = {read=GetWidths, write=SetWidths, stored=StoreWidth, index=6, nodefault};
	__property bool ExtVisible = {read=GetVisible, write=SetVisible, index=6, default=1};
	__property Classes::TAlignment ExtAlignment = {read=GetAlignments, write=SetAlignments, stored=StoreAlignment, index=6, nodefault};
	__property TUnixDirViewCol Column1 = {read=GetDirOrder, write=SetDirOrder, index=0, default=0};
	__property TUnixDirViewCol Column2 = {read=GetDirOrder, write=SetDirOrder, index=1, default=1};
	__property TUnixDirViewCol Column3 = {read=GetDirOrder, write=SetDirOrder, index=2, default=2};
	__property TUnixDirViewCol Column4 = {read=GetDirOrder, write=SetDirOrder, index=3, default=3};
	__property TUnixDirViewCol Column5 = {read=GetDirOrder, write=SetDirOrder, index=4, default=4};
	__property TUnixDirViewCol Column6 = {read=GetDirOrder, write=SetDirOrder, index=5, default=5};
	__property TUnixDirViewCol Column7 = {read=GetDirOrder, write=SetDirOrder, index=6, default=6};
public:
	#pragma option push -w-inl
	/* TPersistent.Destroy */ inline __fastcall virtual ~TUnixDirViewColProperties(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::ResourceString _SUnixDirViewRightsCol;
#define Unixdirviewcolproperties_SUnixDirViewRightsCol System::LoadResourceString(&Unixdirviewcolproperties::_SUnixDirViewRightsCol)
extern PACKAGE System::ResourceString _SUnixDirViewOwnerCol;
#define Unixdirviewcolproperties_SUnixDirViewOwnerCol System::LoadResourceString(&Unixdirviewcolproperties::_SUnixDirViewOwnerCol)
extern PACKAGE System::ResourceString _SUnixDirViewGroupCol;
#define Unixdirviewcolproperties_SUnixDirViewGroupCol System::LoadResourceString(&Unixdirviewcolproperties::_SUnixDirViewGroupCol)
static const Shortint UnixDirViewColumns = 0x7;
extern PACKAGE void *DefaultUnixDirViewCaptions[7];
extern PACKAGE int DefaultUnixDirViewWidths[7];
extern PACKAGE Classes::TAlignment DefaultUnixDirViewAlignments[7];
extern PACKAGE bool DefaultUnixDirViewVisible[7];

}	/* namespace Unixdirviewcolproperties */
using namespace Unixdirviewcolproperties;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnixDirViewColProperties
