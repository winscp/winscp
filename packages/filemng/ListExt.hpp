// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ListExt.pas' rev: 6.00

#ifndef ListExtHPP
#define ListExtHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Listext
{
//-- type declarations -------------------------------------------------------
typedef int IntType;

typedef void __fastcall (__closure *TLxDeleteEvent)(System::TObject* Sender, void * &P, int Size);

typedef DynamicArray<void * >  ListExt__2;

class DELPHICLASS TListExt;
class PASCALIMPLEMENTATION TListExt : public System::TInterfacedObject 
{
	typedef System::TInterfacedObject inherited;
	
public:
	void * operator[](int i) { return data[i]; }
	
private:
	int FCount;
	DynamicArray<void * >  FData;
	bool FSorted;
	TLxDeleteEvent fOnDelete;
	int FItemSize;
	int MaxCount;
	void * __fastcall GetItem(int I);
	void __fastcall FreeItem(int I);
	
public:
	__property void * data[int i] = {read=GetItem/*, default*/};
	__property bool Sorted = {read=FSorted, nodefault};
	__property int Count = {read=FCount, nodefault};
	__property int ItemSize = {read=FItemSize, nodefault};
	__fastcall TListExt(int ItemSize);
	HIDESBASE void __fastcall Free(void);
	void __fastcall Clear(void);
	void __fastcall Add(void * P);
	int __fastcall IndexOf(void * P);
	void __fastcall Sort(Classes::TListSortCompare Compare);
	int __fastcall Find(void * P, Classes::TListSortCompare Compare);
	int __fastcall FindSequential(void * P, Classes::TListSortCompare Compare);
	void * __fastcall First(void);
	void * __fastcall Last(void);
	void __fastcall Delete(int I);
	
__published:
	__property TLxDeleteEvent OnDelete = {read=fOnDelete, write=fOnDelete};
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TListExt(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
static const Word InitSize = 0x1f4;
static const Word ExtendSize = 0x1f4;
static const Shortint FLess = 0xffffffff;
static const Shortint FEqual = 0x0;
static const Shortint FGreater = 0x1;

}	/* namespace Listext */
using namespace Listext;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ListExt
