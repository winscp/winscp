// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ListSort.pas' rev: 6.00

#ifndef ListSortHPP
#define ListSortHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Listsort
{
//-- type declarations -------------------------------------------------------
typedef int __fastcall (__closure *TCompareFunction)(System::TObject* Sender, void * Item1, void * Item2);

typedef void __fastcall (__closure *TOnListEvent)(System::TObject* Sender, void * Item);

typedef void __fastcall (__closure *TBatchControlEvent)(System::TObject* Sender, void * Item);

class DELPHICLASS TSortedList;
class PASCALIMPLEMENTATION TSortedList : public Classes::TList 
{
	typedef Classes::TList inherited;
	
private:
	TCompareFunction FCompare;
	TOnListEvent FOnAdd;
	TOnListEvent FOnModify;
	TOnListEvent FOnErase;
	bool FAllowDuplicates;
	
public:
	__fastcall TSortedList(void);
	HIDESBASE int __fastcall Add(void * Item);
	HIDESBASE void __fastcall Clear(void);
	HIDESBASE void __fastcall Delete(int Index);
	int __fastcall FindObject(void * Item);
	HIDESBASE int __fastcall Insert(int Index, void * Item);
	int __fastcall Rearrange(void * Item);
	HIDESBASE void __fastcall Remove(void * Item);
	HIDESBASE void __fastcall Sort(void);
	__property bool AllowDuplicates = {read=FAllowDuplicates, write=FAllowDuplicates, nodefault};
	__property TOnListEvent OnAdd = {read=FOnAdd, write=FOnAdd};
	__property TCompareFunction OnCompare = {read=FCompare, write=FCompare};
	__property TOnListEvent OnModify = {read=FOnModify, write=FOnModify};
	__property TOnListEvent OnErase = {read=FOnErase, write=FOnErase};
public:
	#pragma option push -w-inl
	/* TList.Destroy */ inline __fastcall virtual ~TSortedList(void) { }
	#pragma option pop
	
};


class DELPHICLASS TBatchControl;
class PASCALIMPLEMENTATION TBatchControl : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	TBatchControlEvent FOnProcess;
	TSortedList* FList;
	
public:
	__property TSortedList* List = {read=FList, write=FList};
	__fastcall virtual TBatchControl(Classes::TComponent* AOwner);
	__fastcall virtual ~TBatchControl(void);
	void __fastcall Execute(void);
	
__published:
	__property TBatchControlEvent OnProcess = {read=FOnProcess, write=FOnProcess};
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Listsort */
using namespace Listsort;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ListSort
