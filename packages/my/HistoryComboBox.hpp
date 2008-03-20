// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'HistoryComboBox.pas' rev: 6.00

#ifndef HistoryComboBoxHPP
#define HistoryComboBoxHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
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

namespace Historycombobox
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum HistoryComboBox__1 { soExit, soDropDown };
#pragma option pop

typedef Set<HistoryComboBox__1, soExit, soDropDown>  THistorySaveOn;

class DELPHICLASS THistoryComboBox;
typedef void __fastcall (__closure *THistoryComboBoxGetData)(THistoryComboBox* Sender, void * &Data);

typedef void __fastcall (__closure *THistoryComboBoxSetData)(THistoryComboBox* Sender, void * Data);

class PASCALIMPLEMENTATION THistoryComboBox : public Stdctrls::TComboBox 
{
	typedef Stdctrls::TComboBox inherited;
	
private:
	THistorySaveOn FSaveOn;
	int FMaxHistorySize;
	THistoryComboBoxGetData FOnGetData;
	THistoryComboBoxSetData FOnSetData;
	void __fastcall SetMaxHistorySize(int AMaxHistorySize);
	bool __fastcall StoreSaveOn(void);
	int __fastcall GetMaxItemWidth(void);
	
protected:
	DYNAMIC void __fastcall DoExit(void);
	DYNAMIC void __fastcall DropDown(void);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	DYNAMIC void __fastcall Change(void);
	
public:
	__fastcall virtual THistoryComboBox(Classes::TComponent* AOwner);
	virtual void __fastcall SaveToHistory(void);
	
__published:
	__property THistorySaveOn SaveOn = {read=FSaveOn, write=FSaveOn, stored=StoreSaveOn, nodefault};
	__property int MaxHistorySize = {read=FMaxHistorySize, write=SetMaxHistorySize, default=30};
	__property THistoryComboBoxGetData OnGetData = {read=FOnGetData, write=FOnGetData};
	__property THistoryComboBoxSetData OnSetData = {read=FOnSetData, write=FOnSetData};
public:
	#pragma option push -w-inl
	/* TCustomComboBox.Destroy */ inline __fastcall virtual ~THistoryComboBox(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall THistoryComboBox(HWND ParentWindow) : Stdctrls::TComboBox(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
#define DefaultHistorySaveOn (System::Set<HistoryComboBox__1, soExit, soDropDown> () << HistoryComboBox__1(0) << HistoryComboBox__1(1) )
static const Shortint DefaultMaxHistorySize = 0x1e;
extern PACKAGE void __fastcall Register(void);
extern PACKAGE void __fastcall SaveToHistory(Classes::TStrings* Strings, AnsiString T, void * Data = (void *)(0x0), int MaxHistorySize = 0x1e);

}	/* namespace Historycombobox */
using namespace Historycombobox;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// HistoryComboBox
