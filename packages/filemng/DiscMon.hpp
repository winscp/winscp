// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DiscMon.pas' rev: 6.00

#ifndef DiscMonHPP
#define DiscMonHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <CompThread.hpp>	// Pascal unit
#include <ActiveX.hpp>	// Pascal unit
#include <ShlObj.hpp>	// Pascal unit
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

namespace Discmon
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDiscMonitorThread;
class PASCALIMPLEMENTATION TDiscMonitorThread : public Compthread::TCompThread 
{
	typedef Compthread::TCompThread inherited;
	
private:
	Classes::TNotifyEvent FOnChange;
	Classes::TNotifyEvent FOnInvalid;
	AnsiString FDirectory;
	unsigned FFilters;
	unsigned FDestroyEvent;
	unsigned FChangeEvent;
	bool FSubTree;
	int fChangeDelay;
	void __fastcall InformChange(void);
	void __fastcall InformInvalid(void);
	void __fastcall SetDirectory(const AnsiString Value);
	void __fastcall SetFilters(unsigned Value);
	void __fastcall SetSubTree(bool Value);
	
protected:
	virtual void __fastcall Execute(void);
	void __fastcall Update(void);
	
public:
	__fastcall TDiscMonitorThread(void);
	__fastcall virtual ~TDiscMonitorThread(void);
	__property AnsiString Directory = {read=FDirectory, write=SetDirectory};
	__property unsigned Filters = {read=FFilters, write=SetFilters, nodefault};
	__property Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property Classes::TNotifyEvent OnInvalid = {read=FOnInvalid, write=FOnInvalid};
	__property bool SubTree = {read=FSubTree, write=SetSubTree, nodefault};
	__property int ChangeDelay = {read=fChangeDelay, write=fChangeDelay, default=500};
};


typedef AnsiString TDiscMonitorDirStr;

#pragma option push -b-
enum TMonitorFilter { moFilename, moDirName, moAttributes, moSize, moLastWrite, moSecurity };
#pragma option pop

typedef Set<TMonitorFilter, moFilename, moSecurity>  TMonitorFilters;

class DELPHICLASS TDiscMonitor;
class PASCALIMPLEMENTATION TDiscMonitor : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	bool FActive;
	TDiscMonitorThread* FMonitor;
	TMonitorFilters FFilters;
	Classes::TNotifyEvent FOnChange;
	Classes::TNotifyEvent FOnInvalid;
	bool FShowMsg;
	AnsiString __fastcall GetDirectory();
	bool __fastcall GetSubTree(void);
	void __fastcall SetActive(bool Value);
	void __fastcall SetDirectory(AnsiString Value);
	void __fastcall SetFilters(TMonitorFilters Value);
	void __fastcall SetSubTree(bool Value);
	int __fastcall GetChangeDelay(void);
	void __fastcall SetChangeDelay(int Value);
	
protected:
	void __fastcall Change(System::TObject* Sender);
	void __fastcall Invalid(System::TObject* Sender);
	
public:
	__fastcall virtual TDiscMonitor(Classes::TComponent* AOwner);
	__fastcall virtual ~TDiscMonitor(void);
	void __fastcall Close(void);
	void __fastcall Open(void);
	__property TDiscMonitorThread* Thread = {read=FMonitor};
	
__published:
	__property AnsiString Directory = {read=GetDirectory, write=SetDirectory};
	__property bool ShowDesignMsg = {read=FShowMsg, write=FShowMsg, default=0};
	__property Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property Classes::TNotifyEvent OnInvalid = {read=FOnInvalid, write=FOnInvalid};
	__property TMonitorFilters Filters = {read=FFilters, write=SetFilters, default=1};
	__property bool SubTree = {read=GetSubTree, write=SetSubTree, default=1};
	__property bool Active = {read=FActive, write=SetActive, default=0};
	__property int ChangeDelay = {read=GetChangeDelay, write=SetChangeDelay, default=500};
};


#pragma option push -b
enum TWinBool { winFalse, winTrue };
#pragma option pop

//-- var, const, procedure ---------------------------------------------------
extern "C" unsigned __stdcall FixFindFirstChangeNotification(const char * lpPathName, TWinBool bWatchSubtree, unsigned dwNotifyFilter);
extern PACKAGE void __fastcall Register(void);

}	/* namespace Discmon */
using namespace Discmon;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DiscMon
