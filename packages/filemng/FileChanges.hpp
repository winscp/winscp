// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FileChanges.pas' rev: 6.00

#ifndef FileChangesHPP
#define FileChangesHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <CompThread.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Filechanges
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TFileDeleteEvent)(System::TObject* Sender, Classes::TStringList* Files);

class DELPHICLASS TFileDeleteThread;
class PASCALIMPLEMENTATION TFileDeleteThread : public Compthread::TCompThread 
{
	typedef Compthread::TCompThread inherited;
	
private:
	TFileDeleteEvent fOnSignalDelete;
	Classes::TStringList* fFiles;
	Classes::TStringList* fDelFiles;
	_FILETIME fEndTime;
	
protected:
	virtual void __fastcall Execute(void);
	virtual void __fastcall DoTerminate(void);
	void __fastcall DoOnSignalDelete(void);
	
public:
	__fastcall TFileDeleteThread(Classes::TStringList* Files, unsigned TimeOut, TFileDeleteEvent SignalProc);
	__property Terminated ;
	
__published:
	__property TFileDeleteEvent OnSignalDelete = {read=fOnSignalDelete, write=fOnSignalDelete};
public:
	#pragma option push -w-inl
	/* TCompThread.Destroy */ inline __fastcall virtual ~TFileDeleteThread(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Filechanges */
using namespace Filechanges;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FileChanges
