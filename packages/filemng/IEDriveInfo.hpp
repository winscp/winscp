// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'IEDriveInfo.pas' rev: 6.00

#ifndef IEDriveInfoHPP
#define IEDriveInfoHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <BaseUtils.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <CommCtrl.hpp>	// Pascal unit
#include <ShlObj.hpp>	// Pascal unit
#include <ShellAPI.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Registry.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Iedriveinfo
{
//-- type declarations -------------------------------------------------------
typedef char TDrive;

struct TDriveInfoRec
{
	_ITEMIDLIST *PIDL;
	bool Init;
	bool Valid;
	bool DriveReady;
	int DriveType;
	AnsiString DisplayName;
	AnsiString Prettyname;
	AnsiString LongPrettyName;
	unsigned DriveSerial;
	__int64 Size;
	int ImageIndex;
	AnsiString FileSystemName;
	unsigned MaxFileNameLength;
	unsigned FileSystemFlags;
} ;

typedef TDriveInfoRec IEDriveInfo__2[26];

class DELPHICLASS TDriveInfo;
class PASCALIMPLEMENTATION TDriveInfo : public System::TObject 
{
	typedef System::TObject inherited;
	
public:
	TDriveInfoRec operator[](char Drive) { return Data[Drive]; }
	
private:
	TDriveInfoRec FData[26];
	unsigned FNoDrives;
	_di_IShellFolder FDesktop;
	TDriveInfoRec __fastcall GetData(char Drive);
	
public:
	__property TDriveInfoRec Data[char Drive] = {read=GetData/*, default*/};
	int __fastcall GetImageIndex(char Drive);
	AnsiString __fastcall GetDisplayName(char Drive);
	AnsiString __fastcall GetPrettyName(char Drive);
	AnsiString __fastcall GetLongPrettyName(char Drive);
	bool __fastcall ReadDriveStatus(char Drive, int Flags);
	__fastcall TDriveInfo(void);
	__fastcall virtual ~TDriveInfo(void);
	void __fastcall Load(void);
};


//-- var, const, procedure ---------------------------------------------------
static const Shortint dsValid = 0x0;
static const Shortint dsImageIndex = 0x1;
static const Shortint dsSize = 0x2;
static const Shortint dsDisplayName = 0x4;
static const Shortint dsAll = 0x7;
static const char FirstDrive = '\x41';
static const char FirstFixedDrive = '\x43';
static const char LastDrive = '\x5a';
extern PACKAGE TDriveInfo* DriveInfo;
extern PACKAGE System::ResourceString _ErrorInvalidDrive;
#define Iedriveinfo_ErrorInvalidDrive System::LoadResourceString(&Iedriveinfo::_ErrorInvalidDrive)
extern PACKAGE AnsiString __fastcall GetShellFileName(const AnsiString Name)/* overload */;
extern PACKAGE AnsiString __fastcall GetShellFileName(Shlobj::PItemIDList PIDL)/* overload */;
extern PACKAGE AnsiString __fastcall GetNetWorkName(char Drive);

}	/* namespace Iedriveinfo */
using namespace Iedriveinfo;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// IEDriveInfo
