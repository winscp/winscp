// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'BaseUtils.pas' rev: 6.00

#ifndef BaseUtilsHPP
#define BaseUtilsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Controls.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <PIDL.hpp>	// Pascal unit
#include <ShlObj.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Baseutils
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TDateTimePrecision { tpNone, tpDay, tpMinute, tpSecond, tpMillisecond };
#pragma option pop

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::ResourceString _SNoValidPath;
#define Baseutils_SNoValidPath System::LoadResourceString(&Baseutils::_SNoValidPath)
extern PACKAGE System::ResourceString _SUcpPathsNotSupported;
#define Baseutils_SUcpPathsNotSupported System::LoadResourceString(&Baseutils::_SUcpPathsNotSupported)
extern PACKAGE AnsiString __fastcall AnyValidPath();
extern PACKAGE bool __fastcall IsUncPath(AnsiString Path);
extern PACKAGE void __fastcall StrTranslate(AnsiString &Str, AnsiString Code);
extern PACKAGE bool __fastcall StrContains(AnsiString Str1, AnsiString Str2);
extern PACKAGE bool __fastcall FileOrDirExists(AnsiString FileName);
extern PACKAGE bool __fastcall CheckFileExists(AnsiString FileName);
extern PACKAGE bool __fastcall DirExists(AnsiString Dir, int &Attrs)/* overload */;
extern PACKAGE bool __fastcall DirExists(AnsiString Dir)/* overload */;
extern PACKAGE AnsiString __fastcall ExtractFileNameOnly(AnsiString Name);
extern PACKAGE AnsiString __fastcall FormatSize(int Size)/* overload */;
extern PACKAGE AnsiString __fastcall FormatSize(unsigned Size)/* overload */;
extern PACKAGE AnsiString __fastcall FormatSize(__int64 Size)/* overload */;
extern PACKAGE void __fastcall FreePIDL(Shlobj::PItemIDList &PIDL);
extern PACKAGE __int64 __fastcall DiskSize(Byte Drive);
extern PACKAGE void __fastcall ReduceDateTimePrecision(System::TDateTime &DateTime, TDateTimePrecision Precision);
extern PACKAGE bool __fastcall SpecialFolderLocation(int Folder, AnsiString &Path, Shlobj::PItemIDList &PIDL)/* overload */;
extern PACKAGE bool __fastcall SpecialFolderLocation(int Folder, AnsiString &Path)/* overload */;
extern PACKAGE Controls::TImageList* __fastcall ShellImageList(Classes::TComponent* Owner, unsigned Flags);
extern PACKAGE void __fastcall Trace(AnsiString Msg);

}	/* namespace Baseutils */
using namespace Baseutils;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// BaseUtils
