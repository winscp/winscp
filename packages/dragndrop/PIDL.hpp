// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'PIDL.pas' rev: 6.00

#ifndef PIDLHPP
#define PIDLHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ActiveX.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <ShlObj.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Pidl
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE _di_IMalloc ShellMalloc;
extern PACKAGE unsigned CF_FILECONTENTS;
extern PACKAGE unsigned CF_FILEDESCRIPTOR;
extern PACKAGE unsigned CF_FILENAME;
extern PACKAGE unsigned CF_FILENAMEMAP;
extern PACKAGE unsigned CF_FILENAMEMAPW;
extern PACKAGE unsigned CF_INDRAGLOOP;
extern PACKAGE unsigned CF_NETRESOURCES;
extern PACKAGE unsigned CF_PASTESUCCEEDED;
extern PACKAGE unsigned CF_PERFORMEDDROPEFFECT;
extern PACKAGE unsigned CF_PREFERREDDROPEFFECT;
extern PACKAGE unsigned CF_PRINTERGROUP;
extern PACKAGE unsigned CF_SHELLIDLIST;
extern PACKAGE unsigned CF_SHELLIDLISTOFFSET;
extern PACKAGE unsigned CF_SHELLURL;
extern PACKAGE Shlobj::PItemIDList __fastcall PIDL_GetNextItem(Shlobj::PItemIDList PIDL);
extern PACKAGE int __fastcall PIDL_GetSize(Shlobj::PItemIDList pidl);
extern PACKAGE Shlobj::PItemIDList __fastcall PIDL_Create(unsigned Size);
extern PACKAGE Shlobj::PItemIDList __fastcall PIDL_Concatenate(Shlobj::PItemIDList pidl1, Shlobj::PItemIDList pidl2);
extern PACKAGE Shlobj::PItemIDList __fastcall PIDL_Copy(Shlobj::PItemIDList pidlSource);
extern PACKAGE bool __fastcall PIDL_GetDisplayName(_di_IShellFolder piFolder, Shlobj::PItemIDList pidl, unsigned dwFlags, char * pszName, unsigned cchMax);
extern PACKAGE Shlobj::PItemIDList __fastcall Pidl_GetFullyQualified(const _di_IShellFolder PiParentFolder, Shlobj::PItemIDList pidl);
extern PACKAGE void __fastcall PIDL_GetRelative(Shlobj::PItemIDList &pidlFQ, Shlobj::PItemIDList &ppidlRoot, Shlobj::PItemIDList &ppidlItem);
extern PACKAGE Shlobj::PItemIDList __fastcall PIDL_GetFromPath(char * pszFile);
extern PACKAGE bool __fastcall PIDL_GetFileFolder(Shlobj::PItemIDList pidl, _di_IShellFolder &piFolder);
extern PACKAGE Shlobj::PItemIDList __fastcall PIDL_GetFromParentFolder(_di_IShellFolder pParentFolder, char * pszFile);
extern PACKAGE void __fastcall PIDL_Free(Shlobj::PItemIDList PIDL);
extern PACKAGE bool __fastcall PIDL_Equal(Shlobj::PItemIDList PIDL1, Shlobj::PItemIDList PIDL2);

}	/* namespace Pidl */
using namespace Pidl;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// PIDL
