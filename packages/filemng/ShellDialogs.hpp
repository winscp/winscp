// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ShellDialogs.pas' rev: 6.00

#ifndef ShellDialogsHPP
#define ShellDialogsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <PIDL.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <ShlObj.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Shelldialogs
{
//-- type declarations -------------------------------------------------------
typedef _ITEMIDLIST *TPIDLArray[1];

typedef Shlobj::PItemIDList *PPIDLArray;

//-- var, const, procedure ---------------------------------------------------
#define shcProperties "properties"
#define shcCut "cut"
#define shcCopy "copy"
#define shcPaste "paste"
#define shcDelete "delete"
#define shcLink "link"
#define shcrename "rename"
#define shcDefault ""
extern PACKAGE Menus::TPopupMenu* CustomContextMenu;
extern PACKAGE void __fastcall ShellDisplayContextMenu(unsigned Handle, const Types::TPoint &P, _di_IShellFolder ShellFolder, int PIDLCount, Shlobj::PItemIDList &PIDL, bool AllowRename, AnsiString &Verb, bool PerformPaste = true)/* overload */;
extern PACKAGE void __fastcall ShellDisplayContextMenu(unsigned Handle, const Types::TPoint &P, AnsiString FileName, bool AllowRename, AnsiString &Verb, bool PerformPaste = true)/* overload */;
extern PACKAGE void __fastcall ShellDisplayContextMenu(unsigned Handle, const Types::TPoint &P, AnsiString Path, Classes::TStringList* Files, AnsiString &Verb, bool PerformPaste = true)/* overload */;
extern PACKAGE bool __fastcall ShellExecuteContextCommand(unsigned Handle, AnsiString Command, _di_IShellFolder ShellFolder, int PIDLCount, Shlobj::PItemIDList &PIDL)/* overload */;
extern PACKAGE bool __fastcall ShellExecuteContextCommand(unsigned Handle, AnsiString Command, AnsiString FileName)/* overload */;
extern PACKAGE bool __fastcall ShellExecuteContextCommand(unsigned Handle, AnsiString Command, AnsiString Path, Classes::TStringList* Files)/* overload */;

}	/* namespace Shelldialogs */
using namespace Shelldialogs;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ShellDialogs
