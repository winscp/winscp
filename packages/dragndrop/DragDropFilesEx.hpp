// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DragDropFilesEx.pas' rev: 6.00

#ifndef DragDropFilesExHPP
#define DragDropFilesExHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Registry.hpp>	// Pascal unit
#include <ComObj.hpp>	// Pascal unit
#include <ShlObj.hpp>	// Pascal unit
#include <PIDL.hpp>	// Pascal unit
#include <ActiveX.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <DragDrop.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dragdropfilesex
{
//-- type declarations -------------------------------------------------------
struct TDropFiles;
typedef TDropFiles *PDropFiles;

#pragma pack(push, 1)
struct TDropFiles
{
	unsigned pFiles;
	Types::TPoint pt;
	BOOL fNC;
	BOOL fWide;
} ;
#pragma pack(pop)

typedef _ITEMIDLIST *PItemIDList;

#pragma option push -b-
enum TFileExMustDnD { nvFilename, nvPIDL };
#pragma option pop

typedef Set<TFileExMustDnD, nvFilename, nvPIDL>  TFileExMustDnDSet;

typedef void __fastcall (__closure *TOnSpecifyDropTarget)(System::TObject* Sender, bool DragDropHandler, const Types::TPoint &pt, Shlobj::PItemIDList &pidlFQ, AnsiString &Filename);

struct TFDDListItem;
typedef TFDDListItem *PFDDListItem;

#pragma pack(push, 4)
struct TFDDListItem
{
	_ITEMIDLIST *pidlFQ;
	AnsiString Name;
	AnsiString MappedName;
} ;
#pragma pack(pop)

struct TCMListItem;
typedef TCMListItem *PCMListItem;

#pragma pack(push, 4)
struct TCMListItem
{
	int FirstCmd;
	int LastCmd;
	_di_IContextMenu CM;
} ;
#pragma pack(pop)

class DELPHICLASS TFileList;
class PASCALIMPLEMENTATION TFileList : public Classes::TList 
{
	typedef Classes::TList inherited;
	
private:
	HIDESBASE PFDDListItem __fastcall Get(int Index);
	HIDESBASE void __fastcall Put(int Index, PFDDListItem Item);
	
public:
	__fastcall TFileList(void);
	__fastcall virtual ~TFileList(void);
	virtual void __fastcall Clear(void);
	HIDESBASE void __fastcall Delete(int Index);
	HIDESBASE int __fastcall Remove(PFDDListItem Item);
	HIDESBASE PFDDListItem __fastcall First(void);
	HIDESBASE PFDDListItem __fastcall Last(void);
	int __fastcall AddItem(Shlobj::PItemIDList ApidlFQ, AnsiString AName);
	int __fastcall AddItemEx(Shlobj::PItemIDList ApidlFQ, AnsiString AName, AnsiString AMappedName);
	bool __fastcall RenderPIDLs(void);
	bool __fastcall RenderNames(void);
	__property PFDDListItem Items[int Index] = {read=Get, write=Put};
};


class DELPHICLASS TDataObjectFilesEx;
class PASCALIMPLEMENTATION TDataObjectFilesEx : public Dragdrop::TDataObject 
{
	typedef Dragdrop::TDataObject inherited;
	
private:
	Classes::TMemoryStream* pidlStream;
	Classes::TMemoryStream* HDropStream;
	Classes::TStringList* FilenameMapList;
	bool FilenamesAreMapped;
	
public:
	__fastcall TDataObjectFilesEx(TFileList* AFileList, bool RenderPIDL, bool RenderFilename);
	__fastcall virtual ~TDataObjectFilesEx(void);
	virtual HRESULT __fastcall RenderData(const tagFORMATETC &FormatEtc, tagSTGMEDIUM &StgMedium);
	bool __fastcall IsValid(bool Formatpidl, bool FormatHDrop);
};


class DELPHICLASS TDropTargetFilesEx;
class PASCALIMPLEMENTATION TDropTargetFilesEx : public Dragdrop::TDropTarget 
{
	typedef Dragdrop::TDropTarget inherited;
	
protected:
	virtual void __fastcall AcceptDataObject(_di_IDataObject DataObj, bool &Accept);
	
public:
	__fastcall TDropTargetFilesEx(Dragdrop::TDragDrop* AOwner);
	__fastcall virtual ~TDropTargetFilesEx(void);
	virtual void __fastcall RenderDropped(_di_IDataObject DataObj, int grfKeyState, const Types::TPoint &pt, int &dwEffect);
};


class DELPHICLASS TShellExtension;
class PASCALIMPLEMENTATION TShellExtension : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;
	
private:
	bool FDropHandler;
	bool FDragDropHandler;
	
protected:
	virtual void __fastcall AssignTo(Classes::TPersistent* Dest);
	
__published:
	__property bool DropHandler = {read=FDropHandler, write=FDropHandler, default=0};
	__property bool DragDropHandler = {read=FDragDropHandler, write=FDragDropHandler, default=0};
public:
	#pragma option push -w-inl
	/* TPersistent.Destroy */ inline __fastcall virtual ~TShellExtension(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TShellExtension(void) : Classes::TPersistent() { }
	#pragma option pop
	
};


class DELPHICLASS TDragDropFilesEx;
class PASCALIMPLEMENTATION TDragDropFilesEx : public Dragdrop::TDragDrop 
{
	typedef Dragdrop::TDragDrop inherited;
	
private:
	TFileList* FFileList;
	TFileExMustDnDSet FNeedValid;
	bool FCompleteFileList;
	bool FFileNamesAreMapped;
	TOnSpecifyDropTarget FOnSpecifyDropTarget;
	TShellExtension* FShellExtension;
	Classes::TList* FCMList;
	
protected:
	virtual Dragdrop::TDataObject* __fastcall CreateDataObject(void);
	virtual void __fastcall DoMenuPopup(System::TObject* Sender, HMENU AMenu, _di_IDataObject DataObj, int AMinCustCmd, int grfKeyState, const Types::TPoint &pt);
	virtual bool __fastcall DoMenuExecCmd(System::TObject* Sender, HMENU AMenu, _di_IDataObject DataObj, int Command, int &dwEffect);
	virtual void __fastcall DoMenuDestroy(System::TObject* Sender, HMENU AMenu);
	virtual bool __fastcall DropHandler(const _di_IDataObject dataObj, int grfKeyState, const Types::TPoint &pt, int &dwEffect);
	
public:
	__fastcall virtual TDragDropFilesEx(Classes::TComponent* AOwner);
	__fastcall virtual ~TDragDropFilesEx(void);
	bool __fastcall TargetHasDropHandler(Shlobj::PItemIDList pidlFQ, AnsiString Filename, int &dwEffect);
	__property TFileList* FileList = {read=FFileList, write=FFileList};
	__property bool FileNamesAreMapped = {read=FFileNamesAreMapped, nodefault};
	
__published:
	__property TFileExMustDnDSet NeedValid = {read=FNeedValid, write=FNeedValid, nodefault};
	__property bool CompleteFileList = {read=FCompleteFileList, write=FCompleteFileList, default=1};
	__property TShellExtension* ShellExtensions = {read=FShellExtension, write=FShellExtension};
	__property TOnSpecifyDropTarget OnSpecifyDropTarget = {read=FOnSpecifyDropTarget, write=FOnSpecifyDropTarget};
	__property OnDropHandlerSucceeded ;
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Dragdropfilesex */
using namespace Dragdropfilesex;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DragDropFilesEx
