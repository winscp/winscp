// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DragDropFiles.pas' rev: 6.00

#ifndef DragDropFilesHPP
#define DragDropFilesHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ActiveX.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <DragDrop.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dragdropfiles
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

class DELPHICLASS TDataObjectFiles;
class PASCALIMPLEMENTATION TDataObjectFiles : public Dragdrop::TDataObject 
{
	typedef Dragdrop::TDataObject inherited;
	
private:
	Classes::TMemoryStream* HDropStream;
	
public:
	__fastcall TDataObjectFiles(Classes::TStringList* StringList);
	__fastcall virtual ~TDataObjectFiles(void);
	virtual HRESULT __fastcall RenderData(const tagFORMATETC &FormatEtc, tagSTGMEDIUM &StgMedium);
};


class DELPHICLASS TDropTargetFiles;
class PASCALIMPLEMENTATION TDropTargetFiles : public Dragdrop::TDropTarget 
{
	typedef Dragdrop::TDropTarget inherited;
	
protected:
	virtual void __fastcall AcceptDataObject(_di_IDataObject DataObj, bool &Accept);
	
public:
	__fastcall TDropTargetFiles(Dragdrop::TDragDrop* AOwner);
	__fastcall virtual ~TDropTargetFiles(void);
	virtual void __fastcall RenderDropped(_di_IDataObject DataObj, int grfKeyState, const Types::TPoint &pt, int &dwEffect);
};


class DELPHICLASS TDragDropFiles;
class PASCALIMPLEMENTATION TDragDropFiles : public Dragdrop::TDragDrop 
{
	typedef Dragdrop::TDragDrop inherited;
	
private:
	Classes::TStringList* FFileList;
	
protected:
	virtual Dragdrop::TDataObject* __fastcall CreateDataObject(void);
	
public:
	__fastcall virtual TDragDropFiles(Classes::TComponent* AOwner);
	__fastcall virtual ~TDragDropFiles(void);
	__property Classes::TStringList* FileList = {read=FFileList, write=FFileList};
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Dragdropfiles */
using namespace Dragdropfiles;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DragDropFiles
