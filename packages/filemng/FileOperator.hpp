// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FileOperator.pas' rev: 6.00

#ifndef FileOperatorHPP
#define FileOperatorHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SysUtils.hpp>	// Pascal unit
#include <BaseUtils.hpp>	// Pascal unit
#include <ShellAPI.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fileoperator
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TFileOperation { foCopy, foDelete, foMove, foRename };
#pragma option pop

#pragma option push -b-
enum TFileOperationFlag { foAllowUndo, foConfirmMouse, foFilesOnly, foMultiDestFiles, foNoConfirmation, foNoConfirmMkDir, foRenameOnCollision, foSilent, foSimpleProgress };
#pragma option pop

typedef Set<TFileOperationFlag, foAllowUndo, foSimpleProgress>  TFileOperationFlags;

class DELPHICLASS TFileOperator;
class PASCALIMPLEMENTATION TFileOperator : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	#pragma pack(push, 1)
	_SHFILEOPSTRUCTA FData;
	#pragma pack(pop)
	
	Classes::TStringList* FFrom;
	Classes::TStringList* FTo;
	Classes::TStringList* FLastFrom;
	Classes::TStringList* FLastTo;
	TFileOperation FLastOperation;
	TFileOperationFlags fLastFlags;
	bool fCanUndo;
	AnsiString FProgressTitle;
	Classes::TComponent* FOwner;
	void __fastcall SetOperation(TFileOperation Value);
	TFileOperation __fastcall GetOperation(void);
	bool __fastcall GetWantMappingHandle(void);
	void __fastcall SetWantMappingHandle(bool Value);
	void __fastcall SetFlags(TFileOperationFlags Value);
	TFileOperationFlags __fastcall GetFlags(void);
	bool __fastcall GetOperFlag(unsigned F);
	void __fastcall SetOperFlag(unsigned F, bool V);
	void __fastcall ReadData(Classes::TReader* Reader);
	void __fastcall WriteData(Classes::TWriter* Writer);
	void __fastcall SwapStringList(Classes::TStringList* &FromL, Classes::TStringList* &ToL);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	
public:
	__property void * NameMappings = {read=FData.hNameMappings};
	__property BOOL OperationAborted = {read=FData.fAnyOperationsAborted, nodefault};
	__property Classes::TStringList* OperandFrom = {read=FFrom, write=FFrom};
	__property Classes::TStringList* OperandTo = {read=FTo, write=FTo};
	__property bool CanUndo = {read=fCanUndo, nodefault};
	__property TFileOperation LastOperation = {read=FLastOperation, nodefault};
	__property Classes::TStringList* LastOperandFrom = {read=FLastFrom};
	__property Classes::TStringList* LastOperandTo = {read=FLastTo};
	__fastcall virtual TFileOperator(Classes::TComponent* aOwner);
	__fastcall virtual ~TFileOperator(void);
	bool __fastcall Execute(void);
	bool __fastcall UndoExecute(void);
	void __fastcall ClearUndo(void);
	
__published:
	__property TFileOperation Operation = {read=GetOperation, write=SetOperation, stored=false, nodefault};
	__property TFileOperationFlags Flags = {read=GetFlags, write=SetFlags, stored=false, nodefault};
	__property AnsiString ProgressTitle = {read=FProgressTitle, write=FProgressTitle};
	__property bool WantMappingHandle = {read=GetWantMappingHandle, write=SetWantMappingHandle, stored=false, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::ResourceString _SFileOperation;
#define Fileoperator_SFileOperation System::LoadResourceString(&Fileoperator::_SFileOperation)
extern PACKAGE void __fastcall Register(void);

}	/* namespace Fileoperator */
using namespace Fileoperator;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FileOperator
