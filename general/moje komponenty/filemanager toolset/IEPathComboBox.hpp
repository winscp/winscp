// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'IEPathComboBox.pas' rev: 6.00

#ifndef IEPathComboBoxHPP
#define IEPathComboBoxHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Menus.hpp>	// Pascal unit
#include <DirView.hpp>	// Pascal unit
#include <IEComboBox.hpp>	// Pascal unit
#include <IEDriveInfo.hpp>	// Pascal unit
#include <CustomPathComboBox.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <ShellAPI.hpp>	// Pascal unit
#include <CommCtrl.hpp>	// Pascal unit
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

namespace Iepathcombobox
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TDirectoryToSelect { dsCurrent, dsRoot };
#pragma option pop

#pragma option push -b-
enum TDriveType { dtUnknown, dtNoRootDrive, dtFloppy, dtFixed, dtRemote, dtCDROM, dtRAM };
#pragma option pop

typedef Set<TDriveType, dtUnknown, dtRAM>  TDriveTypes;

class DELPHICLASS TIEPathComboBox;
class PASCALIMPLEMENTATION TIEPathComboBox : public Custompathcombobox::TCustomPathComboBox 
{
	typedef Custompathcombobox::TCustomPathComboBox inherited;
	
private:
	TDirectoryToSelect FDirectoryToSelect;
	char FDrive;
	Dirview::TVolumeDisplayStyle FDisplayStyle;
	TDriveTypes FDriveTypes;
	bool FDontNotifyPathChange;
	void __fastcall SetDisplayStyle(Dirview::TVolumeDisplayStyle Value);
	void __fastcall SetDrive(char Value);
	bool __fastcall DriveStored(void);
	char __fastcall GetFocusedDrive(void);
	char __fastcall GetItemDrive(int Index);
	
protected:
	virtual void __fastcall CreateWnd(void);
	DYNAMIC void __fastcall DropDown(void);
	virtual void __fastcall SetDriveTypes(TDriveTypes Value);
	virtual int __fastcall GetItemImage(int Index);
	virtual AnsiString __fastcall GetItemTextEx(int Index, bool ForList);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	virtual void __fastcall PathChanged(void);
	virtual void __fastcall SetPath(AnsiString Value);
	
public:
	__fastcall virtual TIEPathComboBox(Classes::TComponent* AOwner);
	int __fastcall GetDriveIndex(char Drive);
	void __fastcall ResetItems(void);
	__property char FocusedDrive = {read=GetFocusedDrive, nodefault};
	__property char ItemDrive[int Index] = {read=GetItemDrive};
	
__published:
	__property char Drive = {read=FDrive, write=SetDrive, stored=DriveStored, nodefault};
	__property TDirectoryToSelect DirectoryToSelect = {read=FDirectoryToSelect, write=FDirectoryToSelect, default=1};
	__property Dirview::TVolumeDisplayStyle DisplayStyle = {read=FDisplayStyle, write=SetDisplayStyle, default=0};
	__property TDriveTypes DriveTypes = {read=FDriveTypes, write=SetDriveTypes, default=124};
	__property DropDownFixedWidth  = {default=0};
	__property OnCloseUp ;
	__property ShowFullPath  = {default=0};
	__property Align  = {default=0};
	__property Anchors  = {default=3};
	__property BiDiMode ;
	__property Color  = {default=-2147483643};
	__property Constraints ;
	__property Ctl3D ;
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property DropDownCount  = {default=8};
	__property Enabled  = {default=1};
	__property Font ;
	__property ImeMode  = {default=3};
	__property ImeName ;
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=0};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ShowHint ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property Visible  = {default=1};
	__property OnChange ;
	__property OnClick ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnDrawItem ;
	__property OnDropDown ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TIECustomComboBox.Destroy */ inline __fastcall virtual ~TIEPathComboBox(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TIEPathComboBox(HWND ParentWindow) : Custompathcombobox::TCustomPathComboBox(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
#define DefaultDriveTypes (System::Set<TDriveType, dtUnknown, dtRAM> () << TDriveType(2) << TDriveType(3) << TDriveType(4) << TDriveType(5) << TDriveType(6) )
extern PACKAGE void __fastcall Register(void);

}	/* namespace Iepathcombobox */
using namespace Iepathcombobox;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// IEPathComboBox
