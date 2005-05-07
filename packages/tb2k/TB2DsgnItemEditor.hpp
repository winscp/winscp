// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TB2DsgnItemEditor.pas' rev: 6.00

#ifndef TB2DsgnItemEditorHPP
#define TB2DsgnItemEditorHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DesignEditors.hpp>	// Pascal unit
#include <DesignWindows.hpp>	// Pascal unit
#include <DesignIntf.hpp>	// Pascal unit
#include <TB2Dock.hpp>	// Pascal unit
#include <TB2Toolbar.hpp>	// Pascal unit
#include <TB2Item.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <Buttons.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
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

namespace Tb2dsgnitemeditor
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTBItemEditForm;
class PASCALIMPLEMENTATION TTBItemEditForm : public Designwindows::TDesignWindow 
{
	typedef Designwindows::TDesignWindow inherited;
	
__published:
	Comctrls::TTreeView* TreeView;
	Comctrls::TListView* ListView;
	Extctrls::TSplitter* Splitter1;
	Tb2toolbar::TTBToolbar* Toolbar;
	Tb2item::TTBItem* NewSubmenuButton;
	Tb2item::TTBItem* NewItemButton;
	Tb2item::TTBItem* NewSepButton;
	Tb2item::TTBItem* DeleteButton;
	Tb2item::TTBSeparatorItem* TBSeparatorItem1;
	Tb2item::TTBPopupMenu* TBPopupMenu1;
	Tb2item::TTBItemContainer* TBItemContainer1;
	Tb2item::TTBSubmenuItem* ToolbarItems;
	Tb2item::TTBItem* CopyButton;
	Tb2item::TTBItem* CutButton;
	Tb2item::TTBItem* PasteButton;
	Tb2item::TTBSubmenuItem* MoreMenu;
	Tb2item::TTBSeparatorItem* TBSeparatorItem2;
	Tb2item::TTBSubmenuItem* TBSubmenuItem1;
	Tb2item::TTBItem* TConvertMenu;
	Tb2item::TTBSeparatorItem* TBSeparatorItem3;
	Tb2item::TTBItem* MoveUpButton;
	Tb2item::TTBItem* MoveDownButton;
	void __fastcall FormClose(System::TObject* Sender, Forms::TCloseAction &Action);
	void __fastcall TreeViewChange(System::TObject* Sender, Comctrls::TTreeNode* Node);
	void __fastcall NewSubmenuButtonClick(System::TObject* Sender);
	void __fastcall NewItemButtonClick(System::TObject* Sender);
	void __fastcall ListViewChange(System::TObject* Sender, Comctrls::TListItem* Item, Comctrls::TItemChange Change);
	void __fastcall DeleteButtonClick(System::TObject* Sender);
	void __fastcall NewSepButtonClick(System::TObject* Sender);
	void __fastcall ListViewDragOver(System::TObject* Sender, System::TObject* Source, int X, int Y, Controls::TDragState State, bool &Accept);
	void __fastcall ListViewDragDrop(System::TObject* Sender, System::TObject* Source, int X, int Y);
	void __fastcall TreeViewEnter(System::TObject* Sender);
	void __fastcall TreeViewDragDrop(System::TObject* Sender, System::TObject* Source, int X, int Y);
	void __fastcall TreeViewDragOver(System::TObject* Sender, System::TObject* Source, int X, int Y, Controls::TDragState State, bool &Accept);
	void __fastcall CopyButtonClick(System::TObject* Sender);
	void __fastcall ListViewKeyDown(System::TObject* Sender, Word &Key, Classes::TShiftState Shift);
	void __fastcall CutButtonClick(System::TObject* Sender);
	void __fastcall PasteButtonClick(System::TObject* Sender);
	void __fastcall FormActivate(System::TObject* Sender);
	void __fastcall ListViewKeyPress(System::TObject* Sender, char &Key);
	void __fastcall ListViewDblClick(System::TObject* Sender);
	void __fastcall ListViewEnter(System::TObject* Sender);
	void __fastcall TreeViewKeyDown(System::TObject* Sender, Word &Key, Classes::TShiftState Shift);
	void __fastcall TConvertMenuClick(System::TObject* Sender);
	void __fastcall TreeViewKeyPress(System::TObject* Sender, char &Key);
	void __fastcall MoveUpButtonClick(System::TObject* Sender);
	void __fastcall MoveDownButtonClick(System::TObject* Sender);
	
private:
	Classes::TComponent* FParentComponent;
	Tb2item::TTBCustomItem* FRootItem;
	Tb2item::TTBCustomItem* FSelParentItem;
	Classes::TList* FNotifyItemList;
	int FSettingSel;
	int FRebuildingTree;
	int FRebuildingList;
	Comctrls::TListItem* __fastcall AddListViewItem(const int Index, const Tb2item::TTBCustomItem* Item);
	void __fastcall Copy(void);
	void __fastcall CreateNewItem(const TMetaClass* AClass);
	void __fastcall Cut(void);
	void __fastcall Delete(void);
	void __fastcall DeleteItem(const Tb2item::TTBCustomItem* Item);
	AnsiString __fastcall GetItemTreeCaption(Tb2item::TTBCustomItem* AItem);
	void __fastcall GetSelItemList(const Classes::TList* AList);
	void __fastcall ItemNotification(Tb2item::TTBCustomItem* Ancestor, bool Relayed, Tb2item::TTBItemChangedAction Action, int Index, Tb2item::TTBCustomItem* Item);
	void __fastcall MoreItemClick(System::TObject* Sender);
	void __fastcall MoveItem(int CurIndex, int NewIndex);
	void __fastcall Paste(void);
	void __fastcall RebuildList(void);
	void __fastcall RebuildTree(void);
	void __fastcall SelectInObjectInspector(Classes::TList* AList);
	void __fastcall SetSelParentItem(Tb2item::TTBCustomItem* ASelParentItem);
	bool __fastcall TreeViewDragHandler(System::TObject* Sender, System::TObject* Source, int X, int Y, bool Drop);
	void __fastcall UnregisterAllNotifications(void);
	
protected:
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	virtual AnsiString __fastcall UniqueName(Classes::TComponent* Component);
	
public:
	__fastcall virtual TTBItemEditForm(Classes::TComponent* AOwner);
	__fastcall virtual ~TTBItemEditForm(void);
	virtual bool __fastcall EditAction(Designintf::TEditAction Action);
	virtual Designintf::TEditState __fastcall GetEditState(void);
public:
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTBItemEditForm(Classes::TComponent* AOwner, int Dummy) : Designwindows::TDesignWindow(AOwner, Dummy) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTBItemEditForm(HWND ParentWindow) : Designwindows::TDesignWindow(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TTBItemsEditor;
class PASCALIMPLEMENTATION TTBItemsEditor : public Designeditors::TDefaultEditor 
{
	typedef Designeditors::TDefaultEditor inherited;
	
public:
	virtual void __fastcall Edit(void);
	virtual void __fastcall ExecuteVerb(int Index);
	virtual AnsiString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount(void);
public:
	#pragma option push -w-inl
	/* TComponentEditor.Create */ inline __fastcall virtual TTBItemsEditor(Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TDefaultEditor(AComponent, ADesigner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TTBItemsEditor(void) { }
	#pragma option pop
	
};


class DELPHICLASS TTBItemsPropertyEditor;
class PASCALIMPLEMENTATION TTBItemsPropertyEditor : public Designeditors::TStringProperty 
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual void __fastcall Edit(void);
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual AnsiString __fastcall GetValue();
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall virtual TTBItemsPropertyEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TTBItemsPropertyEditor(void) { }
	#pragma option pop
	
};


typedef void __fastcall (__closure *TTBDsgnEditorHook)(TTBItemEditForm* Sender);

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall TBRegisterItemClass(TMetaClass* AClass, const AnsiString ACaption, unsigned ResInstance);
extern PACKAGE void __fastcall TBRegisterDsgnEditorHook(TTBDsgnEditorHook Hook);
extern PACKAGE void __fastcall TBUnregisterDsgnEditorHook(TTBDsgnEditorHook Hook);

}	/* namespace Tb2dsgnitemeditor */
using namespace Tb2dsgnitemeditor;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TB2DsgnItemEditor
