// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CustomDriveView.pas' rev: 6.00

#ifndef CustomDriveViewHPP
#define CustomDriveViewHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DragDropFilesEx.hpp>	// Pascal unit
#include <IEDriveInfo.hpp>	// Pascal unit
#include <CustomDirView.hpp>	// Pascal unit
#include <DragDrop.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <ShlObj.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <CommCtrl.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Customdriveview
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TRecursiveScan { rsNoRecursive, rsRecursive, rsRecursiveExisting, rsRecursiveExpanded };
#pragma option pop

#pragma option push -b-
enum TScanStartNode { coNoScanStartNode, coScanStartNode };
#pragma option pop

typedef bool __fastcall (__closure *TCallBackFunc)(Comctrls::TTreeNode* &Node, void * Data);

class DELPHICLASS TCustomDriveView;
class PASCALIMPLEMENTATION TCustomDriveView : public Comctrls::TCustomTreeView 
{
	typedef Comctrls::TCustomTreeView inherited;
	
protected:
	Forms::TCustomForm* FParentForm;
	Classes::TStringList* FDragFileList;
	bool FUseDragImages;
	Customdirview::TCustomizableDragDropFilesEx* FDragDropFilesEx;
	Controls::TDragImageList* FDragImageList;
	char FDragDrive;
	bool FExeDrag;
	bool FDDLinkOnExeDrag;
	_FILETIME FDragOverTime;
	_FILETIME FLastVScrollTime;
	_FILETIME FLastHScrollTime;
	int FVScrollCount;
	Comctrls::TTreeNode* FDragNode;
	_FILETIME FDragStartTime;
	#pragma pack(push, 1)
	Types::TPoint FDragPos;
	#pragma pack(pop)
	
	#pragma pack(push, 1)
	Types::TPoint FStartPos;
	#pragma pack(pop)
	
	bool FContextMenu;
	bool FCanChange;
	bool FUseSystemContextMenu;
	bool FDimmHiddenDirs;
	bool FShowHiddenDirs;
	bool FContinue;
	Controls::TImageList* FImageList;
	Customdirview::TDDOnDragEnter FOnDDDragEnter;
	Customdirview::TDDOnDragLeave FOnDDDragLeave;
	Customdirview::TDDOnDragOver FOnDDDragOver;
	Customdirview::TDDOnDrop FOnDDDrop;
	Customdirview::TDDOnQueryContinueDrag FOnDDQueryContinueDrag;
	Customdirview::TDDOnChooseEffect FOnDDChooseEffect;
	Customdirview::TDDOnGiveFeedback FOnDDGiveFeedback;
	Customdirview::TDDOnDragDetect FOnDDDragDetect;
	Dragdrop::TOnMenuPopup FOnDDMenuPopup;
	Customdirview::TOnProcessDropped FOnDDProcessDropped;
	Customdirview::TDDErrorEvent FOnDDError;
	Customdirview::TDDExecutedEvent FOnDDExecuted;
	Customdirview::TDDFileOperationEvent FOnDDFileOperation;
	Customdirview::TDDFileOperationExecutedEvent FOnDDFileOperationExecuted;
	Customdirview::TDDOnCreateDragFileList FOnDDCreateDragFileList;
	Classes::TNotifyEvent FOnDDEnd;
	Customdirview::TDDOnCreateDataObject FOnDDCreateDataObject;
	Dragdrop::TDragResult FLastDDResult;
	bool __fastcall GetTargetPopupMenu(void);
	void __fastcall SetTargetPopUpMenu(bool Value);
	void __fastcall SetDimmHiddenDirs(bool Value);
	void __fastcall SetShowHiddenDirs(bool Value);
	virtual AnsiString __fastcall GetDirectory();
	virtual void __fastcall SetDirectory(AnsiString Value);
	virtual Customdirview::TCustomDirView* __fastcall GetCustomDirView(void) = 0 ;
	virtual void __fastcall SetCustomDirView(Customdirview::TCustomDirView* Value) = 0 ;
	virtual void __fastcall CreateWnd(void);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	Comctrls::TTreeNode* __fastcall GetNodeFromHItem(const tagTVITEMA &Item);
	virtual bool __fastcall IsCustomDrawn(Comctrls::TCustomDrawTarget Target, Comctrls::TCustomDrawStage Stage);
	virtual bool __fastcall CustomDrawItem(Comctrls::TTreeNode* Node, Comctrls::TCustomDrawState State, Comctrls::TCustomDrawStage Stage, bool &PaintImages);
	HIDESBASE MESSAGE void __fastcall CNNotify(Messages::TWMNotify &Msg);
	HIDESBASE MESSAGE void __fastcall CMColorChanged(Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonUp(Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMRButtonDown(Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMContextMenu(Messages::TWMContextMenu &Msg);
	DYNAMIC void __fastcall Delete(Comctrls::TTreeNode* Node);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(char &Key);
	DYNAMIC void __fastcall KeyUp(Word &Key, Classes::TShiftState Shift);
	void __fastcall InternalOnDrawItem(Comctrls::TCustomTreeView* Sender, Comctrls::TTreeNode* Node, Comctrls::TCustomDrawState State, bool &DefaultDraw);
	void __fastcall DDDragEnter(_di_IDataObject DataObj, int KeyState, const Types::TPoint &Point, int &Effect, bool &Accept);
	void __fastcall DDDragLeave(void);
	void __fastcall DDDragOver(int KeyState, const Types::TPoint &Point, int &Effect);
	void __fastcall DDDrop(_di_IDataObject DataObj, int KeyState, const Types::TPoint &Point, int &Effect);
	void __fastcall DDQueryContinueDrag(BOOL EscapePressed, int KeyState, HRESULT &Result);
	void __fastcall DDDropHandlerSucceeded(System::TObject* Sender, int KeyState, const Types::TPoint &Point, int Effect);
	void __fastcall DDGiveFeedback(int Effect, HRESULT &Result);
	void __fastcall DDMenuPopup(System::TObject* Sender, HMENU AMenu, _di_IDataObject DataObj, int AMinCustCmd, int grfKeyState, const Types::TPoint &Point);
	void __fastcall DDMenuDone(System::TObject* Sender, HMENU AMenu);
	void __fastcall DDProcessDropped(System::TObject* Sender, int KeyState, const Types::TPoint &Point, int Effect);
	virtual void __fastcall DDError(Customdirview::TDDError Error);
	void __fastcall DDSpecifyDropTarget(System::TObject* Sender, bool DragDropHandler, const Types::TPoint &Point, Shlobj::PItemIDList &PIDL, AnsiString &Filename);
	virtual void __fastcall DDDragDetect(int KeyState, const Types::TPoint &DetectStart, const Types::TPoint &Point, Dragdrop::TDragDetectStatus DragStatus);
	virtual void __fastcall PerformDragDropFileOperation(Comctrls::TTreeNode* Node, int Effect) = 0 ;
	virtual void __fastcall DDChooseEffect(int KeyState, int &Effect);
	virtual bool __fastcall DragCompleteFileList(void) = 0 ;
	virtual Dragdrop::TDragResult __fastcall DDExecute(void);
	virtual Dragdrop::TDropEffectSet __fastcall DDSourceEffects(void) = 0 ;
	virtual AnsiString __fastcall NodePath(Comctrls::TTreeNode* Node) = 0 ;
	virtual bool __fastcall NodeIsRecycleBin(Comctrls::TTreeNode* Node);
	virtual bool __fastcall NodePathExists(Comctrls::TTreeNode* Node);
	virtual Graphics::TColor __fastcall NodeColor(Comctrls::TTreeNode* Node) = 0 ;
	virtual bool __fastcall NodeCanDrag(Comctrls::TTreeNode* Node);
	virtual Word __fastcall NodeOverlayIndexes(Comctrls::TTreeNode* Node);
	virtual Comctrls::TTreeNode* __fastcall FindPathNode(AnsiString Path) = 0 ;
	virtual void __fastcall ClearDragFileList(Dragdropfilesex::TFileList* FileList);
	virtual void __fastcall AddToDragFileList(Dragdropfilesex::TFileList* FileList, Comctrls::TTreeNode* Node);
	virtual void __fastcall ValidateDirectoryEx(Comctrls::TTreeNode* Node, TRecursiveScan Recurse, bool NewDirs) = 0 ;
	void __fastcall ValidateVisibleDirectories(Comctrls::TTreeNode* Node);
	void __fastcall ValidateAllDirectories(Comctrls::TTreeNode* Node);
	virtual void __fastcall RebuildTree(void) = 0 ;
	virtual void __fastcall DisplayContextMenu(Comctrls::TTreeNode* Node, const Types::TPoint &ScreenPos) = 0 ;
	virtual void __fastcall DisplayPropertiesMenu(Comctrls::TTreeNode* Node) = 0 ;
	__property Controls::TImageList* ImageList = {read=FImageList};
	
public:
	__fastcall virtual TCustomDriveView(Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomDriveView(void);
	void __fastcall ValidateDirectory(Comctrls::TTreeNode* Node);
	virtual void __fastcall CenterNode(Comctrls::TTreeNode* Node);
	bool __fastcall SortChildren(Comctrls::TTreeNode* ParentNode, bool Recurse);
	bool __fastcall IterateSubTree(Comctrls::TTreeNode* &StartNode, TCallBackFunc CallBackFunc, TRecursiveScan Recurse, TScanStartNode ScanStartNode, void * Data);
	virtual AnsiString __fastcall NodePathName(Comctrls::TTreeNode* Node) = 0 ;
	__property Customdirview::TCustomizableDragDropFilesEx* DragDropFilesEx = {read=FDragDropFilesEx};
	__property bool UseSystemContextMenu = {read=FUseSystemContextMenu, write=FUseSystemContextMenu, default=1};
	__property bool DimmHiddenDirs = {read=FDimmHiddenDirs, write=SetDimmHiddenDirs, default=0};
	__property bool ShowHiddenDirs = {read=FShowHiddenDirs, write=SetShowHiddenDirs, default=0};
	__property bool DDLinkOnExeDrag = {read=FDDLinkOnExeDrag, write=FDDLinkOnExeDrag, default=1};
	__property Customdirview::TDDOnDragEnter OnDDDragEnter = {read=FOnDDDragEnter, write=FOnDDDragEnter};
	__property Customdirview::TDDOnDragLeave OnDDDragLeave = {read=FOnDDDragLeave, write=FOnDDDragLeave};
	__property Customdirview::TDDOnDragOver OnDDDragOver = {read=FOnDDDragOver, write=FOnDDDragOver};
	__property Customdirview::TDDOnDrop OnDDDrop = {read=FOnDDDrop, write=FOnDDDrop};
	__property Customdirview::TDDOnQueryContinueDrag OnDDQueryContinueDrag = {read=FOnDDQueryContinueDrag, write=FOnDDQueryContinueDrag};
	__property Customdirview::TDDOnChooseEffect OnDDChooseEffect = {read=FOnDDChooseEffect, write=FOnDDChooseEffect};
	__property Customdirview::TDDOnGiveFeedback OnDDGiveFeedback = {read=FOnDDGiveFeedback, write=FOnDDGiveFeedback};
	__property Customdirview::TDDOnDragDetect OnDDDragDetect = {read=FOnDDDragDetect, write=FOnDDDragDetect};
	__property Customdirview::TOnProcessDropped OnDDProcessDropped = {read=FOnDDProcessDropped, write=FOnDDProcessDropped};
	__property Customdirview::TDDErrorEvent OnDDError = {read=FOnDDError, write=FOnDDError};
	__property Customdirview::TDDExecutedEvent OnDDExecuted = {read=FOnDDExecuted, write=FOnDDExecuted};
	__property Customdirview::TDDFileOperationEvent OnDDFileOperation = {read=FOnDDFileOperation, write=FOnDDFileOperation};
	__property Customdirview::TDDFileOperationExecutedEvent OnDDFileOperationExecuted = {read=FOnDDFileOperationExecuted, write=FOnDDFileOperationExecuted};
	__property Customdirview::TDDOnCreateDragFileList OnDDCreateDragFileList = {read=FOnDDCreateDragFileList, write=FOnDDCreateDragFileList};
	__property Classes::TNotifyEvent OnDDEnd = {read=FOnDDEnd, write=FOnDDEnd};
	__property Customdirview::TDDOnCreateDataObject OnDDCreateDataObject = {read=FOnDDCreateDataObject, write=FOnDDCreateDataObject};
	__property Dragdrop::TOnMenuPopup OnDDMenuPopup = {read=FOnDDMenuPopup, write=FOnDDMenuPopup};
	__property bool UseDragImages = {read=FUseDragImages, write=FUseDragImages, default=1};
	__property bool TargetPopUpMenu = {read=GetTargetPopupMenu, write=SetTargetPopUpMenu, default=1};
	__property AnsiString Directory = {read=GetDirectory, write=SetDirectory};
	__property Comctrls::TTreeNode* DragNode = {read=FDragNode};
	__property bool Continue = {read=FContinue, write=FContinue, nodefault};
	__property Dragdrop::TDragResult LastDDResult = {read=FLastDDResult, nodefault};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCustomDriveView(HWND ParentWindow) : Comctrls::TCustomTreeView(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::ResourceString _SDragDropError;
#define Customdriveview_SDragDropError System::LoadResourceString(&Customdriveview::_SDragDropError)

}	/* namespace Customdriveview */
using namespace Customdriveview;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CustomDriveView
