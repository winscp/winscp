// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CustomDirView.pas' rev: 6.00

#ifndef CustomDirViewHPP
#define CustomDirViewHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <NortonLikeListView.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <CustomPathComboBox.hpp>	// Pascal unit
#include <AssociatedStatusBar.hpp>	// Pascal unit
#include <PathLabel.hpp>	// Pascal unit
#include <IEListView.hpp>	// Pascal unit
#include <IEDriveInfo.hpp>	// Pascal unit
#include <DragDropFilesEx.hpp>	// Pascal unit
#include <DragDrop.hpp>	// Pascal unit
#include <BaseUtils.hpp>	// Pascal unit
#include <PIDL.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <CommCtrl.hpp>	// Pascal unit
#include <ActiveX.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <ShlObj.hpp>	// Pascal unit
#include <ComObj.hpp>	// Pascal unit
#include <ShellAPI.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Customdirview
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TDDError { DDCreateShortCutError, DDPathNotFoundError };
#pragma option pop

typedef void __fastcall (__closure *TDDOnDragEnter)(System::TObject* Sender, _di_IDataObject DataObj, int grfKeyState, const Types::TPoint &Point, int &dwEffect, bool &Accept);

typedef void __fastcall (__closure *TDDOnDragLeave)(System::TObject* Sender);

typedef void __fastcall (__closure *TDDOnDragOver)(System::TObject* Sender, int grfKeyState, const Types::TPoint &Point, int &dwEffect);

typedef void __fastcall (__closure *TDDOnDrop)(System::TObject* Sender, _di_IDataObject DataObj, int grfKeyState, const Types::TPoint &Point, int &dwEffect);

typedef void __fastcall (__closure *TDDOnQueryContinueDrag)(System::TObject* Sender, BOOL FEscapePressed, int grfKeyState, HRESULT &Result);

typedef void __fastcall (__closure *TDDOnGiveFeedback)(System::TObject* Sender, int dwEffect, HRESULT &Result);

typedef void __fastcall (__closure *TDDOnChooseEffect)(System::TObject* Sender, int grfKeyState, int &dwEffect);

typedef void __fastcall (__closure *TDDOnDragDetect)(System::TObject* Sender, int grfKeyState, const Types::TPoint &DetectStart, const Types::TPoint &Point, Dragdrop::TDragDetectStatus DragStatus);

typedef void __fastcall (__closure *TDDOnCreateDragFileList)(System::TObject* Sender, Dragdropfilesex::TFileList* FileList, bool &Created);

typedef void __fastcall (__closure *TDDOnCreateDataObject)(System::TObject* Sender, Dragdrop::TDataObject* &DataObject);

typedef void __fastcall (__closure *TDDOnTargetHasDropHandler)(System::TObject* Sender, Comctrls::TListItem* Item, int &Effect, bool &DropHandler);

typedef void __fastcall (__closure *TOnProcessDropped)(System::TObject* Sender, int grfKeyState, const Types::TPoint &Point, int &dwEffect);

typedef void __fastcall (__closure *TDDErrorEvent)(System::TObject* Sender, TDDError ErrorNo);

typedef void __fastcall (__closure *TDDExecutedEvent)(System::TObject* Sender, int dwEffect);

typedef void __fastcall (__closure *TDDFileOperationEvent)(System::TObject* Sender, int dwEffect, AnsiString SourcePath, AnsiString TargetPath, bool &DoOperation);

typedef void __fastcall (__closure *TDDFileOperationExecutedEvent)(System::TObject* Sender, int dwEffect, AnsiString SourcePath, AnsiString TargetPath);

typedef void __fastcall (__closure *TDirViewExecFileEvent)(System::TObject* Sender, Comctrls::TListItem* Item, bool &AllowExec);

typedef void __fastcall (__closure *TRenameEvent)(System::TObject* Sender, Comctrls::TListItem* Item, AnsiString NewName);

#pragma option push -b-
enum TSelAttr { selDontCare, selYes, selNo };
#pragma option pop

struct TFileFilter
{
	AnsiString Masks;
	Word IncludeAttr;
	Word ExcludeAttr;
	bool Directories;
	__int64 FileSizeFrom;
	__int64 FileSizeTo;
	System::TDateTime ModificationFrom;
	System::TDateTime ModificationTo;
} ;

#pragma option push -b-
enum THistoryDirection { hdBack, hdForward };
#pragma option pop

class DELPHICLASS TCustomDirView;
typedef void __fastcall (__closure *THistoryChangeEvent)(TCustomDirView* Sender);

typedef void __fastcall (__closure *TDVGetFilterEvent)(TCustomDirView* Sender, bool Select, TFileFilter &Filter);

#pragma option push -b-
enum TCompareCriteria { ccTime, ccSize };
#pragma option pop

typedef Set<TCompareCriteria, ccTime, ccSize>  TCompareCriterias;

class DELPHICLASS TCustomizableDragDropFilesEx;
class PASCALIMPLEMENTATION TCustomizableDragDropFilesEx : public Dragdropfilesex::TDragDropFilesEx 
{
	typedef Dragdropfilesex::TDragDropFilesEx inherited;
	
public:
	HIDESBASE Dragdrop::TDragResult __fastcall Execute(Dragdrop::TDataObject* DataObject);
public:
	#pragma option push -w-inl
	/* TDragDropFilesEx.Create */ inline __fastcall virtual TCustomizableDragDropFilesEx(Classes::TComponent* AOwner) : Dragdropfilesex::TDragDropFilesEx(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TDragDropFilesEx.Destroy */ inline __fastcall virtual ~TCustomizableDragDropFilesEx(void) { }
	#pragma option pop
	
};


class PASCALIMPLEMENTATION TCustomDirView : public Ielistview::TIEListView 
{
	typedef Ielistview::TIEListView inherited;
	
private:
	bool FAddParentDir;
	bool FDimmHiddenFiles;
	bool FShowDirectories;
	bool FDirsOnTop;
	bool FShowSubDirSize;
	bool FSortByExtension;
	bool FWantUseDragImages;
	bool FCanUseDragImages;
	TCustomizableDragDropFilesEx* FDragDropFilesEx;
	AnsiString FInvalidNameChars;
	bool FSingleClickToExec;
	bool FUseSystemContextMenu;
	TDVGetFilterEvent FOnGetSelectFilter;
	Classes::TNotifyEvent FOnStartLoading;
	Classes::TNotifyEvent FOnLoaded;
	Classes::TNotifyEvent FOnDirUpdated;
	_SYSTEMTIME FReloadTime;
	char FDragDrive;
	bool FExeDrag;
	bool FDDLinkOnExeDrag;
	TDDOnDragEnter FOnDDDragEnter;
	TDDOnDragLeave FOnDDDragLeave;
	TDDOnDragOver FOnDDDragOver;
	TDDOnDrop FOnDDDrop;
	TDDOnQueryContinueDrag FOnDDQueryContinueDrag;
	TDDOnGiveFeedback FOnDDGiveFeedback;
	TDDOnChooseEffect FOnDDChooseEffect;
	TDDOnDragDetect FOnDDDragDetect;
	TDDOnCreateDragFileList FOnDDCreateDragFileList;
	TOnProcessDropped FOnDDProcessDropped;
	TDDErrorEvent FOnDDError;
	TDDExecutedEvent FOnDDExecuted;
	TDDFileOperationEvent FOnDDFileOperation;
	TDDFileOperationExecutedEvent FOnDDFileOperationExecuted;
	Classes::TNotifyEvent FOnDDEnd;
	TDDOnCreateDataObject FOnDDCreateDataObject;
	TDDOnTargetHasDropHandler FOnDDTargetHasDropHandler;
	Dragdrop::TOnMenuPopup FOnDDMenuPopup;
	TDirViewExecFileEvent FOnExecFile;
	bool FForceRename;
	Dragdrop::TDragResult FLastDDResult;
	AnsiString FLastRenameName;
	_FILETIME FLastVScrollTime;
	int FVScrollCount;
	bool FContextMenu;
	bool FDragEnabled;
	#pragma pack(push, 1)
	Types::TPoint FDragPos;
	#pragma pack(pop)
	
	#pragma pack(push, 1)
	Types::TPoint FStartPos;
	#pragma pack(pop)
	
	bool FDDOwnerIsSource;
	bool FAbortLoading;
	Comctrls::TAnimate* FAnimation;
	int FBackCount;
	Menus::TPopupMenu* FBackMenu;
	bool FDontRecordPath;
	bool FDragOnDriveIsMove;
	bool FNotifyEnabled;
	_FILETIME FDragStartTime;
	Menus::TPopupMenu* FForwardMenu;
	Classes::TStrings* FHistoryPaths;
	Controls::TImageList* FImageList16;
	Controls::TImageList* FImageList32;
	bool FLoadAnimation;
	int FMaxHistoryCount;
	int FMaxHistoryMenuLen;
	int FMaxHistoryMenuWidth;
	bool FNeverPainted;
	Custompathcombobox::TCustomPathComboBox* FPathComboBox;
	Pathlabel::TCustomPathLabel* FPathLabel;
	Associatedstatusbar::TAssociatedStatusBar* FStatusBar;
	TRenameEvent FOnBeginRename;
	TRenameEvent FOnEndRename;
	THistoryChangeEvent FOnHistoryChange;
	bool FShowHiddenFiles;
	bool FSavedSelection;
	AnsiString FSavedSelectionFile;
	AnsiString FSavedSelectionLastFile;
	bool FPendingFocusSomething;
	HIDESBASE MESSAGE void __fastcall CNNotify(Messages::TWMNotify &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonDblClk(Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonUp(Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMContextMenu(Messages::TWMContextMenu &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMRButtonDown(Messages::TWMMouse &Message);
	void __fastcall DumbCustomDrawItem(Comctrls::TCustomListView* Sender, Comctrls::TListItem* Item, Comctrls::TCustomDrawState State, bool &DefaultDraw);
	void __fastcall DumbCustomDrawSubItem(Comctrls::TCustomListView* Sender, Comctrls::TListItem* Item, int SubItem, Comctrls::TCustomDrawState State, bool &DefaultDraw);
	Menus::TPopupMenu* __fastcall GetBackMenu(void);
	__int64 __fastcall GetFilesMarkedSize(void);
	int __fastcall GetForwardCount(void);
	Menus::TPopupMenu* __fastcall GetForwardMenu(void);
	AnsiString __fastcall GetHistoryPath(int Index);
	bool __fastcall GetTargetPopupMenu(void);
	bool __fastcall GetUseDragImages(void);
	void __fastcall SetMaxHistoryCount(int Value);
	void __fastcall SetMaxHistoryMenuLen(int Value);
	void __fastcall SetMaxHistoryMenuWidth(int Value);
	void __fastcall SetPathComboBox(Custompathcombobox::TCustomPathComboBox* Value);
	void __fastcall SetPathLabel(Pathlabel::TCustomPathLabel* Value);
	void __fastcall SetStatusBar(Associatedstatusbar::TAssociatedStatusBar* Value);
	void __fastcall SetTargetPopupMenu(bool Value);
	HIDESBASE MESSAGE void __fastcall WMPaint(Messages::TWMPaint &Message);
	MESSAGE void __fastcall WMUserRename(Messages::TMessage &Message);
	
protected:
	bool FCaseSensitive;
	bool FDirty;
	__int64 FFilesSize;
	__int64 FFilesSelSize;
	bool FHasParentDir;
	bool FIsRecycleBin;
	AnsiString FLastPath;
	bool FLoadEnabled;
	bool FLoading;
	AnsiString FSelectFile;
	bool FWatchForChanges;
	virtual void __fastcall AddToDragFileList(Dragdropfilesex::TFileList* FileList, Comctrls::TListItem* Item);
	DYNAMIC bool __fastcall CanEdit(Comctrls::TListItem* Item);
	virtual bool __fastcall CanChangeSelection(Comctrls::TListItem* Item, bool Select);
	virtual void __fastcall ClearItems(void);
	virtual bool __fastcall GetDirOK(void) = 0 ;
	virtual void __fastcall DDDragDetect(int grfKeyState, const Types::TPoint &DetectStart, const Types::TPoint &Point, Dragdrop::TDragDetectStatus DragStatus);
	void __fastcall DDDragEnter(_di_IDataObject DataObj, int grfKeyState, const Types::TPoint &Point, int &dwEffect, bool &Accept);
	void __fastcall DDDragLeave(void);
	void __fastcall DDDragOver(int grfKeyState, const Types::TPoint &Point, int &dwEffect);
	virtual void __fastcall DDChooseEffect(int grfKeyState, int &dwEffect);
	void __fastcall DDDrop(_di_IDataObject DataObj, int grfKeyState, const Types::TPoint &Point, int &dwEffect);
	virtual void __fastcall DDDropHandlerSucceeded(System::TObject* Sender, int grfKeyState, const Types::TPoint &Point, int dwEffect);
	virtual void __fastcall DDGiveFeedback(int dwEffect, HRESULT &Result);
	void __fastcall DDMenuPopup(System::TObject* Sender, HMENU AMenu, _di_IDataObject DataObj, int AMinCustCmd, int grfKeyState, const Types::TPoint &pt);
	virtual void __fastcall DDMenuDone(System::TObject* Sender, HMENU AMenu);
	void __fastcall DDProcessDropped(System::TObject* Sender, int grfKeyState, const Types::TPoint &Point, int dwEffect);
	virtual void __fastcall DDQueryContinueDrag(BOOL FEscapePressed, int grfKeyState, HRESULT &Result);
	virtual void __fastcall DDSpecifyDropTarget(System::TObject* Sender, bool DragDropHandler, const Types::TPoint &Point, Shlobj::PItemIDList &pidlFQ, AnsiString &Filename);
	virtual void __fastcall GetDisplayInfo(Comctrls::TListItem* ListItem, tagLVITEMA &DispInfo);
	virtual Dragdrop::TDropEffectSet __fastcall GetDragSourceEffects(void);
	virtual AnsiString __fastcall GetPathName(void) = 0 ;
	virtual int __fastcall GetFilesCount(void);
	DYNAMIC void __fastcall ColClick(Comctrls::TListColumn* Column);
	virtual void __fastcall CreateWnd(void);
	Classes::TStrings* __fastcall CustomCreateFileList(bool Focused, bool OnlyFocused, bool FullPath, Classes::TStrings* FileList = (Classes::TStrings*)(0x0), bool ItemObject = false);
	virtual bool __fastcall CustomDrawItem(Comctrls::TListItem* Item, Comctrls::TCustomDrawState State, Comctrls::TCustomDrawStage Stage);
	virtual bool __fastcall CustomDrawSubItem(Comctrls::TListItem* Item, int SubItem, Comctrls::TCustomDrawState State, Comctrls::TCustomDrawStage Stage);
	void __fastcall CustomSortItems(void * SortProc);
	DYNAMIC void __fastcall Delete(Comctrls::TListItem* Item);
	virtual void __fastcall DisplayContextMenu(const Types::TPoint &Where) = 0 ;
	void __fastcall DoAnimation(bool Start);
	DYNAMIC void __fastcall DoHistoryChange(void);
	virtual bool __fastcall DragCompleteFileList(void);
	DYNAMIC void __fastcall Edit(const tagLVITEMA &HItem);
	virtual void __fastcall EndSelectionUpdate(void);
	virtual void __fastcall Execute(Comctrls::TListItem* Item);
	virtual void __fastcall ExecuteFile(Comctrls::TListItem* Item) = 0 ;
	virtual void __fastcall FocusSomething(void);
	virtual bool __fastcall GetIsRoot(void) = 0 ;
	virtual void __fastcall IconsSetImageList(void);
	virtual bool __fastcall ItemCanDrag(Comctrls::TListItem* Item);
	virtual Graphics::TColor __fastcall ItemColor(Comctrls::TListItem* Item);
	virtual __int64 __fastcall ItemFileSize(Comctrls::TListItem* Item) = 0 ;
	virtual int __fastcall ItemImageIndex(Comctrls::TListItem* Item, bool Cache) = 0 ;
	virtual System::TDateTime __fastcall ItemFileTime(Comctrls::TListItem* Item, Baseutils::TDateTimePrecision &Precision) = 0 ;
	virtual bool __fastcall ItemIsRecycleBin(Comctrls::TListItem* Item);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(char &Key);
	DYNAMIC void __fastcall KeyUp(Word &Key, Classes::TShiftState Shift);
	virtual void __fastcall LoadFiles(void) = 0 ;
	virtual void __fastcall PerformItemDragDropOperation(Comctrls::TListItem* Item, int Effect) = 0 ;
	void __fastcall ProcessChangedFiles(TCustomDirView* DirView, Classes::TStrings* FileList, bool FullPath, bool ExistingOnly, TCompareCriterias Criterias);
	void __fastcall ReloadForce(bool CacheIcons);
	void __fastcall RetryRename(AnsiString NewName);
	void __fastcall SelectFiles(const TFileFilter &Filter, bool Select);
	virtual void __fastcall SetAddParentDir(bool Value);
	virtual void __fastcall SetDimmHiddenFiles(bool Value);
	virtual void __fastcall SetShowDirectories(bool Value);
	void __fastcall SetDirsOnTop(bool Value);
	virtual void __fastcall SetItemImageIndex(Comctrls::TListItem* Item, int Index) = 0 ;
	virtual void __fastcall SetLoadEnabled(bool Enabled);
	virtual void __fastcall SetMultiSelect(bool Value);
	virtual AnsiString __fastcall GetPath(void) = 0 ;
	virtual bool __fastcall GetValid(void);
	void __fastcall HistoryItemClick(System::TObject* Sender);
	virtual void __fastcall InternalEdit(const tagLVITEMA &HItem) = 0 ;
	virtual bool __fastcall ItemIsFile(Comctrls::TListItem* Item) = 0 ;
	virtual bool __fastcall ItemMatchesFilter(Comctrls::TListItem* Item, const TFileFilter &Filter) = 0 ;
	virtual Word __fastcall ItemOverlayIndexes(Comctrls::TListItem* Item);
	void __fastcall LimitHistorySize(void);
	virtual AnsiString __fastcall MinimizePath(AnsiString Path, int Len) = 0 ;
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	void __fastcall PathChanged(void);
	virtual void __fastcall SetPath(AnsiString Value) = 0 ;
	void __fastcall SetSortByExtension(bool Value);
	virtual void __fastcall SetShowHiddenFiles(bool Value);
	virtual void __fastcall SetShowSubDirSize(bool Value);
	virtual void __fastcall SetViewStyle(Comctrls::TViewStyle Value);
	virtual void __fastcall SetWatchForChanges(bool Value);
	virtual bool __fastcall TargetHasDropHandler(Comctrls::TListItem* Item, int Effect);
	void __fastcall UpdateHistoryMenu(THistoryDirection Direction);
	DYNAMIC void __fastcall UpdatePathComboBox(void);
	DYNAMIC void __fastcall UpdatePathLabel(void);
	DYNAMIC void __fastcall UpdateStatusBar(void);
	virtual void __fastcall WndProc(Messages::TMessage &Message);
	__property Controls::TImageList* ImageList16 = {read=FImageList16};
	__property Controls::TImageList* ImageList32 = {read=FImageList32};
	
public:
	bool __fastcall AnyFileSelected(bool OnlyFocused);
	__fastcall virtual TCustomDirView(Classes::TComponent* AOwner);
	virtual void __fastcall CreateDirectory(AnsiString DirName) = 0 ;
	__fastcall virtual ~TCustomDirView(void);
	virtual void __fastcall Load(void);
	virtual void __fastcall Reload(bool CacheIcons);
	Classes::TStrings* __fastcall CreateFocusedFileList(bool FullPath, Classes::TStrings* FileList = (Classes::TStrings*)(0x0));
	Classes::TStrings* __fastcall CreateFileList(bool Focused, bool FullPath, Classes::TStrings* FileList = (Classes::TStrings*)(0x0));
	virtual bool __fastcall DoSelectByMask(bool Select);
	virtual void __fastcall ExecuteHomeDirectory(void) = 0 ;
	virtual void __fastcall ExecuteParentDirectory(void) = 0 ;
	virtual void __fastcall ExecuteRootDirectory(void) = 0 ;
	void __fastcall ExecuteCurrentFile(void);
	Comctrls::TListItem* __fastcall FindFileItem(AnsiString FileName);
	void __fastcall HistoryGo(int Index);
	virtual bool __fastcall ItemIsDirectory(Comctrls::TListItem* Item) = 0 ;
	virtual bool __fastcall ItemIsParentDirectory(Comctrls::TListItem* Item) = 0 ;
	virtual AnsiString __fastcall ItemFullFileName(Comctrls::TListItem* Item) = 0 ;
	virtual AnsiString __fastcall ItemFileName(Comctrls::TListItem* Item) = 0 ;
	virtual void __fastcall ReloadDirectory(void) = 0 ;
	virtual void __fastcall DisplayPropertiesMenu(void) = 0 ;
	Classes::TStrings* __fastcall CreateChangedFileList(TCustomDirView* DirView, bool FullPath, bool ExistingOnly, TCompareCriterias Criterias);
	virtual void __fastcall CompareFiles(TCustomDirView* DirView, bool ExistingOnly, TCompareCriterias Criterias);
	void __fastcall SaveSelection(void);
	void __fastcall RestoreSelection(void);
	void __fastcall DiscardSavedSelection(void);
	DYNAMIC bool __fastcall CanPasteFromClipBoard(void);
	virtual bool __fastcall PasteFromClipBoard(AnsiString TargetPath = "") = 0 ;
	__property bool AddParentDir = {read=FAddParentDir, write=SetAddParentDir, default=0};
	__property bool DimmHiddenFiles = {read=FDimmHiddenFiles, write=SetDimmHiddenFiles, default=1};
	__property bool ShowDirectories = {read=FShowDirectories, write=SetShowDirectories, default=1};
	__property bool DirsOnTop = {read=FDirsOnTop, write=SetDirsOnTop, default=1};
	__property TCustomizableDragDropFilesEx* DragDropFilesEx = {read=FDragDropFilesEx};
	__property bool ShowSubDirSize = {read=FShowSubDirSize, write=SetShowSubDirSize, default=0};
	__property bool SortByExtension = {read=FSortByExtension, write=SetSortByExtension, default=0};
	__property bool WantUseDragImages = {read=FWantUseDragImages, write=FWantUseDragImages, default=1};
	__property bool UseDragImages = {read=GetUseDragImages, stored=false, nodefault};
	__property FullDrag  = {default=1};
	__property bool TargetPopupMenu = {read=GetTargetPopupMenu, write=SetTargetPopupMenu, default=1};
	__property bool DDOwnerIsSource = {read=FDDOwnerIsSource, nodefault};
	__property __int64 FilesSize = {read=FFilesSize};
	__property __int64 FilesSelSize = {read=FFilesSelSize};
	__property int FilesCount = {read=GetFilesCount, nodefault};
	__property __int64 FilesMarkedSize = {read=GetFilesMarkedSize};
	__property bool HasParentDir = {read=FHasParentDir, nodefault};
	__property AnsiString Path = {read=GetPath, write=SetPath};
	__property AnsiString PathName = {read=GetPathName};
	__property _SYSTEMTIME ReloadTime = {read=FReloadTime};
	__property bool SingleClickToExec = {read=FSingleClickToExec, write=FSingleClickToExec, default=0};
	__property bool UseSystemContextMenu = {read=FUseSystemContextMenu, write=FUseSystemContextMenu, default=1};
	__property bool Loading = {read=FLoading, nodefault};
	__property bool AbortLoading = {read=FAbortLoading, write=FAbortLoading, stored=false, nodefault};
	__property int BackCount = {read=FBackCount, nodefault};
	__property Menus::TPopupMenu* BackMenu = {read=GetBackMenu};
	__property bool LoadAnimation = {read=FLoadAnimation, write=FLoadAnimation, default=1};
	__property bool LoadEnabled = {read=FLoadEnabled, write=SetLoadEnabled, default=1};
	__property bool Dirty = {read=FDirty, nodefault};
	__property bool DirOK = {read=GetDirOK, nodefault};
	__property AnsiString LastPath = {read=FLastPath};
	__property bool IsRecycleBin = {read=FIsRecycleBin, nodefault};
	__property bool DDLinkOnExeDrag = {read=FDDLinkOnExeDrag, write=FDDLinkOnExeDrag, default=0};
	__property char DragDrive = {read=FDragDrive, nodefault};
	__property bool DragOnDriveIsMove = {read=FDragOnDriveIsMove, write=FDragOnDriveIsMove, nodefault};
	__property Dragdrop::TDropEffectSet DragSourceEffects = {read=GetDragSourceEffects, nodefault};
	__property bool ExeDrag = {read=FExeDrag, nodefault};
	__property int ForwardCount = {read=GetForwardCount, nodefault};
	__property Menus::TPopupMenu* ForwardMenu = {read=GetForwardMenu};
	__property AnsiString HistoryPath[int Index] = {read=GetHistoryPath};
	__property bool IsRoot = {read=GetIsRoot, nodefault};
	__property Dragdrop::TDragResult LastDDResult = {read=FLastDDResult, nodefault};
	__property SmallImages ;
	__property LargeImages ;
	__property int MaxHistoryCount = {read=FMaxHistoryCount, write=SetMaxHistoryCount, default=200};
	__property int MaxHistoryMenuLen = {read=FMaxHistoryMenuLen, write=SetMaxHistoryMenuLen, default=9};
	__property int MaxHistoryMenuWidth = {read=FMaxHistoryMenuWidth, write=SetMaxHistoryMenuWidth, default=300};
	__property OnContextPopup ;
	__property TRenameEvent OnBeginRename = {read=FOnBeginRename, write=FOnBeginRename};
	__property TRenameEvent OnEndRename = {read=FOnEndRename, write=FOnEndRename};
	__property TDVGetFilterEvent OnGetSelectFilter = {read=FOnGetSelectFilter, write=FOnGetSelectFilter};
	__property Classes::TNotifyEvent OnStartLoading = {read=FOnStartLoading, write=FOnStartLoading};
	__property Classes::TNotifyEvent OnLoaded = {read=FOnLoaded, write=FOnLoaded};
	__property Classes::TNotifyEvent OnDirUpdated = {read=FOnDirUpdated, write=FOnDirUpdated};
	__property TDDOnDragEnter OnDDDragEnter = {read=FOnDDDragEnter, write=FOnDDDragEnter};
	__property TDDOnDragLeave OnDDDragLeave = {read=FOnDDDragLeave, write=FOnDDDragLeave};
	__property TDDOnDragOver OnDDDragOver = {read=FOnDDDragOver, write=FOnDDDragOver};
	__property TDDOnDrop OnDDDrop = {read=FOnDDDrop, write=FOnDDDrop};
	__property TDDOnQueryContinueDrag OnDDQueryContinueDrag = {read=FOnDDQueryContinueDrag, write=FOnDDQueryContinueDrag};
	__property TDDOnGiveFeedback OnDDGiveFeedback = {read=FOnDDGiveFeedback, write=FOnDDGiveFeedback};
	__property TDDOnChooseEffect OnDDChooseEffect = {read=FOnDDChooseEffect, write=FOnDDChooseEffect};
	__property TDDOnDragDetect OnDDDragDetect = {read=FOnDDDragDetect, write=FOnDDDragDetect};
	__property TDDOnCreateDragFileList OnDDCreateDragFileList = {read=FOnDDCreateDragFileList, write=FOnDDCreateDragFileList};
	__property Classes::TNotifyEvent OnDDEnd = {read=FOnDDEnd, write=FOnDDEnd};
	__property TDDOnCreateDataObject OnDDCreateDataObject = {read=FOnDDCreateDataObject, write=FOnDDCreateDataObject};
	__property TDDOnTargetHasDropHandler OnDDTargetHasDropHandler = {read=FOnDDTargetHasDropHandler, write=FOnDDTargetHasDropHandler};
	__property TOnProcessDropped OnDDProcessDropped = {read=FOnDDProcessDropped, write=FOnDDProcessDropped};
	__property TDDErrorEvent OnDDError = {read=FOnDDError, write=FOnDDError};
	__property TDDExecutedEvent OnDDExecuted = {read=FOnDDExecuted, write=FOnDDExecuted};
	__property TDDFileOperationEvent OnDDFileOperation = {read=FOnDDFileOperation, write=FOnDDFileOperation};
	__property TDDFileOperationExecutedEvent OnDDFileOperationExecuted = {read=FOnDDFileOperationExecuted, write=FOnDDFileOperationExecuted};
	__property Dragdrop::TOnMenuPopup OnDDMenuPopup = {read=FOnDDMenuPopup, write=FOnDDMenuPopup};
	__property TDirViewExecFileEvent OnExecFile = {read=FOnExecFile, write=FOnExecFile};
	__property THistoryChangeEvent OnHistoryChange = {read=FOnHistoryChange, write=FOnHistoryChange};
	__property Custompathcombobox::TCustomPathComboBox* PathComboBox = {read=FPathComboBox, write=SetPathComboBox};
	__property Pathlabel::TCustomPathLabel* PathLabel = {read=FPathLabel, write=SetPathLabel};
	__property bool ShowHiddenFiles = {read=FShowHiddenFiles, write=SetShowHiddenFiles, default=1};
	__property Associatedstatusbar::TAssociatedStatusBar* StatusBar = {read=FStatusBar, write=SetStatusBar};
	__property bool WatchForChanges = {read=FWatchForChanges, write=SetWatchForChanges, default=0};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCustomDirView(HWND ParentWindow) : Ielistview::TIEListView(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
static const Shortint clDefaultItemColor = 0xffffffe1;
static const Word WM_USER_RENAME = 0x439;
static const Shortint oiNoOverlay = 0x0;
static const Shortint oiDirUp = 0x1;
static const Shortint oiLink = 0x2;
static const Shortint oiBrokenLink = 0x4;
static const Shortint oiShared = 0x8;
static const Word DefaultHistoryMenuWidth = 0x12c;
static const Shortint DefaultHistoryMenuLen = 0x9;
static const Byte DefaultHistoryCount = 0xc8;
static const Shortint DDMaxSlowCount = 0x3;
static const int DDVScrollDelay = 0x1e8480;
static const int DDHScrollDelay = 0x1e8480;
static const int DDDragStartDelay = 0x7a120;
static const Shortint DirAttrMask = 0x16;
extern PACKAGE System::ResourceString _SErrorOpenFile;
#define Customdirview_SErrorOpenFile System::LoadResourceString(&Customdirview::_SErrorOpenFile)
extern PACKAGE System::ResourceString _SErrorRenameFile;
#define Customdirview_SErrorRenameFile System::LoadResourceString(&Customdirview::_SErrorRenameFile)
extern PACKAGE System::ResourceString _SErrorRenameFileExists;
#define Customdirview_SErrorRenameFileExists System::LoadResourceString(&Customdirview::_SErrorRenameFileExists)
extern PACKAGE System::ResourceString _SErrorInvalidName;
#define Customdirview_SErrorInvalidName System::LoadResourceString(&Customdirview::_SErrorInvalidName)
extern PACKAGE System::ResourceString _STextFileExt;
#define Customdirview_STextFileExt System::LoadResourceString(&Customdirview::_STextFileExt)
extern PACKAGE System::ResourceString _STextFiles;
#define Customdirview_STextFiles System::LoadResourceString(&Customdirview::_STextFiles)
extern PACKAGE System::ResourceString _STextDirectories;
#define Customdirview_STextDirectories System::LoadResourceString(&Customdirview::_STextDirectories)
extern PACKAGE System::ResourceString _SParentDir;
#define Customdirview_SParentDir System::LoadResourceString(&Customdirview::_SParentDir)
extern PACKAGE System::ResourceString _SIconUpdateThreadTerminationError;
#define Customdirview_SIconUpdateThreadTerminationError System::LoadResourceString(&Customdirview::_SIconUpdateThreadTerminationError)
extern PACKAGE System::ResourceString _SDragDropError;
#define Customdirview_SDragDropError System::LoadResourceString(&Customdirview::_SDragDropError)
extern PACKAGE System::ResourceString _SDriveNotReady;
#define Customdirview_SDriveNotReady System::LoadResourceString(&Customdirview::_SDriveNotReady)
extern PACKAGE System::ResourceString _SDirNotExists;
#define Customdirview_SDirNotExists System::LoadResourceString(&Customdirview::_SDirNotExists)
extern PACKAGE int StdDirIcon;
extern PACKAGE int StdDirSelIcon;
extern PACKAGE System::TObject* DropSourceControl;
extern PACKAGE int UnknownFileIcon;
extern PACKAGE bool HasExtendedCOMCTL32;
extern PACKAGE AnsiString StdDirTypeName;
extern PACKAGE int DefaultExeIcon;
extern PACKAGE AnsiString UserDocumentDirectory;
extern PACKAGE bool __fastcall IsExecutable(AnsiString FileName);
extern PACKAGE AnsiString __fastcall GetNextMask(AnsiString &Mask);
extern PACKAGE bool __fastcall FileNameMatchesMasks(AnsiString FileName, AnsiString Masks);
extern PACKAGE void __fastcall DefaultFileFilter(TFileFilter &Filter);
extern PACKAGE AnsiString __fastcall ResolveFileShortCut(AnsiString SourceFile, bool ShowDialog = false);
extern PACKAGE bool __fastcall CreateFileShortCut(AnsiString SourceFile, AnsiString Target, AnsiString DisplayName, bool UpdateIfExists = false);
extern PACKAGE int __fastcall GetIconIndex(const AnsiString AFile, unsigned Attrs, unsigned Flags);
extern PACKAGE _SHFILEINFOA __fastcall GetshFileInfo(const AnsiString AFile, unsigned Attrs, unsigned Flags);
extern PACKAGE bool __fastcall GetShellDisplayName(const _di_IShellFolder ShellFolder, Shlobj::PItemIDList IDList, unsigned Flags, AnsiString &Name);
extern PACKAGE Controls::TImageList* __fastcall OverlayImageList(int Size);

}	/* namespace Customdirview */
using namespace Customdirview;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CustomDirView
