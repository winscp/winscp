// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DirView.pas' rev: 6.00

#ifndef DirViewHPP
#define DirViewHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <AssociatedStatusBar.hpp>	// Pascal unit
#include <PathLabel.hpp>	// Pascal unit
#include <CustomPathComboBox.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <NortonLikeListView.hpp>	// Pascal unit
#include <IEListView.hpp>	// Pascal unit
#include <BaseUtils.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <FileCtrl.hpp>	// Pascal unit
#include <DragDropFilesEx.hpp>	// Pascal unit
#include <CommCtrl.hpp>	// Pascal unit
#include <ListViewColProperties.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <DragDrop.hpp>	// Pascal unit
#include <DirViewColProperties.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <DiscMon.hpp>	// Pascal unit
#include <FileOperator.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <ListExt.hpp>	// Pascal unit
#include <CustomDirView.hpp>	// Pascal unit
#include <CompThread.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <ShlObj.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dirview
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TVolumeDisplayStyle { doPrettyName, doDisplayName, doLongPrettyName };
#pragma option pop

class DELPHICLASS EIUThread;
class PASCALIMPLEMENTATION EIUThread : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EIUThread(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EIUThread(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EIUThread(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EIUThread(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EIUThread(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EIUThread(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EIUThread(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EIUThread(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EIUThread(void) { }
	#pragma option pop
	
};


class DELPHICLASS EDragDrop;
class PASCALIMPLEMENTATION EDragDrop : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EDragDrop(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EDragDrop(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EDragDrop(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EDragDrop(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EDragDrop(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EDragDrop(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EDragDrop(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EDragDrop(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EDragDrop(void) { }
	#pragma option pop
	
};


class DELPHICLASS EInvalidFileName;
class PASCALIMPLEMENTATION EInvalidFileName : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EInvalidFileName(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EInvalidFileName(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EInvalidFileName(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EInvalidFileName(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EInvalidFileName(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EInvalidFileName(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EInvalidFileName(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidFileName(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EInvalidFileName(void) { }
	#pragma option pop
	
};


class DELPHICLASS ERenameFileFailed;
class PASCALIMPLEMENTATION ERenameFileFailed : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall ERenameFileFailed(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall ERenameFileFailed(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall ERenameFileFailed(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall ERenameFileFailed(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall ERenameFileFailed(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall ERenameFileFailed(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall ERenameFileFailed(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall ERenameFileFailed(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~ERenameFileFailed(void) { }
	#pragma option pop
	
};


#pragma option push -b-
enum TClipboardOperation { cboNone, cboCut, cboCopy };
#pragma option pop

#pragma option push -b-
enum TFileNameDisplay { fndStored, fndCap, fndNoCap, fndNice };
#pragma option pop

typedef SmallString<4>  TExtStr;

struct TFileRec;
typedef TFileRec *PFileRec;

struct TFileRec
{
	bool Empty;
	bool IconEmpty;
	bool IsDirectory;
	bool IsRecycleBin;
	bool IsParentDir;
	AnsiString FileName;
	AnsiString Displayname;
	TExtStr FileExt;
	AnsiString TypeName;
	int ImageIndex;
	__int64 Size;
	unsigned Attr;
	_FILETIME FileTime;
	_ITEMIDLIST *PIDL;
} ;

struct TInfoCache;
typedef TInfoCache *PInfoCache;

#pragma pack(push, 4)
struct TInfoCache
{
	TExtStr FileExt;
	System::ShortString TypeName;
	int ImageIndex;
} ;
#pragma pack(pop)

typedef void __fastcall (__closure *TDirViewAddFileEvent)(System::TObject* Sender, Sysutils::TSearchRec &SearchRec, bool &AddFile);

typedef void __fastcall (__closure *TDirViewFileSizeChanged)(System::TObject* Sender, Comctrls::TListItem* Item);

class DELPHICLASS TSubDirScanner;
class DELPHICLASS TDirView;
class DELPHICLASS TIconUpdateThread;
class PASCALIMPLEMENTATION TIconUpdateThread : public Compthread::TCompThread 
{
	typedef Compthread::TCompThread inherited;
	
private:
	TDirView* FOwner;
	int FIndex;
	int FMaxIndex;
	bool FNewIcons;
	int FSyncIcon;
	int CurrentIndex;
	AnsiString CurrentFilePath;
	TFileRec CurrentItemData;
	bool InvalidItem;
	void __fastcall SetIndex(int Value);
	void __fastcall SetMaxIndex(int Value);
	
protected:
	__fastcall TIconUpdateThread(TDirView* Owner);
	void __fastcall DoFetchData(void);
	void __fastcall DoUpdateIcon(void);
	virtual void __fastcall Execute(void);
	HIDESBASE void __fastcall Terminate(void);
	__property int Index = {read=FIndex, write=SetIndex, nodefault};
	__property int MaxIndex = {read=FMaxIndex, write=SetMaxIndex, nodefault};
public:
	#pragma option push -w-inl
	/* TCompThread.Destroy */ inline __fastcall virtual ~TIconUpdateThread(void) { }
	#pragma option pop
	
};


class PASCALIMPLEMENTATION TDirView : public Customdirview::TCustomDirView 
{
	typedef Customdirview::TCustomDirView inherited;
	
private:
	bool FConfirmDelete;
	bool FConfirmOverwrite;
	bool FUseIconCache;
	Listext::TListExt* FInfoCacheList;
	Extctrls::TTimer* FChangeTimer;
	unsigned FChangeInterval;
	bool FUseIconUpdateThread;
	bool FIUThreadFinished;
	int FDriveType;
	AnsiString FAttrSpace;
	AnsiString FNoCheckDrives;
	bool FSortAfterUpdate;
	Graphics::TColor FCompressedColor;
	TFileNameDisplay FFileNameDisplay;
	_di_IShellFolder FParentFolder;
	_di_IShellFolder FDesktopFolder;
	bool FDirOK;
	AnsiString FPath;
	bool FDrawLinkOverlay;
	bool SelectNewFiles;
	Customdirview::TSelAttr FSelArchive;
	Customdirview::TSelAttr FSelHidden;
	Customdirview::TSelAttr FSelSysFile;
	Customdirview::TSelAttr FSelReadOnly;
	__int64 FSelFileSizeFrom;
	__int64 FSelFileSizeTo;
	Word FSelFileDateFrom;
	Word FSelFileDateTo;
	Word FSelFileTimeFrom;
	Word FSelFileTimeTo;
	Fileoperator::TFileOperator* FFileOperator;
	TIconUpdateThread* FIconUpdateThread;
	Discmon::TDiscMonitor* FDiscMonitor;
	AnsiString FHomeDirectory;
	Classes::TList* FSubDirScanner;
	TDirViewAddFileEvent FOnAddFile;
	TDirViewFileSizeChanged FOnFileSizeChanged;
	Classes::TNotifyEvent FOnChangeDetected;
	Classes::TNotifyEvent FOnChangeInvalid;
	_di_IShellFolder iRecycleFolder;
	_ITEMIDLIST *PIDLRecycle;
	Dirviewcolproperties::TDirViewColProperties* __fastcall GetDirColProperties(void);
	AnsiString __fastcall GetHomeDirectory();
	void __fastcall SignalFileDelete(System::TObject* Sender, Classes::TStringList* Files);
	void __fastcall PerformDragDropFileOperation(AnsiString TargetPath, int dwEffect, bool RenameOnCollision);
	void __fastcall SetDirColProperties(Dirviewcolproperties::TDirViewColProperties* Value);
	
protected:
	virtual Listviewcolproperties::TCustomListViewColProperties* __fastcall NewColProperties(void);
	virtual void __fastcall SetShowSubDirSize(bool Value);
	DYNAMIC void __fastcall Delete(Comctrls::TListItem* Item);
	virtual void __fastcall SetMask(AnsiString Value);
	void __fastcall DDError(Customdirview::TDDError ErrorNo);
	virtual bool __fastcall GetCanUndoCopyMove(void);
	_di_IShellFolder __fastcall GetShellFolder(AnsiString Dir);
	virtual bool __fastcall GetDirOK(void);
	virtual void __fastcall GetDisplayInfo(Comctrls::TListItem* ListItem, tagLVITEMA &DispInfo);
	virtual void __fastcall DDDragDetect(int grfKeyState, const Types::TPoint &DetectStart, const Types::TPoint &Point, Dragdrop::TDragDetectStatus DragStatus);
	virtual void __fastcall DDMenuDone(System::TObject* Sender, HMENU AMenu);
	virtual void __fastcall DDDropHandlerSucceeded(System::TObject* Sender, int grfKeyState, const Types::TPoint &Point, int dwEffect);
	virtual void __fastcall DDChooseEffect(int grfKeyState, int &dwEffect);
	virtual AnsiString __fastcall GetPathName();
	virtual void __fastcall SetChangeInterval(unsigned Value);
	virtual void __fastcall LoadFromRecycleBin(AnsiString Dir);
	virtual void __fastcall SetLoadEnabled(bool Value);
	virtual AnsiString __fastcall GetPath();
	virtual void __fastcall SetPath(AnsiString Value);
	virtual void __fastcall SetItemImageIndex(Comctrls::TListItem* Item, int Index);
	void __fastcall SetCompressedColor(Graphics::TColor Value);
	void __fastcall ChangeDetected(System::TObject* Sender);
	void __fastcall ChangeInvalid(System::TObject* Sender);
	void __fastcall TimerOnTimer(System::TObject* Sender);
	void __fastcall ResetItemImage(int Index);
	void __fastcall SetAttrSpace(AnsiString Value);
	void __fastcall SetNoCheckDrives(AnsiString Value);
	virtual void __fastcall SetWatchForChanges(bool Value);
	void __fastcall AddParentDirItem(void);
	virtual void __fastcall AddToDragFileList(Dragdropfilesex::TFileList* FileList, Comctrls::TListItem* Item);
	virtual void __fastcall SetFileNameDisplay(TFileNameDisplay Value);
	virtual void __fastcall DisplayContextMenu(const Types::TPoint &Where);
	virtual bool __fastcall DragCompleteFileList(void);
	virtual void __fastcall ExecuteFile(Comctrls::TListItem* Item);
	virtual bool __fastcall GetIsRoot(void);
	virtual void __fastcall InternalEdit(const tagLVITEMA &HItem);
	virtual Graphics::TColor __fastcall ItemColor(Comctrls::TListItem* Item);
	virtual AnsiString __fastcall ItemDisplayName(AnsiString FileName);
	AnsiString __fastcall ItemFileExt(Comctrls::TListItem* Item);
	AnsiString __fastcall ItemFileNameOnly(Comctrls::TListItem* Item);
	virtual __int64 __fastcall ItemFileSize(Comctrls::TListItem* Item);
	virtual System::TDateTime __fastcall ItemFileTime(Comctrls::TListItem* Item, Baseutils::TDateTimePrecision &Precision);
	virtual int __fastcall ItemImageIndex(Comctrls::TListItem* Item, bool Cache);
	virtual bool __fastcall ItemIsFile(Comctrls::TListItem* Item);
	virtual bool __fastcall ItemIsRecycleBin(Comctrls::TListItem* Item);
	virtual bool __fastcall ItemMatchesFilter(Comctrls::TListItem* Item, const Customdirview::TFileFilter &Filter);
	virtual Word __fastcall ItemOverlayIndexes(Comctrls::TListItem* Item);
	virtual void __fastcall LoadFiles(void);
	virtual AnsiString __fastcall MinimizePath(AnsiString Path, int Len);
	virtual void __fastcall PerformItemDragDropOperation(Comctrls::TListItem* Item, int Effect);
	virtual void __fastcall SortItems(void);
	void __fastcall StartFileDeleteThread(void);
	virtual void __fastcall SetShowHiddenFiles(bool Value);
	HIDESBASE MESSAGE void __fastcall WMDestroy(Messages::TWMNoParams &Msg);
	
public:
	__property int DriveType = {read=FDriveType, nodefault};
	__property Items  = {stored=false};
	__property Columns  = {stored=false};
	__property _di_IShellFolder ParentFolder = {read=FParentFolder};
	__property bool CanUndoCopyMove = {read=GetCanUndoCopyMove, nodefault};
	__property Fileoperator::TFileOperator* DDFileOperator = {read=FFileOperator};
	DYNAMIC bool __fastcall UndoCopyMove(void);
	DYNAMIC void __fastcall EmptyClipboard(void);
	DYNAMIC bool __fastcall CopyToClipBoard(void);
	DYNAMIC bool __fastcall CutToClipBoard(void);
	DYNAMIC bool __fastcall CanPasteFromClipBoard(void);
	DYNAMIC bool __fastcall PasteFromClipBoard(AnsiString TargetPath = "");
	DYNAMIC bool __fastcall DuplicateSelectedFiles(void);
	virtual void __fastcall DisplayPropertiesMenu(void);
	virtual void __fastcall ExecuteParentDirectory(void);
	virtual void __fastcall ExecuteRootDirectory(void);
	virtual bool __fastcall ItemIsDirectory(Comctrls::TListItem* Item);
	virtual AnsiString __fastcall ItemFullFileName(Comctrls::TListItem* Item);
	virtual bool __fastcall ItemIsParentDirectory(Comctrls::TListItem* Item);
	virtual AnsiString __fastcall ItemFileName(Comctrls::TListItem* Item);
	void __fastcall StartWatchThread(void);
	void __fastcall StopWatchThread(void);
	bool __fastcall WatchThreadActive(void);
	void __fastcall StartIconUpdateThread(void);
	void __fastcall StopIconUpdateThread(void);
	void __fastcall StartSubDirScanner(void);
	void __fastcall StopSubDirScanner(void);
	void __fastcall TerminateThreads(void);
	void __fastcall Syncronize(void);
	void __fastcall ClearIconCache(void);
	DYNAMIC Comctrls::TListItem* __fastcall CreateFile(AnsiString NewName);
	virtual void __fastcall CreateDirectory(AnsiString DirName);
	DYNAMIC bool __fastcall DeleteSelectedFiles(bool AllowUndo);
	void __fastcall ValidateFile(Comctrls::TListItem* Item)/* overload */;
	void __fastcall ValidateFile(AnsiString FileName)/* overload */;
	DYNAMIC void __fastcall ValidateSelectedFiles(void);
	HIDESBASE Comctrls::TListItem* __fastcall AddItem(const Sysutils::TSearchRec &SRec);
	void __fastcall GetDisplayData(Comctrls::TListItem* Item, bool FetchIcon);
	PFileRec __fastcall GetFileRec(int Index);
	virtual void __fastcall Load(void);
	virtual void __fastcall ReLoad(bool CacheIcons);
	void __fastcall Reload2(void);
	virtual AnsiString __fastcall FormatFileTime(const _FILETIME &FileTime);
	virtual AnsiString __fastcall GetAttrString(int Attr);
	void __fastcall FetchAllDisplayData(void);
	__fastcall virtual TDirView(Classes::TComponent* AOwner);
	__fastcall virtual ~TDirView(void);
	virtual void __fastcall ExecuteHomeDirectory(void);
	virtual void __fastcall ReloadDirectory(void);
	__property AnsiString HomeDirectory = {read=GetHomeDirectory, write=FHomeDirectory};
	__property Customdirview::TSelAttr SelArchive = {read=FSelArchive, write=FSelArchive, default=0};
	__property Customdirview::TSelAttr SelHidden = {read=FSelHidden, write=FSelHidden, default=0};
	__property Customdirview::TSelAttr SelSysFile = {read=FSelSysFile, write=FSelSysFile, default=0};
	__property Customdirview::TSelAttr SelReadOnly = {read=FSelReadOnly, write=FSelReadOnly, default=0};
	__property __int64 SelFileSizeFrom = {read=FSelFileSizeFrom, write=FSelFileSizeFrom};
	__property __int64 SelFileSizeTo = {read=FSelFileSizeTo, write=FSelFileSizeTo, default=0};
	__property Word SelFileDateFrom = {read=FSelFileDateFrom, write=FSelFileDateFrom, default=33};
	__property Word SelFileDateTo = {read=FSelFileDateTo, write=FSelFileDateTo, default=61343};
	__property Word SelFileTimeFrom = {read=FSelFileTimeFrom, write=FSelFileTimeFrom, nodefault};
	__property Word SelFileTimeTo = {read=FSelFileTimeTo, write=FSelFileTimeTo, default=49152};
	
__published:
	__property Dirviewcolproperties::TDirViewColProperties* DirColProperties = {read=GetDirColProperties, write=SetDirColProperties};
	__property PathComboBox ;
	__property PathLabel ;
	__property StatusBar ;
	__property OnGetSelectFilter ;
	__property HeaderImages ;
	__property LoadAnimation  = {default=1};
	__property DimmHiddenFiles  = {default=1};
	__property ShowDirectories  = {default=1};
	__property ShowHiddenFiles  = {default=1};
	__property DirsOnTop  = {default=1};
	__property ShowSubDirSize  = {default=0};
	__property SingleClickToExec  = {default=0};
	__property WantUseDragImages  = {default=1};
	__property TargetPopupMenu  = {default=1};
	__property AddParentDir  = {default=0};
	__property OnSelectItem ;
	__property OnStartLoading ;
	__property OnLoaded ;
	__property OnDDDragEnter ;
	__property OnDDDragLeave ;
	__property OnDDDragOver ;
	__property OnDDDrop ;
	__property OnDDQueryContinueDrag ;
	__property OnDDGiveFeedback ;
	__property OnDDDragDetect ;
	__property OnDDCreateDragFileList ;
	__property OnDDEnd ;
	__property OnDDCreateDataObject ;
	__property OnDDTargetHasDropHandler ;
	__property DDLinkOnExeDrag  = {default=1};
	__property OnDDProcessDropped ;
	__property OnDDError ;
	__property OnDDExecuted ;
	__property OnDDFileOperation ;
	__property OnDDFileOperationExecuted ;
	__property OnDDMenuPopup ;
	__property OnExecFile ;
	__property Graphics::TColor CompressedColor = {read=FCompressedColor, write=SetCompressedColor, default=16711680};
	__property bool ConfirmDelete = {read=FConfirmDelete, write=FConfirmDelete, default=1};
	__property bool ConfirmOverwrite = {read=FConfirmOverwrite, write=FConfirmOverwrite, default=1};
	__property bool SortAfterUpdate = {read=FSortAfterUpdate, write=FSortAfterUpdate, default=1};
	__property unsigned ChangeInterval = {read=FChangeInterval, write=SetChangeInterval, default=1000};
	__property bool UseIconUpdateThread = {read=FUseIconUpdateThread, write=FUseIconUpdateThread, default=0};
	__property bool UseIconCache = {read=FUseIconCache, write=FUseIconCache, default=0};
	__property TFileNameDisplay FileNameDisplay = {read=FFileNameDisplay, write=SetFileNameDisplay, default=0};
	__property AnsiString AttrSpace = {read=FAttrSpace, write=SetAttrSpace};
	__property AnsiString NoCheckDrives = {read=FNoCheckDrives, write=SetNoCheckDrives};
	__property WatchForChanges  = {default=0};
	__property Classes::TNotifyEvent OnChangeDetected = {read=FOnChangeDetected, write=FOnChangeDetected};
	__property Classes::TNotifyEvent OnChangeInvalid = {read=FOnChangeInvalid, write=FOnChangeInvalid};
	__property TDirViewAddFileEvent OnAddFile = {read=FOnAddFile, write=FOnAddFile};
	__property TDirViewFileSizeChanged OnFileSizeChanged = {read=FOnFileSizeChanged, write=FOnFileSizeChanged};
	__property UseSystemContextMenu  = {default=1};
	__property OnContextPopup ;
	__property OnBeginRename ;
	__property OnEndRename ;
	__property OnHistoryChange ;
	__property ColumnClick  = {default=1};
	__property MultiSelect  = {default=1};
	__property ReadOnly  = {default=0};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TDirView(HWND ParentWindow) : Customdirview::TCustomDirView(ParentWindow) { }
	#pragma option pop
	
};


class PASCALIMPLEMENTATION TSubDirScanner : public Compthread::TCompThread 
{
	typedef Compthread::TCompThread inherited;
	
private:
	TDirView* FOwner;
	AnsiString FStartPath;
	AnsiString FDirName;
	__int64 FTotalSize;
	void __fastcall ThreadTerminated(System::TObject* Sender);
	
protected:
	__fastcall TSubDirScanner(TDirView* Owner, Comctrls::TListItem* Item);
	void __fastcall DoUpdateItem(void);
	virtual void __fastcall Execute(void);
public:
	#pragma option push -w-inl
	/* TCompThread.Destroy */ inline __fastcall virtual ~TSubDirScanner(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::ResourceString _coFileOperatorTitle;
#define Dirview_coFileOperatorTitle System::LoadResourceString(&Dirview::_coFileOperatorTitle)
extern PACKAGE System::ResourceString _coInvalidDosChars;
#define Dirview_coInvalidDosChars System::LoadResourceString(&Dirview::_coInvalidDosChars)
extern PACKAGE System::ResourceString _Space;
#define Dirview_Space System::LoadResourceString(&Dirview::_Space)
static const Shortint msThreadChangeDelay = 0xa;
static const Shortint MaxWaitTimeOut = 0xa;
static const Shortint FileAttr = 0x37;
static const Shortint ExtLen = 0x4;
#define SpecialExtensions "EXE,LNK,ICO,ANI,CUR,PIF,JOB,CPL"
#define ExeExtension "EXE"
static const Shortint MinDate = 0x21;
static const Word MaxDate = 0xef9f;
static const Shortint MinTime = 0x0;
static const Word MaxTime = 0xc000;
extern PACKAGE TClipboardOperation LastClipBoardOperation;
extern PACKAGE unsigned LastIOResult;
extern PACKAGE void __fastcall Register(void);
extern PACKAGE bool __fastcall MatchesFileExt( TExtStr &Ext, const AnsiString FileExtList);

}	/* namespace Dirview */
using namespace Dirview;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DirView
