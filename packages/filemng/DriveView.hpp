// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DriveView.pas' rev: 6.00

#ifndef DriveViewHPP
#define DriveViewHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Menus.hpp>	// Pascal unit
#include <CustomDriveView.hpp>	// Pascal unit
#include <CustomDirView.hpp>	// Pascal unit
#include <ListExt.hpp>	// Pascal unit
#include <BaseUtils.hpp>	// Pascal unit
#include <PIDL.hpp>	// Pascal unit
#include <IEListView.hpp>	// Pascal unit
#include <IEDriveInfo.hpp>	// Pascal unit
#include <DiscMon.hpp>	// Pascal unit
#include <FileOperator.hpp>	// Pascal unit
#include <FileChanges.hpp>	// Pascal unit
#include <DragDropFilesEx.hpp>	// Pascal unit
#include <DragDrop.hpp>	// Pascal unit
#include <ShellDialogs.hpp>	// Pascal unit
#include <DirView.hpp>	// Pascal unit
#include <ShlObj.hpp>	// Pascal unit
#include <ActiveX.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <CommCtrl.hpp>	// Pascal unit
#include <ShellAPI.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <ComObj.hpp>	// Pascal unit
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

namespace Driveview
{
//-- type declarations -------------------------------------------------------
typedef SmallString<12>  TString12;

class DELPHICLASS ECreateShortCut;
class PASCALIMPLEMENTATION ECreateShortCut : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall ECreateShortCut(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall ECreateShortCut(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall ECreateShortCut(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall ECreateShortCut(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall ECreateShortCut(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall ECreateShortCut(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall ECreateShortCut(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall ECreateShortCut(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~ECreateShortCut(void) { }
	#pragma option pop
	
};


class DELPHICLASS EInvalidDirName;
class PASCALIMPLEMENTATION EInvalidDirName : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EInvalidDirName(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EInvalidDirName(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EInvalidDirName(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EInvalidDirName(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EInvalidDirName(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EInvalidDirName(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EInvalidDirName(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidDirName(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EInvalidDirName(void) { }
	#pragma option pop
	
};


class DELPHICLASS EInvalidPath;
class PASCALIMPLEMENTATION EInvalidPath : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EInvalidPath(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EInvalidPath(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EInvalidPath(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EInvalidPath(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EInvalidPath(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EInvalidPath(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EInvalidPath(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidPath(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EInvalidPath(void) { }
	#pragma option pop
	
};


class DELPHICLASS ENodeNotAssigned;
class PASCALIMPLEMENTATION ENodeNotAssigned : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall ENodeNotAssigned(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall ENodeNotAssigned(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall ENodeNotAssigned(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall ENodeNotAssigned(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall ENodeNotAssigned(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall ENodeNotAssigned(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall ENodeNotAssigned(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall ENodeNotAssigned(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~ENodeNotAssigned(void) { }
	#pragma option pop
	
};


#pragma pack(push, 4)
struct TDriveStatus
{
	bool Scanned;
	bool Verified;
	Comctrls::TTreeNode* RootNode;
	Discmon::TDiscMonitor* DiscMonitor;
	Extctrls::TTimer* ChangeTimer;
	AnsiString DefaultDir;
} ;
#pragma pack(pop)

#pragma pack(push, 4)
struct TScanDirInfo
{
	bool SearchNewDirs;
	Comctrls::TTreeNode* StartNode;
	int DriveType;
} ;
#pragma pack(pop)

typedef TScanDirInfo *PScanDirInfo;

typedef void __fastcall (__closure *TDriveViewScanDirEvent)(System::TObject* Sender, Comctrls::TTreeNode* Node, bool &DoScanDir);

typedef void __fastcall (__closure *TDriveViewDiskChangeEvent)(System::TObject* Sender, char Drive);

class DELPHICLASS TNodeData;
class PASCALIMPLEMENTATION TNodeData : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	AnsiString FDirName;
	TString12 FShortName;
	int FAttr;
	bool FScanned;
	void *FData;
	bool FExpanded;
	unsigned FDirSize;
	bool FIsRecycleBin;
	bool FIconEmpty;
	
public:
	unsigned shAttr;
	_ITEMIDLIST *PIDL;
	_di_IShellFolder ShellFolder;
	__fastcall TNodeData(void);
	__fastcall virtual ~TNodeData(void);
	__property AnsiString DirName = {read=FDirName, write=FDirName};
	__property TString12 ShortName = {read=FShortName, write=FShortName};
	__property int Attr = {read=FAttr, write=FAttr, nodefault};
	__property bool Scanned = {read=FScanned, write=FScanned, nodefault};
	__property void * Data = {read=FData, write=FData};
	__property bool Expanded = {read=FExpanded, write=FExpanded, nodefault};
	__property unsigned DirSize = {read=FDirSize, write=FDirSize, nodefault};
	__property bool IsRecycleBin = {read=FIsRecycleBin, nodefault};
	__property bool IconEmpty = {read=FIconEmpty, write=FIconEmpty, nodefault};
};


typedef TDriveStatus DriveView__7[26];

class DELPHICLASS TDriveView;
class PASCALIMPLEMENTATION TDriveView : public Customdriveview::TCustomDriveView 
{
	typedef Customdriveview::TCustomDriveView inherited;
	
private:
	TDriveStatus DriveStatus[26];
	bool FConfirmDelete;
	bool FConfirmOverwrite;
	bool FWatchDirectory;
	AnsiString FDirectory;
	bool FFullDriveScan;
	bool FShowDirSize;
	bool FShowVolLabel;
	Dirview::TVolumeDisplayStyle FVolDisplayStyle;
	bool FShowAnimation;
	bool FChangeFlag;
	AnsiString FLastDir;
	bool FValidateFlag;
	bool FCreating;
	bool FForceRename;
	Comctrls::TTreeNode* FRenameNode;
	AnsiString FLastRenameName;
	HWND FInternalWindowHandle;
	Comctrls::TTreeNode* FPrevSelected;
	_di_IShellFolder FDesktop;
	_di_IShellFolder FWorkPlace;
	Classes::TNotifyEvent FOnStartScan;
	Classes::TNotifyEvent FOnEndScan;
	TDriveViewScanDirEvent FOnScanDir;
	TDriveViewDiskChangeEvent FOnDiskChange;
	TDriveViewDiskChangeEvent FOnInsertedDiskChange;
	TDriveViewDiskChangeEvent FOnChangeDetected;
	TDriveViewDiskChangeEvent FOnChangeInvalid;
	Classes::TNotifyEvent FOnDisplayContextMenu;
	Classes::TNotifyEvent FOnRefreshDrives;
	Dirview::TDirView* FDirView;
	Fileoperator::TFileOperator* FFileOperator;
	unsigned FChangeInterval;
	AnsiString FNoCheckDrives;
	Graphics::TColor FCompressedColor;
	Dirview::TFileNameDisplay FFileNameDisplay;
	AnsiString FLastPathCut;
	void __fastcall SignalDirDelete(System::TObject* Sender, Classes::TStringList* Files);
	bool __fastcall CheckForSubDirs(AnsiString Path);
	bool __fastcall ReadSubDirs(Comctrls::TTreeNode* Node, int DriveType);
	bool __fastcall CallBackValidateDir(Comctrls::TTreeNode* &Node, void * Data);
	bool __fastcall CallBackSaveNodeState(Comctrls::TTreeNode* &Node, void * Data);
	bool __fastcall CallBackRestoreNodeState(Comctrls::TTreeNode* &Node, void * Data);
	bool __fastcall CallBackDisplayName(Comctrls::TTreeNode* &Node, void * Data);
	bool __fastcall CallBackSetDirSize(Comctrls::TTreeNode* &Node, void * Data);
	bool __fastcall CallBackExpandLevel(Comctrls::TTreeNode* &Node, void * Data);
	void __fastcall ChangeDetected(System::TObject* Sender, const AnsiString Directory, bool &SubdirsChanged);
	void __fastcall ChangeInvalid(System::TObject* Sender, const AnsiString Directory, const AnsiString ErrorStr);
	void __fastcall ChangeTimerOnTimer(System::TObject* Sender);
	
protected:
	HIDESBASE void __fastcall SetSelected(Comctrls::TTreeNode* Node);
	void __fastcall SetFullDriveScan(bool DoFullDriveScan);
	void __fastcall SetWatchDirectory(bool Value);
	void __fastcall SetShowDirSize(bool ShowIt);
	void __fastcall SetShowVolLabel(bool ShowIt);
	void __fastcall SetVolDisplayStyle(Dirview::TVolumeDisplayStyle DoStyle);
	void __fastcall SetDirView(Dirview::TDirView* Value);
	void __fastcall SetChangeInterval(unsigned Value);
	void __fastcall SetNoCheckDrives(AnsiString Value);
	void __fastcall SetCompressedColor(Graphics::TColor Value);
	void __fastcall SetFileNameDisplay(Dirview::TFileNameDisplay Value);
	virtual void __fastcall SetDirectory(AnsiString Value);
	void __fastcall SetDrive(char Drive);
	char __fastcall GetDrive(void);
	void __fastcall GetNodeShellAttr(_di_IShellFolder ParentFolder, TNodeData* NodeData, AnsiString Path, bool ContentMask = true);
	virtual bool __fastcall DoScanDir(Comctrls::TTreeNode* FromNode);
	virtual Comctrls::TTreeNode* __fastcall AddChildNode(Comctrls::TTreeNode* ParentNode, const Sysutils::TSearchRec &SRec);
	virtual void __fastcall CreateWatchThread(char Drive);
	void __fastcall InternalWndProc(Messages::TMessage &Msg);
	int __fastcall DirAttrMask(void);
	virtual void __fastcall ValidateDirectoryEx(Comctrls::TTreeNode* Node, Customdriveview::TRecursiveScan Recurse, bool NewDirs);
	void __fastcall ValidateDirectoryEasy(Comctrls::TTreeNode* Node);
	virtual void __fastcall RebuildTree(void);
	void __fastcall SetLastPathCut(AnsiString Path);
	virtual bool __fastcall GetCanUndoCopyMove(void);
	virtual void __fastcall CreateWnd(void);
	DYNAMIC void __fastcall Edit(const tagTVITEMA &Item);
	MESSAGE void __fastcall WMUserRename(Messages::TMessage &Message);
	virtual Customdirview::TCustomDirView* __fastcall GetCustomDirView(void);
	virtual void __fastcall SetCustomDirView(Customdirview::TCustomDirView* Value);
	virtual AnsiString __fastcall NodePath(Comctrls::TTreeNode* Node);
	virtual bool __fastcall NodeIsRecycleBin(Comctrls::TTreeNode* Node);
	virtual bool __fastcall NodePathExists(Comctrls::TTreeNode* Node);
	virtual Graphics::TColor __fastcall NodeColor(Comctrls::TTreeNode* Node);
	virtual Comctrls::TTreeNode* __fastcall FindPathNode(AnsiString Path);
	virtual Dragdrop::TDropEffectSet __fastcall DDSourceEffects(void);
	virtual void __fastcall DDChooseEffect(int KeyState, int &Effect);
	virtual bool __fastcall DragCompleteFileList(void);
	virtual Dragdrop::TDragResult __fastcall DDExecute(void);
	
public:
	__property Images ;
	__property StateImages ;
	__property Items  = {stored=false};
	__property Selected  = {write=SetSelected, stored=false};
	__property _di_IShellFolder WorkPlace = {read=FWorkPlace};
	__property Controls::TDragImageList* DragImageList = {read=FDragImageList};
	__property char Drive = {read=GetDrive, write=SetDrive, stored=false, nodefault};
	__property char DragDrive = {read=FDragDrive, nodefault};
	__property bool CanUndoCopyMove = {read=GetCanUndoCopyMove, nodefault};
	__property Fileoperator::TFileOperator* DDFileOperator = {read=FFileOperator};
	__property AnsiString LastPathCut = {read=FLastPathCut, write=SetLastPathCut};
	DYNAMIC bool __fastcall UndoCopyMove(void);
	DYNAMIC void __fastcall EmptyClipboard(void);
	DYNAMIC bool __fastcall CopyToClipBoard(Comctrls::TTreeNode* Node);
	DYNAMIC bool __fastcall CutToClipBoard(Comctrls::TTreeNode* Node);
	DYNAMIC bool __fastcall CanPasteFromClipBoard(void);
	DYNAMIC bool __fastcall PasteFromClipBoard(AnsiString TargetPath = "");
	virtual void __fastcall PerformDragDropFileOperation(Comctrls::TTreeNode* Node, int Effect);
	TDriveStatus __fastcall GetDriveStatus(char Drive);
	int __fastcall GetDriveTypetoNode(Comctrls::TTreeNode* Node);
	int __fastcall GetDriveType(char Drive);
	char __fastcall GetDriveToNode(Comctrls::TTreeNode* Node);
	AnsiString __fastcall GetDriveText(char Drive);
	void __fastcall ScanDrive(char Drive);
	void __fastcall RefreshRootNodes(bool ScanDirectory, int dsFlags);
	AnsiString __fastcall GetValidDrivesStr();
	void __fastcall RefreshDirSize(Comctrls::TTreeNode* Node);
	void __fastcall RefreshDriveDirSize(char Drive);
	virtual void __fastcall SetImageIndex(Comctrls::TTreeNode* Node);
	Comctrls::TTreeNode* __fastcall FindNodeToPath(AnsiString Path);
	bool __fastcall NodeVerified(Comctrls::TTreeNode* Node);
	int __fastcall NodeAttr(Comctrls::TTreeNode* Node);
	Comctrls::TTreeNode* __fastcall RootNode(Comctrls::TTreeNode* Node);
	AnsiString __fastcall GetDirName(Comctrls::TTreeNode* Node);
	virtual unsigned __fastcall GetDirSize(Comctrls::TTreeNode* Node);
	virtual void __fastcall SetDirSize(Comctrls::TTreeNode* Node);
	AnsiString __fastcall GetDisplayName(Comctrls::TTreeNode* Node);
	virtual bool __fastcall NodeUpdateAble(Comctrls::TTreeNode* Node);
	virtual AnsiString __fastcall FormatDirSize(unsigned Size);
	virtual void __fastcall ExpandLevel(Comctrls::TTreeNode* Node, int Level);
	virtual AnsiString __fastcall NodePathName(Comctrls::TTreeNode* Node);
	Shlobj::PItemIDList __fastcall GetFQPIDL(Comctrls::TTreeNode* Node);
	DYNAMIC int __fastcall GetSubTreeSize(Comctrls::TTreeNode* Node);
	DYNAMIC Comctrls::TTreeNode* __fastcall CreateDirectory(Comctrls::TTreeNode* ParentNode, AnsiString NewName);
	DYNAMIC bool __fastcall DeleteDirectory(Comctrls::TTreeNode* Node, bool AllowUndo);
	DYNAMIC void __fastcall DeleteSubNodes(Comctrls::TTreeNode* Node);
	__fastcall virtual TDriveView(Classes::TComponent* AOwner);
	__fastcall virtual ~TDriveView(void);
	void __fastcall SaveNodesState(Comctrls::TTreeNode* Node);
	void __fastcall RestoreNodesState(Comctrls::TTreeNode* Node);
	virtual void __fastcall DisplayContextMenu(Comctrls::TTreeNode* Node, const Types::TPoint &Point);
	virtual void __fastcall DisplayPropertiesMenu(Comctrls::TTreeNode* Node);
	virtual void __fastcall StartWatchThread(void);
	virtual void __fastcall StopWatchThread(void);
	virtual void __fastcall TerminateWatchThread(char Drive);
	virtual void __fastcall StartAllWatchThreads(void);
	virtual void __fastcall StopAllWatchThreads(void);
	bool __fastcall WatchThreadActive(void)/* overload */;
	bool __fastcall WatchThreadActive(char Drive)/* overload */;
	virtual bool __fastcall NodeWatched(Comctrls::TTreeNode* Node);
	virtual void __fastcall GetImageIndex(Comctrls::TTreeNode* Node);
	DYNAMIC bool __fastcall CanEdit(Comctrls::TTreeNode* Node);
	DYNAMIC bool __fastcall CanChange(Comctrls::TTreeNode* Node);
	DYNAMIC bool __fastcall CanExpand(Comctrls::TTreeNode* Node);
	DYNAMIC void __fastcall Delete(Comctrls::TTreeNode* Node);
	virtual void __fastcall Loaded(void);
	DYNAMIC void __fastcall KeyPress(char &Key);
	DYNAMIC void __fastcall Change(Comctrls::TTreeNode* Node);
	
__published:
	__property Directory ;
	__property bool ConfirmDelete = {read=FConfirmDelete, write=FConfirmDelete, default=1};
	__property bool ConfirmOverwrite = {read=FConfirmOverwrite, write=FConfirmOverwrite, default=1};
	__property bool FullDriveScan = {read=FFullDriveScan, write=SetFullDriveScan, default=0};
	__property bool WatchDirectory = {read=FWatchDirectory, write=SetWatchDirectory, default=0};
	__property unsigned ChangeInterval = {read=FChangeInterval, write=SetChangeInterval, default=1000};
	__property Dirview::TDirView* DirView = {read=FDirView, write=SetDirView};
	__property bool ShowDirSize = {read=FShowDirSize, write=SetShowDirSize, default=0};
	__property bool ShowVolLabel = {read=FShowVolLabel, write=SetShowVolLabel, default=1};
	__property Dirview::TVolumeDisplayStyle VolDisplayStyle = {read=FVolDisplayStyle, write=SetVolDisplayStyle, default=0};
	__property bool ShowAnimation = {read=FShowAnimation, write=FShowAnimation, default=0};
	__property AnsiString NoCheckDrives = {read=FNoCheckDrives, write=SetNoCheckDrives};
	__property Graphics::TColor CompressedColor = {read=FCompressedColor, write=SetCompressedColor, default=16711680};
	__property Dirview::TFileNameDisplay FileNameDisplay = {read=FFileNameDisplay, write=SetFileNameDisplay, default=0};
	__property Classes::TNotifyEvent OnStartScan = {read=FOnStartScan, write=FOnStartScan};
	__property Classes::TNotifyEvent OnEndScan = {read=FOnEndScan, write=FOnEndScan};
	__property TDriveViewScanDirEvent OnScanDir = {read=FOnScanDir, write=FOnScanDir};
	__property TDriveViewDiskChangeEvent OnDiskChange = {read=FOnDiskChange, write=FOnDiskChange};
	__property TDriveViewDiskChangeEvent OnInsertedDiskChange = {read=FOnInsertedDiskChange, write=FOnInsertedDiskChange};
	__property TDriveViewDiskChangeEvent OnChangeDetected = {read=FOnChangeDetected, write=FOnChangeDetected};
	__property TDriveViewDiskChangeEvent OnChangeInvalid = {read=FOnChangeInvalid, write=FOnChangeInvalid};
	__property Classes::TNotifyEvent OnDisplayContextMenu = {read=FOnDisplayContextMenu, write=FOnDisplayContextMenu};
	__property Classes::TNotifyEvent OnRefreshDrives = {read=FOnRefreshDrives, write=FOnRefreshDrives};
	__property DDLinkOnExeDrag  = {default=1};
	__property UseDragImages  = {default=1};
	__property TargetPopUpMenu  = {default=1};
	__property OnDDDragEnter ;
	__property OnDDDragLeave ;
	__property OnDDDragOver ;
	__property OnDDDrop ;
	__property OnDDQueryContinueDrag ;
	__property OnDDGiveFeedback ;
	__property OnDDDragDetect ;
	__property OnDDProcessDropped ;
	__property OnDDError ;
	__property OnDDExecuted ;
	__property OnDDFileOperation ;
	__property OnDDFileOperationExecuted ;
	__property OnDDMenuPopup ;
	__property Align  = {default=0};
	__property Anchors  = {default=3};
	__property AutoExpand  = {default=0};
	__property BiDiMode ;
	__property BorderStyle  = {default=1};
	__property BorderWidth  = {default=0};
	__property ChangeDelay  = {default=0};
	__property Color  = {default=-2147483643};
	__property Ctl3D ;
	__property Constraints ;
	__property DragKind  = {default=0};
	__property DragCursor  = {default=-12};
	__property DragMode  = {default=1};
	__property OnDragDrop ;
	__property OnDragOver ;
	__property Enabled  = {default=1};
	__property Font ;
	__property HideSelection  = {default=1};
	__property HotTrack  = {default=0};
	__property Indent ;
	__property ParentBiDiMode  = {default=1};
	__property ParentColor  = {default=1};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ReadOnly  = {default=0};
	__property RightClickSelect  = {default=0};
	__property RowSelect  = {default=0};
	__property ShowButtons  = {default=1};
	__property ShowHint ;
	__property ShowLines  = {default=1};
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property ToolTips  = {default=1};
	__property Visible  = {default=1};
	__property OnChange ;
	__property OnChanging ;
	__property OnClick ;
	__property OnCollapsing ;
	__property OnCollapsed ;
	__property OnCompare ;
	__property OnDblClick ;
	__property OnDeletion ;
	__property OnEdited ;
	__property OnEditing ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnExpanding ;
	__property OnExpanded ;
	__property OnGetImageIndex ;
	__property OnGetSelectedIndex ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMouseDown ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TDriveView(HWND ParentWindow) : Customdriveview::TCustomDriveView(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::ResourceString _coFileOperatorTitle;
#define Driveview_coFileOperatorTitle System::LoadResourceString(&Driveview::_coFileOperatorTitle)
extern PACKAGE System::ResourceString _coInvalidDosChars;
#define Driveview_coInvalidDosChars System::LoadResourceString(&Driveview::_coInvalidDosChars)
extern PACKAGE System::ResourceString _Space;
#define Driveview_Space System::LoadResourceString(&Driveview::_Space)
static const Shortint msThreadChangeDelay = 0x32;
static const unsigned CInvalidSize = 0xffffffff;
#define ErrorNodeNA "%s: Node not assigned"
static const Shortint dvdsFloppy = 0x8;
static const Shortint dvdsRereadAllways = 0x10;
extern PACKAGE void __fastcall Register(void);

}	/* namespace Driveview */
using namespace Driveview;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DriveView
