//---------------------------------------------------------------------------
#ifndef UnixDirViewH
#define UnixDirViewH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include "CustomUnixDirView.hpp"

#ifndef DESIGN_ONLY
#include <Terminal.h>
#else
struct TCopyParamType {};
#endif
//---------------------------------------------------------------------------
class TUnixDirView;
enum TTransferDirection { tdToRemote, tdToLocal };
enum TTransferType { ttCopy, ttMove };
typedef void __fastcall (__closure *TGetCopyParamEvent)
  (TUnixDirView * Sender, TTransferDirection Direction, TTransferType Type,
   AnsiString &TargetDirectory, TStrings * FileList, TCopyParamType & CopyParam);
typedef void __fastcall (__closure *TWarnLackOfTempSpaceEvent)
  (TUnixDirView * Sender, const AnsiString Path, __int64 RequiredSpace,
    bool & Continue);
#define DefaultDDDeleteDelay 120
#define HOMEDIRECTORY ""
#define CURRENTDIRECTORY "."
//---------------------------------------------------------------------------
class PACKAGE TUnixDirView : public TCustomUnixDirView
{
private:
  bool FDDAllowMove;
  int FDDDeleteDelay;
  bool FDirLoadedAfterChangeDir;
  int FLastDropEffect;
  __int64 FDDTotalSize;
  TNotifyEvent FOldChangeDirectory;
  TNotifyEvent FOldReadDirectory;
  TNotifyEvent FOnDisplayProperties;
  TGetCopyParamEvent FOnGetCopyParam;
  bool FFullLoad;
#ifndef DESIGN_ONLY
  TTerminal *FTerminal;
#endif
  AnsiString FUniqTempDir;
  bool FShowInaccesibleDirectories;
  AnsiString FDDTemporaryDirectory;
  TWarnLackOfTempSpaceEvent FOnWarnLackOfTempSpace;
  AnsiString FDragDropSshTerminate;
  TStrings * FDDFileList;
  bool __fastcall GetActive();
  TTimer * FDelayedDeletionTimer;
  TStrings * FDelayedDeletionList;
  void __fastcall SetDDAllowMove(bool value);
  void __fastcall SetDDDeleteDelay(int value);
#ifndef DESIGN_ONLY
  void __fastcall SetTerminal(TTerminal *value);
#endif
  void __fastcall SetShowInaccesibleDirectories(bool value);
protected:
  void __fastcall AddDelayedDirectoryDeletion(
    const AnsiString TempDir, int SecDelay);
  virtual void __fastcall DDDragDetect(int grfKeyState, const TPoint &DetectStart,
    const TPoint &Point, TDragDetectStatus DragStatus);
  virtual void __fastcall DDGiveFeedback(int dwEffect, HRESULT & Result);
  virtual void __fastcall DDMenuDone(TObject* Sender, HMENU AMenu);
  virtual void __fastcall DDQueryContinueDrag(BOOL FEscapePressed, int grfKeyState, HRESULT & Result);
  void __fastcall DDTargetDrop();
    virtual void __fastcall AddToDragFileList(TFileList* FileList, TListItem* Item);
  void __fastcall DisplayContextMenu(const TPoint &Where);
  void __fastcall DoChangeDirectory(TObject * Sender);
  void __fastcall DoDelayedDeletion(TObject * Sender);
  void __fastcall DoReadDirectory(TObject * Sender, bool ReloadOnly);
  virtual void __fastcall ExecuteFile(TListItem * Item);
  virtual bool __fastcall GetDirOK();
  virtual void __fastcall GetDisplayInfo(TListItem * ListItem, tagLVITEMA &DispInfo);
  virtual TDropEffectSet __fastcall GetDragSourceEffects();
  virtual bool __fastcall GetIsRoot();
  virtual AnsiString __fastcall GetPath();
  virtual AnsiString __fastcall GetPathName();
  AnsiString __fastcall GetUniqTempDir();
  void __fastcall ChangeDirectory(AnsiString Path);
  virtual void __fastcall InternalEdit(const tagLVITEMA & HItem);
  virtual TColor __fastcall ItemColor(TListItem * Item);
  virtual AnsiString __fastcall ItemDragFileName(TListItem * Item);
  virtual AnsiString __fastcall ItemFileName(TListItem * Item);
  virtual __int64 __fastcall ItemFileSize(TListItem * Item);
  virtual AnsiString __fastcall ItemFullFileName(TListItem * Item);
  virtual int __fastcall ItemImageIndex(TListItem * Item, bool Cache);
  virtual bool __fastcall ItemIsFile(TListItem * Item);
  virtual bool __fastcall ItemMatchesFilter(TListItem * Item, const TFileFilter &Filter);
  virtual Word __fastcall ItemOverlayIndexes(TListItem * Item);
  virtual void __fastcall LoadFiles();
  virtual AnsiString __fastcall MinimizePath(AnsiString Path, int Length);
  virtual void __fastcall PerformItemDragDropOperation(TListItem * Item, int Effect);
  virtual void __fastcall SetAddParentDir(bool Value);
  virtual void __fastcall SetItemImageIndex(TListItem * Item, int Index);
  virtual void __fastcall SetPath(AnsiString Value);
  virtual void __fastcall SortItems();
  virtual bool __fastcall TargetHasDropHandler(TListItem * Item, int Effect);
  virtual TDateTime __fastcall ItemFileTime(TListItem * Item);
  void __fastcall DoWarnLackOfTempSpace(const AnsiString Path, __int64 RequiredSpace,
    bool & Continue);
public:
  __fastcall TUnixDirView(TComponent* Owner);
  virtual __fastcall ~TUnixDirView();
  virtual void __fastcall CreateDirectory(AnsiString DirName);
  virtual void __fastcall DisplayPropertiesMenu();
  virtual void __fastcall ExecuteHomeDirectory();
  virtual void __fastcall ExecuteParentDirectory();
  virtual void __fastcall ExecuteRootDirectory();
  virtual void __fastcall ReloadDirectory();
  virtual bool __fastcall ItemIsDirectory(TListItem * Item);
  virtual bool __fastcall ItemIsParentDirectory(TListItem * Item);
  __property bool Active = { read = GetActive };
  __property AnsiString DDTemporaryDirectory  = { read=FDDTemporaryDirectory, write=FDDTemporaryDirectory };
#ifndef DESIGN_ONLY
  __property TTerminal *Terminal = { read = FTerminal, write = SetTerminal };
#endif
__published:
  __property bool DDAllowMove = { read = FDDAllowMove,
    write = SetDDAllowMove, default = False };
  __property int DDDeleteDelay = { read = FDDDeleteDelay,
    write = SetDDDeleteDelay, default = DefaultDDDeleteDelay };
  __property TGetCopyParamEvent OnGetCopyParam = { read = FOnGetCopyParam,
    write = FOnGetCopyParam};
  __property bool ShowInaccesibleDirectories  =
    { read=FShowInaccesibleDirectories, write=SetShowInaccesibleDirectories,
      default=true  };

  __property PathComboBox;
  __property StatusBar;
  __property PathLabel;
  __property LoadAnimation;
  __property OnGetSelectFilter;

  __property AddParentDir;
  __property DimmHiddenFiles;
  __property ShowDirectories;
  __property DirsOnTop;
  __property ShowSubDirSize;
  __property ShowHiddenFiles;
  __property SingleClickToExec;
  __property WantUseDragImages;
  __property TargetPopupMenu;
  __property OnSelectItem;
  __property OnStartLoading;
  __property OnLoaded;
  __property OnExecFile;
  __property OnDDDragEnter;
  __property OnDDDragLeave;
  __property OnDDDragOver;
  __property OnDDDrop;
  __property OnDDQueryContinueDrag;
  __property OnDDGiveFeedback;
  __property OnDDDragDetect;
  __property OnDDProcessDropped;
  __property OnDDError;
  __property OnDDExecuted;
  __property OnDDFileOperation;
  __property OnDDFileOperationExecuted;

  __property OnContextPopup;
  __property OnBeginRename;
  __property OnEndRename;

  __property ColumnClick;
  __property MultiSelect;
  __property TNotifyEvent OnDisplayProperties = { read = FOnDisplayProperties, write = FOnDisplayProperties };
  __property TWarnLackOfTempSpaceEvent OnWarnLackOfTempSpace  = { read=FOnWarnLackOfTempSpace, write=FOnWarnLackOfTempSpace };
  __property ReadOnly;
  __property HeaderImages;
};
//---------------------------------------------------------------------------
AnsiString __fastcall UniqTempDir(const AnsiString BaseDir = "");
//---------------------------------------------------------------------------
#endif
