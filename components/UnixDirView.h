//---------------------------------------------------------------------------
#ifndef UnixDirViewH
#define UnixDirViewH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include "CustomUnixDirView.hpp"
//---------------------------------------------------------------------------
class TTerminal;
class TUnixDirView;
class TCustomUnixDriveView;
class TRemoteFile;
class TRemoteProperties;
//---------------------------------------------------------------------------
enum TTransferDirection { tdToRemote, tdToLocal };
enum TTransferType { ttCopy, ttMove };
typedef void __fastcall (__closure *TDDDragFileName)
  (TObject * Sender, TRemoteFile * File, AnsiString & FileName);
//---------------------------------------------------------------------------
class PACKAGE TUnixDirView : public TCustomUnixDirView
{
friend class TCustomUnixDriveView;
private:
  bool FDDAllowMove;
  bool FDirLoadedAfterChangeDir;
  TNotifyEvent FOnDisplayProperties;
  bool FFullLoad;
  int FHiddenCount;
  int FFilteredCount;
  TTerminal *FTerminal;
  bool FShowInaccesibleDirectories;
  TDDDragFileName FOnDDDragFileName;
  bool __fastcall GetActive();
  TCustomUnixDriveView * FDriveView;
  void __fastcall SetDDAllowMove(bool value);
  void __fastcall SetTerminal(TTerminal *value);
  void __fastcall SetShowInaccesibleDirectories(bool value);
protected:
  virtual void __fastcall DDDragDetect(int grfKeyState, const TPoint &DetectStart,
    const TPoint &Point, TDragDetectStatus DragStatus);
  virtual void __fastcall DDMenuDone(TObject* Sender, HMENU AMenu);
  virtual void __fastcall DDChooseEffect(int grfKeyState, int &dwEffect);
  virtual void __fastcall AddToDragFileList(TFileList* FileList, TListItem* Item);
  void __fastcall DisplayContextMenu(const TPoint &Where);
  void __fastcall DoChangeDirectory(TObject * Sender);
  void __fastcall DoReadDirectory(TObject * Sender, bool ReloadOnly);
  void __fastcall DoStartReadDirectory(TObject * Sender);
  virtual void __fastcall ExecuteFile(TListItem * Item);
  virtual bool __fastcall GetDirOK();
  virtual void __fastcall GetDisplayInfo(TListItem * ListItem, tagLVITEMA &DispInfo);
  virtual TDropEffectSet __fastcall GetDragSourceEffects();
  virtual bool __fastcall GetIsRoot();
  virtual AnsiString __fastcall GetPath();
  virtual AnsiString __fastcall GetPathName();
  void __fastcall ChangeDirectory(AnsiString Path);
  virtual void __fastcall InternalEdit(const tagLVITEMA & HItem);
  virtual TColor __fastcall ItemColor(TListItem * Item);
  virtual AnsiString __fastcall ItemFileName(TListItem * Item);
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
  virtual TDateTime __fastcall ItemFileTime(TListItem * Item,
    TDateTimePrecision & Precision);
  DYNAMIC bool __fastcall CanEdit(TListItem* Item);
  void __fastcall SetDriveView(TCustomUnixDriveView * Value);
  virtual int __fastcall SecondaryColumnHeader(int Index, bool & AliasOnly);
  virtual int __fastcall HiddenCount();
  virtual int __fastcall FilteredCount();

  __property TCustomUnixDriveView * DriveView = { read = FDriveView, write = SetDriveView };

public:
  __fastcall TUnixDirView(TComponent* Owner);
  virtual __fastcall ~TUnixDirView();
  virtual void __fastcall CreateDirectory(AnsiString DirName);
  void __fastcall CreateDirectoryEx(AnsiString DirName, const TRemoteProperties * Properties);
  virtual void __fastcall DisplayPropertiesMenu();
  virtual void __fastcall ExecuteHomeDirectory();
  virtual void __fastcall ExecuteParentDirectory();
  virtual void __fastcall ExecuteRootDirectory();
  virtual void __fastcall ReloadDirectory();
  virtual bool __fastcall ItemIsDirectory(TListItem * Item);
  virtual bool __fastcall ItemIsParentDirectory(TListItem * Item);
  virtual AnsiString __fastcall ItemFullFileName(TListItem * Item);
  virtual __int64 __fastcall ItemFileSize(TListItem * Item);
  virtual bool __fastcall PasteFromClipBoard(AnsiString TargetPath = "");
  void __fastcall UpdateFiles();

  __property bool Active = { read = GetActive };
  __property TTerminal *Terminal = { read = FTerminal, write = SetTerminal };
__published:
  __property bool DDAllowMove = { read = FDDAllowMove,
    write = SetDDAllowMove, default = False };
  __property TDDDragFileName OnDDDragFileName = { read = FOnDDDragFileName,
    write = FOnDDDragFileName};
  __property bool ShowInaccesibleDirectories  =
    { read=FShowInaccesibleDirectories, write=SetShowInaccesibleDirectories,
      default=true  };

  __property OnUpdateStatusBar;
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
  __property OnMatchMask;
  __property OnGetOverlay;
  __property OnDDDragEnter;
  __property OnDDDragLeave;
  __property OnDDDragOver;
  __property OnDDDrop;
  __property OnDDQueryContinueDrag;
  __property OnDDGiveFeedback;
  __property OnDDChooseEffect;
  __property OnDDDragDetect;
  __property OnDDEnd;
  __property OnDDCreateDragFileList;
  __property OnDDTargetHasDropHandler;
  __property OnDDProcessDropped;
  __property OnDDError;
  __property OnDDExecuted;
  __property OnDDFileOperation;
  __property OnDDFileOperationExecuted;
  __property OnDDCreateDataObject;
  __property OnDDMenuPopup;

  __property OnContextPopup;
  __property OnBeginRename;
  __property OnEndRename;
  __property OnHistoryChange;
  __property OnHistoryGo;
  __property OnPathChange;

  __property ColumnClick;
  __property MultiSelect;
  __property TNotifyEvent OnDisplayProperties = { read = FOnDisplayProperties, write = FOnDisplayProperties };
  __property ReadOnly;
  __property HeaderImages;
};
//---------------------------------------------------------------------------
#endif
