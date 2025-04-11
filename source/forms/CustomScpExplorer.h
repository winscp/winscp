//---------------------------------------------------------------------------
#ifndef CustomScpExplorerH
#define CustomScpExplorerH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <CustomDirView.hpp>
#include <CustomUnixDirView.hpp>
#include <IEListView.hpp>
#include <NortonLikeListView.hpp>
#include <UnixDirView.h>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ToolWin.hpp>

#include <WinInterface.h>
#include <WinConfiguration.h>
#include <Terminal.h>
#include <Queue.h>
#include "QueueController.h"
#include "UnixDriveView.h"
#include "CustomDriveView.hpp"
#include "TBX.hpp"
#include "TB2Dock.hpp"
#include "TBXExtItems.hpp"
#include "TBXStatusBars.hpp"
#include "TB2Item.hpp"
#include "TB2Toolbar.hpp"
#include "TBXToolPals.hpp"
#include "PngImageList.hpp"
#include "ThemePageControl.h"
#include "PathLabel.hpp"
#include <Vcl.AppEvnts.hpp>
#include <GUITools.h>
//---------------------------------------------------------------------------
class TProgressForm;
class TSynchronizeProgressForm;
class TTerminalQueue;
class TTerminalQueueStatus;
class TQueueItem;
class TQueueItemProxy;
class TQueueController;
class TSynchronizeController;
class TEditorManager;
class TEditorData;
class TTransferPresetNoteData;
struct TEditedFileData;
class ITaskbarList3;
struct TSynchronizeParams;
class TBookmark;
class TManagedTerminal;
class TCalculateSizeOperation;
class TThumbnailDownloadQueueItem;
//---------------------------------------------------------------------------
enum TActionAllowed { aaShortCut, aaUpdate, aaExecute };
enum TActionFlag { afLocal = 1, afRemote = 2, afExplorer = 4, afCommander = 8 };
enum TExecuteFileBy { efShell = 1, efInternalEditor = 2, efExternalEditor = 3, efDefaultEditor = 100 };
enum TPanelExport { pePath, peFileList, peFullFileList };
enum TPanelExportDestination { pedClipboard, pedCommandLine };
enum TCopyOperationCommandFlag {
  cocNone = 0x00, cocShortCutHint = 0x01, cocQueue = 0x02, cocNonQueue = 0x04
};
enum TCustomCommandListType { ccltAll, ccltBoth, ccltNonFile, ccltFile };
const TOperationSide osOther = osRemote;
//---------------------------------------------------------------------------
class TCustomScpExplorerForm : public TForm
{
friend class TAutoBatch;
friend class TEditorUploadQueueItem;
__published:
  TPanel *RemotePanel;
  TTBXStatusBar *RemoteStatusBar;
  TUnixDirView *RemoteDirView;
  TPanel *RemoteDirPanel;
  TTBXDock *TopDock;
  TListView *QueueView3;
  TPanel *QueuePanel;
  TSplitter *QueueSplitter;
  TTBXToolbar *QueueToolbar;
  TTBXDock *QueueDock;
  TTBXItem *QueueEnableItem;
  TTBXSeparatorItem *TBXSeparatorItem203;
  TTBXItem *TBXItem201;
  TTBXItem *TBXItem202;
  TTBXItem *TBXItem203;
  TTBXItem *TBXItem204;
  TTBXItem *TBXItem205;
  TTBXSeparatorItem *TBXSeparatorItem201;
  TTBXItem *TBXItem206;
  TTBXItem *TBXItem207;
  TTBXSeparatorItem *TBXSeparatorItem202;
  TTBXItem *TBXItem208;
  TUnixDriveView *RemoteDriveView;
  TPanel *RemoteDrivePanel;
  TSplitter *RemotePanelSplitter;
  TTBXItem *TBXItem194;
  TTBXItem *TBXItem195;
  TTBXSubmenuItem *TBXSubmenuItem27;
  TTBXItem *TBXItem211;
  TTBXItem *TBXItem225;
  TTBXItem *TBXItem226;
  TThemeTabSheet *TabSheet1;
  TThemePageControl *SessionsPageControl;
  TPathLabel *QueueLabel;
  TTBXSeparatorItem *TBXSeparatorItem57;
  TTBXItem *QueueDeleteAllDoneQueueToolbarItem;
  TTBXItem *TBXItem173;
  TApplicationEvents *ApplicationEvents;
  TTBXToolbar *ReconnectToolbar;
  TTBXItem *TBXItem254;
  TSplitter *QueueFileListSplitter;
  TListView *QueueFileList;
  TTBXDock *MessageDock;
  void __fastcall ApplicationMinimize(TObject * Sender);
  void __fastcall ApplicationRestore(TObject * Sender);
  void __fastcall RemoteDirViewContextPopup(TObject *Sender,
    const TPoint &MousePos, bool &Handled);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall RemoteDirViewDisplayProperties(TObject *Sender);
  void __fastcall DirViewColumnRightClick(TObject *Sender,
    TListColumn *Column, TPoint &Point);
  void __fastcall DirViewExecFile(TObject *Sender, TListItem *Item, bool &AllowExec);
  void __fastcall ToolBarResize(TObject *Sender);
  void __fastcall FileControlDDDragEnter(TObject *Sender,
    _di_IDataObject DataObj, int grfKeyState, const TPoint &Point,
    int &dwEffect, bool &Accept);
  void __fastcall FileControlDDDragLeave(TObject *Sender);
  void __fastcall RemoteFileControlDDCreateDragFileList(TObject *Sender,
    TFileList *FileList, bool &Created);
  void __fastcall RemoteFileControlDDEnd(TObject *Sender);
  void __fastcall RemoteFileControlDDCreateDataObject(TObject *Sender,
    TDataObject *&DataObject);
  void __fastcall RemoteFileControlDDGiveFeedback(TObject *Sender,
    int dwEffect, HRESULT &Result);
  void __fastcall QueueSplitterCanResize(TObject *Sender, int &NewSize,
    bool &Accept);
  void __fastcall QueueView3ContextPopup(TObject *Sender, TPoint &MousePos,
    bool &Handled);
  void __fastcall QueueView3Deletion(TObject *Sender, TListItem *Item);
  void __fastcall QueueView3StartDrag(TObject *Sender,
    TDragObject *&DragObject);
  void __fastcall QueueView3DragOver(TObject *Sender, TObject *Source,
    int X, int Y, TDragState State, bool &Accept);
  void __fastcall QueueView3DragDrop(TObject *Sender, TObject *Source,
    int X, int Y);
  void __fastcall QueueView3Enter(TObject *Sender);
  void __fastcall QueueView3SelectItem(TObject *Sender, TListItem *Item,
    bool Selected);
  void __fastcall RemoteFileControlDDFileOperation(
    TObject * Sender, int Effect, UnicodeString SourcePath, UnicodeString TargetPath,
    bool Paste, bool & DoOperation);
  void __fastcall RemoteFileContolDDChooseEffect(TObject * Sender,
    int grfKeyState, int & dwEffect);
  void __fastcall RemoteFileControlDDDragFileName(TObject * Sender,
    TRemoteFile * File, UnicodeString & FileName);
  void __fastcall RemoteFileControlDDDragDetect(TObject * Sender,
    int grfKeyState, const TPoint & DetectStart, const TPoint & Point,
    TDragDetectStatus DragStatus);
  void __fastcall RemoteFileControlDDQueryContinueDrag(TObject *Sender,
          LongBool FEscapePressed, int grfKeyState, HRESULT &Result);
  void __fastcall RemoteDirViewEnter(TObject *Sender);
  void __fastcall RemoteDriveViewEnter(TObject *Sender);
  void __fastcall DirViewMatchMask(TObject *Sender, UnicodeString FileName,
    bool Directory, __int64 Size, TDateTime Modification,
    UnicodeString Masks, bool &Matches, bool AllowImplicitMatches);
  void __fastcall DirViewGetOverlay(TObject *Sender, TListItem *Item,
    WORD &Indexes);
  void __fastcall DirViewHistoryChange(TCustomDirView *Sender);
  void __fastcall RemoteStatusBarClick(TObject *Sender);
  void __fastcall DirViewLoaded(TObject *Sender);
  void __fastcall ToolbarGetBaseSize(TTBCustomToolbar * Toolbar, TPoint & ASize);
  void __fastcall FormConstrainedResize(TObject * Sender, int & MinWidth,
    int  &MinHeight, int  &MaxWidth, int  &MaxHeight);
  void __fastcall StatusBarPanelDblClick(TTBXCustomStatusBar * Sender,
    TTBXStatusPanel * Panel);
  void __fastcall RemotePathComboBoxAdjustImageIndex(
    TTBXComboBoxItem * Sender, const UnicodeString AText, int AIndex,
    int & ImageIndex);
  void __fastcall RemotePathComboBoxDrawItem(TTBXCustomList * Sender,
    TCanvas * ACanvas, TRect & ARect, int AIndex, int AHoverIndex,
    bool & DrawDefault);
  void __fastcall RemotePathComboBoxMeasureWidth(TTBXCustomList * Sender,
    TCanvas * ACanvas, int AIndex, int & AWidth);
  void __fastcall RemotePathComboBoxItemClick(TObject * Sender);
  void __fastcall RemotePathComboBoxCancel(TObject * Sender);
  void __fastcall DirViewEditing(TObject *Sender, TListItem *Item,
          bool &AllowEdit);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall SessionsPageControlChange(TObject *Sender);
  void __fastcall SessionsPageControlMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
  void __fastcall RemoteDirViewRead(TObject *Sender);
  void __fastcall DirViewSelectItem(TObject *Sender, TListItem *Item, bool Selected);
  void __fastcall SessionsPageControlDragDrop(TObject *Sender, TObject *Source, int X,
    int Y);
  void __fastcall SessionsPageControlDragOver(TObject *Sender, TObject *Source, int X,
    int Y, TDragState State, bool &Accept);
  void __fastcall QueueView3Exit(TObject *Sender);
  void __fastcall EditMenuItemPopup(TTBCustomItem *Sender, bool FromLink);
  void __fastcall DirViewBusy(TObject *Sender, int Busy, bool & Allow);
  void __fastcall SessionsPageControlContextPopup(TObject *Sender, TPoint &MousePos, bool &Handled);
  void __fastcall DockContextPopup(TObject *Sender, TPoint &MousePos, bool &Handled);
  void __fastcall SessionsPageControlTabButtonClick(TPageControl *Sender, int Index);
  void __fastcall DirViewGetItemColor(
    TObject * Sender, UnicodeString FileName, bool Directory, __int64 Size, TDateTime Modification, TColor & Color);
  void __fastcall DirViewExit(TObject *Sender);
  void __fastcall DirViewKeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
  void __fastcall DirViewKeyPress(TObject *Sender, System::WideChar &Key);
  void __fastcall ApplicationEventsDeactivate(TObject *Sender);
  void __fastcall ApplicationEventsModalBegin(TObject *Sender);
  void __fastcall DirViewChangeFocus(TObject *Sender, TListItem *Item);
  void __fastcall RemoteStatusBarMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);
  void __fastcall RemoteDirViewResize(TObject *Sender);
  void __fastcall QueueFileListSplitterCanResize(TObject *Sender, int &NewSize, bool &Accept);
  void __fastcall QueueView3Change(TObject *Sender, TListItem *Item, TItemChange Change);
  void __fastcall QueueLabelGetStatus(TCustomPathLabel *Sender, bool &Active);
  void __fastcall QueueFileListEnterExit(TObject *Sender);
  void __fastcall QueueFileListData(TObject *Sender, TListItem *Item);
  void __fastcall QueueFileListCustomDrawItem(TCustomListView *Sender, TListItem *Item, TCustomDrawState State, bool &DefaultDraw);
  void __fastcall QueueFileListResize(TObject *Sender);
  void __fastcall SessionsPageControlResize(TObject *Sender);
  void __fastcall SessionsPageControlTabHint(TPageControl *Sender, int Index, UnicodeString &Hint);
  void __fastcall MessageDockRequestDock(TObject *Sender, TTBCustomDockableWindow *Bar, bool &Accept);
  void __fastcall QueueView3EndDrag(TObject *Sender, TObject *Target, int X, int Y);
  void __fastcall RemoteDirViewThumbnailNeeded(
    TUnixDirView * Sender, TListItem * Item, TRemoteFile * File, const TSize & Size, TBitmap *& Bitmap);
  void __fastcall RemoteDirViewStartLoading(TObject *Sender);
  void __fastcall RemoteDirViewStartReading(TObject *Sender);
  void __fastcall FormAfterMonitorDpiChanged(TObject *Sender, int OldDPI, int NewDPI);

private:
  TManagedTerminal * FManagedSession;
  TTerminalQueue * FQueue;
  TTerminalQueueStatus * FQueueStatus;
  TCriticalSection * FQueueStatusSection;
  bool FQueueStatusInvalidated;
  bool FQueueItemInvalidated;
  bool FQueueStatusUpdating;
  bool FFormRestored;
  bool FAutoOperation;
  TFileOperationFinishedEvent FOnFileOperationFinished;
  TFileOperation FPrimaryOperation;
  bool FForceExecution;
  unsigned short FIgnoreNextDialogChar;
  TStringList * FErrorList;
  HANDLE FDDExtMutex;
  UnicodeString FDragFakeDirectory;
  TObjectList * FDragFakeMonitors;
  UnicodeString FClipboardFakeDirectory;
  UnicodeString FClipboardPasteTarget;
  std::unique_ptr<TObjectList> FClipboardFakeMonitors;
  bool FDownloadingFromClipboard;
  bool FClipboardFakeMonitorsPendingReset;
  std::unique_ptr<TDragDropFilesEx> FClipboardDragDropFilesEx;
  TManagedTerminal * FClipboardTerminal;
  std::unique_ptr<TStrings> FClipboardFileList;
  TStrings * FDelayedDeletionList;
  TTimer * FDelayedDeletionTimer;
  TStrings * FDDFileList;
  __int64 FDDTotalSize;
  std::unique_ptr<ExtException> FDragDropSshTerminate;
  HINSTANCE FOle32Library;
  HCURSOR FDragMoveCursor;
  UnicodeString FDragTempDir;
  bool FRefreshLocalDirectory;
  bool FRefreshRemoteDirectory;
  TListItem * FQueueActedItem;
  TQueueController * FQueueController;
  TListViewScrollOnDragOver * FQueueScrollOnDragOver;
  int FLastDropEffect;
  bool FPendingTempSpaceWarn;
  TEditorManager * FEditorManager;
  TList * FLocalEditors;
  TStrings * FCapturedLog;
  bool FDragDropOperation;
  UnicodeString FCopyParamDefault;
  UnicodeString FCopyParamAutoSelected;
  bool FEditingFocusedAdHocCommand;
  TList * FDocks;
  TSynchronizeController * FSynchronizeController;
  UnicodeString FTransferDropDownHint;
  int FTransferListHoverIndex;
  TColor FSessionColor;
  TPngImageList * FSessionColors;
  int FNewRemoteTabTabImageIndex;
  int FNewLocalTabTabImageIndex;
  int FSessionTabImageIndex;
  int FSessionColorMaskImageIndex;
  int FLocalBrowserTabImageIndex;
  ::TTrayIcon * FTrayIcon;
  TCustomCommandType FLastCustomCommand;
  TFileMasks FDirViewMatchMask;
  TTBXPopupMenu * FCustomCommandMenu;
  TStrings * FCustomCommandLocalFileList;
  TStrings * FCustomCommandRemoteFileList;
  ITaskbarList3 * FTaskbarList;
  bool FShowing;
  int FMaxQueueLength;
  TDateTime FSessionsPageControlNewTabTime;
  bool FAppIdle;
  typedef std::set<TTBCustomItem *> TItemsWithTextDisplayMode;
  TItemsWithTextDisplayMode FItemsWithTextDisplayMode;
  HWND FHiddenWindow;
  TStrings * FTransferResumeList;
  TStrings * FDeletedFiles;
  bool FMoveToQueue;
  bool FStandaloneOperation;
  TFeedSynchronizeError FOnFeedSynchronizeError;
  TNotifyEvent FOnSynchronizeAbort;
  TTerminal * FSynchronizeTerminal;
  bool FNeedSession;
  TTerminal * FFileFindTerminal;
  UnicodeString FFileColorsCurrent;
  bool FInvalid;
  std::unique_ptr<TQueueFileList> FQueueFileList;
  bool FShowingChanged;
  bool FStarted;
  bool FUpdatingSessionTabs;
  TCalculateSizeOperation * FCalculateSizeOperation;
  TTBXTheme * FHiContrastTheme;

  bool __fastcall GetEnableFocusedOperation(TOperationSide Side, int FilesOnly);
  bool __fastcall GetEnableSelectedOperation(TOperationSide Side, int FilesOnly);
  void __fastcall SetManagedSession(TManagedTerminal * value);
  void __fastcall DoSetManagedSession(TManagedTerminal * value, bool Replace);
  TManagedTerminal * GetTerminal();
  void __fastcall SetQueue(TTerminalQueue * value);
  void __fastcall TransferListChange(TObject * Sender);
  void __fastcall TransferListDrawItem(TTBXCustomList * Sender, TCanvas * ACanvas,
    const TRect & ARect, int AIndex, int AHoverIndex, bool & DrawDefault);
  void __fastcall CloseInternalEditor(TObject * Sender);
  void __fastcall ForceCloseInternalEditor(TObject * Sender);
  void __fastcall ForceCloseLocalEditors();
  void __fastcall TerminalCaptureLog(const UnicodeString & AddedLine, TCaptureOutputType OutputType);
  void __fastcall HistoryItemClick(System::TObject* Sender);
  void __fastcall UpdateHistoryMenu(TOperationSide Side, bool Back);
  void __fastcall AdHocCustomCommandValidate(const TCustomCommandType & Command);
  void __fastcall SetDockAllowDrag(bool value);
  void __fastcall QueueSplitterDblClick(TObject * Sender);
  void __fastcall QueueFileListSplitterDblClick(TObject * Sender);
  void __fastcall AddQueueItem(
    TTerminalQueue * Queue, TTransferDirection Direction,
    TStrings * FileList, const UnicodeString TargetDirectory,
    TGUICopyParamType & CopyParam, int Params);
  void __fastcall AddQueueItem(TTerminalQueue * Queue, TQueueItem * QueueItem, TManagedTerminal * Terminal);
  void __fastcall ClearTransferSourceSelection(TTransferDirection Direction);
  void ClearOperationSelection(TOperationSide Side);
  void __fastcall SessionsDDDragOver(int KeyState, const TPoint & Point, int & Effect, int PreferredEffect);
  void __fastcall SessionsDDProcessDropped(TObject * Sender, int KeyState, const TPoint & Point, int Effect);
  void __fastcall RemoteFileControlDragDropFileOperation(
    TObject * Sender, int Effect, UnicodeString TargetPath, bool ForceQueue, bool Paste);
  void __fastcall SessionsDDDragEnter(_di_IDataObject DataObj, int KeyState,
    const TPoint & Point, int & Effect, bool & Accept);
  void __fastcall SessionsDDDragLeave(int);
  void __fastcall QueueDDProcessDropped(TObject * Sender, int KeyState, const TPoint & Point, int Effect);
  void __fastcall QueueDDDragEnter(_di_IDataObject DataObj, int KeyState,
    const TPoint & Point, int & Effect, bool & Accept);
  void __fastcall QueueDDDragLeave(int);
  void __fastcall EnableDDTransferConfirmation(TObject * Sender);
  void __fastcall CollectItemsWithTextDisplayMode(TWinControl * Control);
  void __fastcall CreateHiddenWindow();
  bool __fastcall IsQueueAutoPopup();
  void __fastcall UpdateSessionsPageControlHeight();
  void UpdateTabsSize();
  TDragDropFilesEx * __fastcall CreateDragDropFilesEx();
  void __fastcall KeyProcessed(Word & Key, TShiftState Shift);
  void __fastcall CheckCustomCommandShortCut(TCustomCommandList * List, Word & Key, Classes::TShiftState Shift, TShortCut KeyShortCut);
  void __fastcall CMShowingChanged(TMessage & Message);
  void __fastcall WMClose(TMessage & Message);
  void __fastcall WMDpiChanged(TMessage & Message);
  void __fastcall DoBookmarkClick(TOperationSide Side, TObject * Sender);
  void __fastcall LocalBookmarkClick(TObject * Sender);
  void __fastcall RemoteBookmarkClick(TObject * Sender);
  void __fastcall InitControls();
  void __fastcall UpdateQueueFileList();
  void __fastcall QueueFileListColumnAutoSize();
  void __fastcall AdjustQueueLayout();
  void __fastcall StoreTransitionCloseClick(TObject * Sender);
  void __fastcall StoreTransitionLinkClick(TObject * Sender);
  void InitializeRemoteThumbnailMask();

protected:
  TOperationSide FCurrentSide;
  bool FEverShown;
  TControl * FDDTargetControl;
  TProgressForm * FProgressForm;
  TSynchronizeProgressForm * FSynchronizeProgressForm;
  HANDLE FDDExtMapFile;
  TTimer * FUserActionTimer;
  TQueueItemProxy * FPendingQueueActionItem;
  TTBXPopupMenu * FHistoryMenu[2][2];
  bool FAllowTransferPresetAutoSelect;
  bool FSessionChanging;
  TStrings * FNotes;
  TTimer * FNoteTimer;
  TDateTime FNoteShown;
  UnicodeString FNote;
  TObject * FNoteData;
  UnicodeString FNoteHints;
  TNotifyEvent FOnNoteClick;
  unsigned int FLockLevel;
  unsigned int FLockSuspendLevel;
  bool FDisabledOnLockSuspend;
  bool FAlternativeDelete;
  TDragDropFilesEx * FSessionsDragDropFilesEx;
  TDragDropFilesEx * FQueueDragDropFilesEx;
  TPoint FLastContextPopupScreenPoint;
  int FDoNotIdleCurrentTerminal;
  UnicodeString FFakeFileDropTarget;
  TFileColorData::TList FFileColors;
  TIncrementalSearchState FIncrementalSearchState;
  int FIncrementalSearching;
  TOperationSide FProgressSide;
  bool FImmersiveDarkMode;
  TFileMasks FRemoteThumbnailMask;

  virtual bool __fastcall CopyParamDialog(TTransferDirection Direction,
    TTransferType Type, bool Temp, TStrings * FileList,
    UnicodeString & TargetDirectory, TGUICopyParamType & CopyParam, bool Confirm,
    bool DragDrop, int Options);
  virtual void __fastcall CopyParamDialogAfter(TTransferDirection Direction, bool Temp, const UnicodeString & TargetDirectory);
  virtual bool __fastcall RemoteTransferDialog(TManagedTerminal *& Session,
    TStrings * FileList, UnicodeString & Target, UnicodeString & FileMask, bool & DirectCopy,
    bool NoConfirmation, bool Move);
  virtual void __fastcall Loaded();
  void __fastcall DeleteFiles(TOperationSide Side, TStrings * FileList, bool Alternative);
  bool __fastcall RemoteTransferFiles(TStrings * FileList, bool NoConfirmation,
    bool Move, TManagedTerminal * Session);
  virtual void __fastcall DoDirViewExecFile(TObject * Sender, TListItem * Item, bool & AllowExec);
  virtual TControl * __fastcall GetComponent(Byte Component);
  bool __fastcall GetComponentVisible(Byte Component);
  virtual Boolean __fastcall GetHasDirView(TOperationSide Side);
  virtual TCustomDriveView * __fastcall DriveView(TOperationSide Side);
  virtual TCustomDirView * GetCurrentLocalBrowser();
  DYNAMIC void __fastcall KeyDown(Word & Key, Classes::TShiftState Shift);
  virtual void __fastcall RestoreFormParams();
  virtual void __fastcall RestoreParams();
  virtual void __fastcall SetComponentVisible(Byte Component, bool value);
  virtual void __fastcall ComponentShowing(Byte Component, bool value);
  virtual void __fastcall FixControlsPlacement();
  bool __fastcall SetProperties(TOperationSide Side, TStrings * FileList);
  void __fastcall CustomCommand(TStrings * FileList,
    const TCustomCommandType & Command, TStrings * ALocalFileList);
  void __fastcall RemoteCustomCommand(
    TStrings * FileList, const TCustomCommandType & ACommand,
    const TCustomCommandData & Data, const UnicodeString & CommandCommand);
  void __fastcall LocalCustomCommandPure(
    TStrings * FileList, const TCustomCommandType & ACommand, const UnicodeString & Command, TStrings * ALocalFileList,
    const TCustomCommandData & Data, bool LocalFileCommand, bool FileListCommand, UnicodeString * POutput);
  void __fastcall LocalCustomCommandWithLocalFiles(
    const TCustomCommandType & ACommand, const UnicodeString & Command, const TCustomCommandData & Data,
    bool FileListCommand, UnicodeString * POutput);
  void __fastcall LocalCustomCommand(TStrings * FileList,
    const TCustomCommandType & ACommand, TStrings * ALocalFileList,
    const TCustomCommandData & Data, const UnicodeString & CommandCommand);
  virtual void __fastcall SessionChanging();
  virtual void __fastcall SessionChanged(bool Replaced);
  virtual void __fastcall QueueChanged();
  void __fastcall InitStatusBar();
  void __fastcall UpdateStatusBar();
  virtual void __fastcall UpdateStatusPanelText(TTBXStatusPanel * Panel);
  virtual void __fastcall DoOperationFinished(TFileOperation Operation,
    TOperationSide Side, bool Temp, const UnicodeString & FileName, bool Success, bool NotCancelled,
    TOnceDoneOperation & OnceDoneOperation);
  virtual void __fastcall DoOpenDirectoryDialog(TOpenDirectoryMode Mode, TOperationSide Side);
  void __fastcall CreateProgressForm(TSynchronizeProgress * SynchronizeProgress);
  void __fastcall DestroyProgressForm();
  virtual void __fastcall FileOperationProgress(TFileOperationProgressType & ProgressData);
  void __fastcall OperationComplete(const TDateTime & StartTime);
  void EditedFileUploaded(TTerminal * ATerminal, HANDLE UploadCompleteEvent);
  void __fastcall ExecutedFileChanged(
    const UnicodeString & FileName, TEditedFileData * Data, HANDLE UploadCompleteEvent, bool & Retry);
  void __fastcall ExecutedFileReload(const UnicodeString & FileName, TEditedFileData * Data);
  void __fastcall ExecutedFileEarlyClosed(const TEditedFileData * Data,
    bool & KeepOpen);
  void __fastcall ExecutedFileUploadComplete(TObject * Sender);
  bool EditorCheckNotModified(const TEditedFileData * Data);
  void __fastcall CMDialogChar(TMessage & AMessage);
  inline void __fastcall WMAppCommand(TMessage & Message);
  inline void __fastcall WMSysCommand(TMessage & Message);
  void __fastcall WMQueryEndSession(TMessage & Message);
  void __fastcall WMEndSession(TWMEndSession & Message);
#ifdef _DEBUG
  inline void __fastcall WMWindowPosChanged(TWMWindowPosMsg & Message);
#endif
  void __fastcall WMCopyData(TMessage & Message);
  virtual void __fastcall SysResizing(unsigned int Cmd);
  DYNAMIC void __fastcall DoShow();
  TStrings * __fastcall CreateVisitedDirectories(TOperationSide Side);
  void __fastcall HandleErrorList(TStringList *& ErrorList);
  void __fastcall TerminalSynchronizeDirectory(
    const UnicodeString & LocalDirectory, const UnicodeString & RemoteDirectory,
    bool & Continue, bool Collect, const TSynchronizeOptions * Options);
  void __fastcall DoSynchronize(TSynchronizeController * Sender,
    const UnicodeString LocalDirectory, const UnicodeString RemoteDirectory,
    const TCopyParamType & CopyParam, const TSynchronizeParamType & Params,
    TSynchronizeChecklist ** Checklist, TSynchronizeOptions * Options, bool Full);
  void __fastcall DoSynchronizeInvalid(TSynchronizeController * Sender,
    const UnicodeString Directory, const UnicodeString ErrorStr);
  void __fastcall DoSynchronizeTooManyDirectories(TSynchronizeController * Sender,
    int & MaxDirectories);
  void __fastcall Synchronize(const UnicodeString LocalDirectory,
    const UnicodeString RemoteDirectory, TSynchronizeMode Mode,
    const TCopyParamType & CopyParam, int Params, TSynchronizeChecklist ** Checklist,
    TSynchronizeOptions * Options);
  void __fastcall SynchronizeSessionLog(const UnicodeString & Message);
  void __fastcall GetSynchronizeOptions(int Params, TSynchronizeOptions & Options);
  UnicodeString __fastcall SerializeCopyParamForCommandLine(const TCopyParamType * CopyParams);
  void __fastcall SynchronizeInNewWindow(const TSynchronizeParamType & Params, const TCopyParamType * CopyParams);
  void __fastcall FullSynchronizeInNewWindow(
    TSynchronizeMode Mode, int Params, const UnicodeString & LocalDirectory, const UnicodeString & RemoteDirectory,
     const TCopyParamType * CopyParams);
  bool __fastcall SynchronizeAllowSelectedOnly();
  virtual void __fastcall BatchStart(void *& Storage);
  virtual void __fastcall BatchEnd(void * Storage);
  bool __fastcall ExecuteFileOperation(TFileOperation Operation, TOperationSide Side,
    TStrings * FileList, bool NoConfirmation, void * Param);
  bool __fastcall ExecuteCopyMoveFileOperation(
    TFileOperation Operation, TOperationSide Side, TStrings * FileList, bool NoConfirmation, void * Param);
  bool __fastcall ExecuteDeleteFileOperation(TOperationSide Side, TStrings * FileList, void * Param);
  virtual bool __fastcall DDGetTarget(UnicodeString & Directory,
    bool & ForceQueue, UnicodeString & CounterName);
  virtual void __fastcall DDFakeFileInitDrag(TFileList * FileList, bool & Created);
  UnicodeString __fastcall CreateFakeTransferDirectory();
  void __fastcall DDFakeCreated(TObject * Sender, const UnicodeString FileName);
  void __fastcall ClipboardFakeCreated(TObject * Sender, const UnicodeString FileName);
  void __fastcall ClipboardDataObjectRelease(TObject * Sender);
  virtual void __fastcall SideEnter(TOperationSide Side);
  virtual TOperationSide __fastcall GetSide(TOperationSide Side);
  TStrings * __fastcall PanelExport(TOperationSide Side, TPanelExport Export);
  virtual void __fastcall PanelExportStore(TOperationSide Side,
    TPanelExport Export, TPanelExportDestination Destination,
    TStrings * ExportData);
  void __fastcall GenerateUrl(TStrings * Paths);
  void __fastcall QueueListUpdate(TTerminalQueue * Queue);
  void __fastcall QueueItemUpdate(TTerminalQueue * Queue, TQueueItem * Item);
  void __fastcall UpdateQueueStatus(bool QueueChanging);
  bool IsAnythingQueued();
  void __fastcall RefreshQueueItems();
  virtual int __fastcall GetStaticComponentsHeight();
  void __fastcall FillQueueViewItem(TListItem * Item,
    TQueueItemProxy * QueueItem, bool Detail);
  void __fastcall QueueViewDeleteItem(int Index);
  void __fastcall UserActionTimer(TObject * Sender);
  void __fastcall UpdateQueueView();
  bool __fastcall CanCloseQueue(TTerminalQueue * Queue);
  bool __fastcall CanCloseQueue();
  virtual bool __fastcall IsFileControl(TObject * Control, TOperationSide Side);
  virtual void __fastcall ReloadLocalDirectory(const UnicodeString Directory = L"");
  virtual bool __fastcall PanelOperation(TOperationSide Side, bool DragDrop);
  void __fastcall DoWarnLackOfTempSpace(const UnicodeString Path,
    __int64 RequiredSpace, bool & Continue);
  void __fastcall AddDelayedDirectoryDeletion(const UnicodeString TempDir, int SecDelay);
  void __fastcall DoDelayedDeletion(TObject * Sender);
  TDragDropFilesEx * __fastcall DragDropFiles(TObject * Sender);
  void __fastcall RemoteFileControlDDTargetDrop();
  bool __fastcall RemoteFileControlFileOperation(TObject * Sender,
    TFileOperation Operation, bool NoConfirmation, void * Param);
  void __fastcall DDDownload(
    TStrings * FilesToCopy, const UnicodeString & TargetDir, TCopyParamType * CopyParam, int Params);
  bool __fastcall EnsureCommandSessionFallback(TFSCapability Capability);
  bool __fastcall CommandSessionFallback();
  void __fastcall FileTerminalRemoved(const UnicodeString FileName,
    TEditedFileData * Data, TObject * Token, void * Arg);
  void __fastcall FileTerminalReplaced(
    const UnicodeString FileName, TEditedFileData * Data, TObject * Token, void * Arg);
  void __fastcall FileConfigurationChanged(const UnicodeString FileName,
    TEditedFileData * Data, TObject * Token, void * Arg);
  void __fastcall CustomExecuteFile(TOperationSide Side,
    TExecuteFileBy ExecuteFileBy, UnicodeString FileName, UnicodeString OriginalFileName,
    const TEditorData * ExternalEditor, UnicodeString LocalRootDirectory,
    UnicodeString RemoteDirectory, bool NewFile, const TDateTime & SourceTimestamp);
  void __fastcall ExecuteFile(TOperationSide Side,
    TExecuteFileBy ExecuteFileBy, const TEditorData * ExternalEditor,
    UnicodeString FullFileName, TObject * Object,
    const TFileMasks::TParams & MaskParams);
  bool __fastcall RemoteExecuteForceText(TExecuteFileBy ExecuteFileBy,
    const TEditorData * ExternalEditor);
  void __fastcall ExecuteFileNormalize(TExecuteFileBy & ExecuteFileBy,
    const TEditorData *& ExternalEditor, const UnicodeString & FileName,
    bool Local, const TFileMasks::TParams & MaskParams);
  void __fastcall ExecuteRemoteFile(
    const UnicodeString & FullFileName, TRemoteFile * File, TExecuteFileBy ExecuteFileBy);
  void __fastcall TemporaryFileCopyParam(TCopyParamType & CopyParam);
  void __fastcall TemporaryDirectoryForRemoteFiles(
    const UnicodeString & RemoteDirectory, const TCopyParamType & CopyParam, bool Simple,
    UnicodeString & Result, UnicodeString & RootDirectory);
  void __fastcall TemporarilyDownloadFiles(TStrings * FileList, bool ForceText,
    UnicodeString & RootTempDir, UnicodeString & TempDir, bool GetTargetNames,
    bool AutoOperation, bool SimpleTempDir);
  void __fastcall LocalEditorClosed(TObject * Sender, bool Forced);
  TTBXPopupMenu * __fastcall HistoryMenu(TOperationSide Side, bool Back);
  UnicodeString __fastcall FileStatusBarText(const TStatusFileInfo & FileInfo, TOperationSide Side);
  void __fastcall UpdateFileStatusBar(TTBXStatusBar * StatusBar,
    const TStatusFileInfo & FileInfo, TOperationSide Side);
  void __fastcall UpdateFileStatusExtendedPanels(
    TTBXStatusBar * StatusBar, const TStatusFileInfo & FileInfo);
  void __fastcall FileStatusBarPanelClick(TTBXStatusPanel * Panel, TOperationSide Side);
  virtual void __fastcall DoDirViewLoaded(TCustomDirView * Sender);
  virtual void UpdatePanelControls(TCustomDirView * ADirView, TCustomDriveView * ADriveView);
  void __fastcall UpdateTransferList();
  void __fastcall UpdateTransferLabel();
  void __fastcall StartUpdates();
  void __fastcall TransferPresetAutoSelect();
  virtual void __fastcall GetTransferPresetAutoSelectData(TCopyParamRuleData & Data);
  inline bool __fastcall CustomCommandRemoteAllowed();
  void __fastcall CustomCommandMenu(
    TAction * Action, TStrings * LocalFileList, TStrings * RemoteFileList);
  void __fastcall LoadToolbarsLayoutStr(UnicodeString LayoutStr, UnicodeString ButtonsStr);
  UnicodeString __fastcall GetToolbarItemName(TTBCustomItem * Item);
  UnicodeString __fastcall GetToolbarsLayoutStr();
  UnicodeString __fastcall GetToolbarsButtonsStr();
  virtual void __fastcall Dispatch(void * Message);
  void __fastcall PostComponentHide(Byte Component);
  void __fastcall GetSpaceAvailable(const UnicodeString Path,
    TSpaceAvailable & ASpaceAvailable, bool & Close);
  void __fastcall CalculateSize(TStrings * FileList, __int64 & Size,
    TCalculateSizeStats & Stats, bool & Close);
  void __fastcall CalculateChecksum(const UnicodeString & Alg, TStrings * FileList,
    TCalculatedChecksumEvent OnCalculatedChecksum, bool & Close);
  void __fastcall UpdateCustomCommandsToolbar();
  virtual void __fastcall UpdateActions();
  void __fastcall UpdateSessionColor(TColor value);
  void __fastcall SetSessionColor(TColor value);
  void __fastcall NoteTimer(TObject * Sender);
  void __fastcall AddNote(UnicodeString Note, bool UpdateNow = true);
  void __fastcall PostNote(UnicodeString Note, unsigned int Seconds,
    TNotifyEvent OnNoteClick, TObject * NoteData);
  bool __fastcall CancelNote(bool Force);
  void __fastcall UpdateNoteHints();
  void __fastcall UpdatesChecked();
  void __fastcall UpdatesNoteClicked(TObject * Sender);
  void __fastcall TransferPresetNoteClicked(TObject * Sender);
  void __fastcall TransferPresetNoteMessage(TTransferPresetNoteData * NoteData,
    bool AllowNeverAskAgain);
  void __fastcall UpdateTrayIcon();
  void __fastcall TrayIconClick(TObject * Sender);
  void __fastcall Notify(TTerminal * Terminal, UnicodeString Message,
    TQueryType Type, bool Important = false, TNotifyEvent OnClick = NULL,
    TObject * UserData = NULL, Exception * E = NULL);
  virtual void __fastcall UpdateSessionData(TSessionData * Data);
  virtual void __fastcall UpdateRemotePathComboBox(bool TextOnly);
  virtual void __fastcall ToolbarItemResize(TTBXCustomDropDownItem * Item, int Width);
  virtual void __fastcall CreateWnd();
  virtual void __fastcall DestroyWnd();
  virtual bool __fastcall OpenBookmark(TOperationSide Side, TBookmark * Bookmark);
  void __fastcall DoFindFiles(TTerminal * Terminal, UnicodeString Directory, const TFileMasks & FileMask,
    TFileFoundEvent OnFileFound, TFindingFileEvent OnFindingFile);
  virtual void __fastcall DoFocusRemotePath(TTerminal * Terminal, const UnicodeString & Path);
  bool __fastcall CanOperateOnFoundFiles(TTerminal * ATerminal);
  void __fastcall DoOperationOnFoundFiles(
    TFileOperation Operation, TTerminal * ATerminal, TStrings * FileList, TFileOperationFinishedEvent OnFileOperationFinished);
  void __fastcall DoDeleteFoundFiles(TTerminal * Terminal, TStrings * FileList, TFileOperationFinishedEvent OnFileOperationFinished);
  void __fastcall DoDownloadFoundFiles(TTerminal * ATerminal, TStrings * FileList, TFileOperationFinishedEvent OnFileOperationFinished);
  void __fastcall DoEditFoundFiles(TTerminal * ATerminal, TStrings * FileList, TFileOperationFinishedEvent OnFileOperationFinished);
  bool __fastcall ExecuteFileOperation(TFileOperation Operation, TOperationSide Side,
    bool OnFocused, bool NoConfirmation = false, void * Param = NULL);
  void __fastcall UpdateCopyParamCounters(const TCopyParamType & CopyParam);
  int __fastcall AddSessionColor(TColor Color);
  void UpdateSessionTab(TThemeTabSheet * TabSheet);
  void __fastcall UpdateNewTabTab();
  TThemeTabSheet * GetNewTabTab();
  TThemeTabCaptionTruncation GetNewTabTabCaptionTruncation();
  UnicodeString GetNewTabTabCaption();
  void __fastcall AddFixedSessionImages();
  int __fastcall AddFixedSessionImage(int GlyphsSourceIndex);
  TObjectList * __fastcall DoCollectWorkspace();
  void __fastcall DoSaveWorkspace(const UnicodeString & Name,
    TObjectList * DataList, bool SavePasswords, bool Explicit);
  UnicodeString __fastcall WorkspaceName();
  virtual bool __fastcall EligibleForImageDisplayMode(TTBCustomItem * Item);
  virtual bool __fastcall UpdateToolbarDisplayMode();
  virtual void __fastcall QueueLabelUpdateStatus();
  void __fastcall EditorAutoConfig();
  void __fastcall DirViewContextPopupDefaultItem(
    TOperationSide Side, TTBXCustomItem * Item, TResolvedDoubleClickAction DoubleClickAction1, TResolvedDoubleClickAction DoubleClickAction2);
  void __fastcall DirViewContextPopup(
    TOperationSide Side, Byte PopupComponent, const TPoint & MousePos);
  bool __fastcall CommandLineFromAnotherInstance(const UnicodeString & CommandLine);
  bool __fastcall CanCommandLineFromAnotherInstance();
  void __fastcall SetQueueProgress();
  void __fastcall UpdateQueueLabel();
  void CheckStoreTransition();
  void __fastcall SetTaskbarListProgressState(TBPFLAG Flags);
  void __fastcall SetTaskbarListProgressValue(int Progress);
  void __fastcall SetTaskbarListProgressValue(TFileOperationProgressType * ProgressData);
  TManagedTerminal * __fastcall GetSessionTabSession(TTabSheet * TabSheet);
  bool __fastcall SessionTabSwitched();
  void __fastcall RestoreApp();
  void __fastcall GoToQueue();
  void __fastcall LockFiles(TStrings * FileList, bool Lock);
  void __fastcall SaveInternalEditor(
    const UnicodeString FileName, TEditedFileData * Data, TObject * Token,
    void * Arg);
  void __fastcall SaveAllInternalEditors(TObject * Sender);
  void __fastcall InternalEditorModified(
    const UnicodeString FileName, TEditedFileData * Data, TObject * Token,
    void * Arg);
  void __fastcall AnyInternalEditorModified(TObject * Sender, bool & Modified);
  virtual void __fastcall StartingWithoutSession();
  virtual void __fastcall NeedSession(bool Startup);
  bool __fastcall DraggingAllFilesFromDirView(TOperationSide Side, TStrings * FileList);
  bool __fastcall SelectedAllFilesInDirView(TCustomDirView * DView);
  TSessionData * __fastcall SessionDataForCode();
  void __fastcall RefreshPanel(const UnicodeString & Session, const UnicodeString & Path);
  virtual void __fastcall UpdateImages();
  void __fastcall UpdatePixelsPerInchMainWindowCounter();
  void __fastcall CopyPopup(TControl * DestControl, TControl * SourceControl);
  void __fastcall CreateRemoteDirectory(const UnicodeString & Path, TRemoteProperties & Properties);
  void __fastcall DoFullSynchronize(
    void * Token, TProcessedSynchronizationChecklistItem OnProcessedItem,
    TUpdatedSynchronizationChecklistItems OnUpdatedSynchronizationChecklistItems);
  void DoQueueSynchronize(void * Token);
  void __fastcall DoSynchronizeChecklistCalculateSize(
    TSynchronizeChecklist * Checklist, const TSynchronizeChecklist::TItemList & Items, void * Token);
  void __fastcall DoSynchronizeMove(
    TOperationSide Side, TStrings * FileList, const UnicodeString & NewFileName, bool TargetIsDirectory, void * Token);
  void __fastcall DoSynchronizeBrowse(TOperationSide Side, TSynchronizeChecklist::TAction Action, const TSynchronizeChecklist::TItem * Item);
  void __fastcall FullSynchronize(
    TSynchronizeParams & Params, TProcessedSynchronizationChecklistItem OnProcessedItem,
    TUpdatedSynchronizationChecklistItems OnUpdatedSynchronizationChecklistItems);
  void __fastcall SynchronizeProcessedItem(void * Token, const TSynchronizeChecklist::TItem * ChecklistItem);
  void __fastcall CreateOpenDirMenuList(TTBCustomItem * Menu, TOperationSide Side, TBookmarkList * BookmarkList);
  void __fastcall CreateOpenDirMenu(TTBCustomItem * Menu, TOperationSide Side);
  bool __fastcall TryOpenDirectory(TOperationSide Side, const UnicodeString & Path);
  void __fastcall ClipboardStop();
  void __fastcall ClipboardClear();
  void __fastcall ClipboardDownload(const UnicodeString & TargetDirectory, bool NoConfirmation, bool DragDrop);
  bool __fastcall DoesClipboardContainOurFiles();
  bool __fastcall CanPasteToDirViewFromClipBoard();
  void __fastcall CloseSessionTab(int Index);
  void __fastcall DoFileColorsChanged(TCustomDirView * DirView);
  virtual void __fastcall FileColorsChanged();
  TColor __fastcall PanelColor();
  TColor __fastcall DisabledPanelColor();
  void __fastcall WMSettingChange(TMessage & Message);
  void __fastcall ResetIncrementalSearch();
  void __fastcall IncrementalSearch(const UnicodeString & Text, bool SkipCurrent, bool Reverse);
  TListItem * __fastcall GetNextFile(TListItem * Item, bool Reverse);
  TListItem * __fastcall SearchFile(const UnicodeString & Text, bool SkipCurrent, bool Reverse);
  void __fastcall CMDialogKey(TWMKeyDown & Message);
  DYNAMIC void __fastcall Deactivate();
  void __fastcall CenterReconnectToolbar();
  void DoOpenFolderOrWorkspace(const UnicodeString & Name, bool ConnectFirstTerminal, bool CheckMaxSessions);
  virtual void __fastcall ThemeChanged();
  int __fastcall GetStaticQueuePanelComponentsHeight();
  int __fastcall GetMinQueueViewHeight();
  void __fastcall DetachTerminal(TObject * ATerminal);
  bool __fastcall IsActiveTerminal(TTerminal * Terminal);
  void __fastcall UpdateRowSelect(TCustomDirView * DirView);
  void __fastcall MakeFocusedItemVisible(TCustomDirView * DirView);
  virtual void __fastcall DoRemotePathComboBoxAdjustImageIndex(
    TTBXComboBoxItem * Sender, const UnicodeString AText, int AIndex, int & ImageIndex);
  virtual void __fastcall DoRemotePathComboBoxCancel(TObject * Sender);
  virtual void __fastcall DoRemotePathComboBoxItemClick(TObject * Sender);
  bool GetDoNotShowCopyDialogDefault(bool DragDrop);
  void HandleDoNotShowCopyDialogAgain(bool DragDrop, bool DoNotShowAgain);
  void __fastcall UpdateDarkMode();
  virtual UnicodeString GetTabHintDetails(TManagedTerminal * ASession);
  virtual UnicodeString GetNewTabHintDetails();
  UnicodeString GetTabHintSessionDetails(TManagedTerminal * ASession);
  UnicodeString GetSessionPath(TManagedTerminal * ASession, TOperationSide Side);
  void __fastcall DirectorySizeCalculated(TOperationSide Side, const UnicodeString & FileName, bool Success, bool NotCancelled);
  TListItem * VisualiseOperationFinished(TOperationSide Side, const UnicodeString & FileName, bool Unselect);
  void __fastcall FileDeleted(TOperationSide Side, const UnicodeString & FileName, bool Success, bool NotCancelled);
  void LoadFilesProperties(TStrings * FileList);
  void PasteFiles();
  bool DoDirectoryExists(void * Session, const UnicodeString & Directory);
  void DoBrowseFile(TCustomDirView * DirView, const UnicodeString & FileName);
  bool NeedSecondarySessionForRemoteCopy(TStrings * FileList);
  void ReleaseHiContrastTheme();
  bool CanCalculateChecksum();
  void RegenerateSessionColorsImageList();
  void WMQueueCallback(TMessage & Message);

public:
  virtual __fastcall ~TCustomScpExplorerForm();
  void __fastcall AddBookmark(TOperationSide Side);
  virtual void __fastcall AddEditLink(TOperationSide Side, bool Add);
  bool __fastcall CanAddEditLink(TOperationSide Side);
  bool __fastcall LinkFocused();
  virtual Boolean __fastcall AllowedAction(TAction * Action, TActionAllowed Allowed);
  bool __fastcall IsBusy();
  virtual void __fastcall ConfigurationChanged();
  void __fastcall CreateDirectory(TOperationSide Side);
  void __fastcall ExecuteFileOperationCommand(TFileOperation Operation, TOperationSide Side,
    bool OnFocused, bool NoConfirmation = false, void * Param = NULL);
  void __fastcall ExecuteCopyOperationCommand(
    TOperationSide Side, bool OnFocused, unsigned int Flags);
  virtual void LocalLocalCopy(
    TFileOperation Operation, TOperationSide Side, bool OnFocused, bool NoConfirmation, bool DragDrop, unsigned int Flags);
  void LocalLocalCopyCommand(TFileOperation Operation, TOperationSide Side, bool OnFocused, unsigned int Flags);
  void __fastcall AdHocCustomCommand(bool OnFocused);
  virtual TCustomDirView * __fastcall DirView(TOperationSide Side);
  virtual bool __fastcall DirViewEnabled(TOperationSide Side);
  virtual void __fastcall ChangePath(TOperationSide Side) = 0;
  virtual void __fastcall StoreParams();
  int __fastcall CustomCommandState(const TCustomCommandType & Command, bool OnFocused, TCustomCommandListType ListType);
  bool __fastcall GetLastCustomCommand(bool OnFocused,
    TCustomCommandType & CustomCommand, int & State);
  void __fastcall LastCustomCommand(bool OnFocused);
  void __fastcall BothCustomCommand(const TCustomCommandType & Command);
  void __fastcall LockWindow(bool Force = false);
  void __fastcall UnlockWindow();
  void __fastcall SuspendWindowLock();
  void __fastcall ResumeWindowLock();
  bool __fastcall HasActiveTerminal();
  bool __fastcall HasManagedSession();
  virtual bool IsLocalBrowserMode();
  bool CanCloseSession(TManagedTerminal * Session);
  virtual UnicodeString __fastcall DefaultDownloadTargetDirectory() = 0;
  virtual bool SupportedSession(TSessionData * SessionData) = 0;
  TOperationSide GetOtherSide(TOperationSide Side);

  void __fastcall NewSession(const UnicodeString & SessionUrl = L"");
  virtual void NewTab(TOperationSide Side = osCurrent, bool AllowReverse = true);
  void __fastcall DuplicateTab();
  void __fastcall RenameTab();
  void __fastcall CloseTab();
  void __fastcall DisconnectSession();
  void __fastcall ReconnectSession();
  void __fastcall OpenDirectory(TOperationSide Side);
  virtual void __fastcall HomeDirectory(TOperationSide Side);
  void __fastcall ReloadDirectory(TOperationSide Side);
  void __fastcall OpenStoredSession(TSessionData * Data);
  void __fastcall OpenFolderOrWorkspace(const UnicodeString & Name);
  void __fastcall Idle();
  __fastcall TCustomScpExplorerForm(TComponent* Owner);
  void __fastcall SaveCurrentSession();
  UnicodeString __fastcall CreateHiddenDuplicateSession();
  UnicodeString __fastcall SaveHiddenDuplicateSession(TSessionData * SessionData);
  TSessionData * __fastcall CloneCurrentSessionData();
  bool __fastcall SaveWorkspace(bool EnableAutoSave);
  virtual void __fastcall CompareDirectories();
  void __fastcall ExecuteCurrentFile();
  virtual void __fastcall OpenConsole(UnicodeString Command = L"");
  virtual void __fastcall UpdateSession(TManagedTerminal * Terminal);
  virtual void __fastcall SynchronizeDirectories();
  virtual void __fastcall FullSynchronizeDirectories() = 0;
  virtual void __fastcall ExploreLocalDirectory(TOperationSide Side);
  virtual void __fastcall GoToCommandLine();
  virtual void __fastcall GoToTree();
  void __fastcall PanelExport(TOperationSide Side, TPanelExport Export,
    TPanelExportDestination Destination);
  void __fastcall Filter(TOperationSide Side);
  void __fastcall ExecuteFile(TOperationSide Side, TExecuteFileBy ExecuteFileBy,
    const TEditorData * ExternalEditor = NULL, bool AllSelected = false,
    bool OnFocused = false);
  void __fastcall ExecuteCurrentFileWith(bool OnFocused);
  void __fastcall EditNew(TOperationSide Side);
  bool __fastcall AllowQueueOperation(TQueueOperation Operation, void ** Param = NULL);
  void __fastcall ExecuteQueueOperation(TQueueOperation Operation, void * Param = NULL);
  TQueueOperation __fastcall DefaultQueueOperation();
  bool __fastcall GetQueueEnabled();
  void __fastcall ToggleQueueEnabled();
  UnicodeString __fastcall GetQueueProgressTitle();
  void __fastcall LastTerminalClosed();
  virtual TManagedTerminal * GetReplacementForLastSession();
  void __fastcall TerminalRemoved(TObject * Sender);
  void __fastcall TerminalDisconnected();
  void __fastcall TerminalConnecting();
  void __fastcall SessionListChanged(bool ForceTruncationUpdate = false);
  void __fastcall ApplicationTitleChanged();
  unsigned int __fastcall MoreMessageDialog(const UnicodeString Message,
    TStrings * MoreMessages, TQueryType Type, unsigned int Answers,
    UnicodeString HelpKeyword, const TMessageParams * Params = NULL,
    TTerminal * Terminal = NULL);
  void __fastcall OperationFinished(TFileOperation Operation, TOperationSide Side,
    bool Temp, const UnicodeString & FileName, bool Success, bool NotCancelled, TOnceDoneOperation & OnceDoneOperation);
  void __fastcall OperationProgress(TFileOperationProgressType & ProgressData);
  UnicodeString __fastcall GetProgressTitle();
  void __fastcall ShowExtendedException(TTerminal * Terminal, Exception * E);
  void __fastcall InactiveTerminalException(TTerminal * Terminal, Exception * E);
  void __fastcall SessionReady();
  void __fastcall QueueEvent(TManagedTerminal * Terminal, TTerminalQueue * Queue, TQueueEvent Event);
  void __fastcall QueueEmptyNoteClicked(TObject * Sender);
  bool __fastcall DoSynchronizeDirectories(
    UnicodeString & LocalDirectory, UnicodeString & RemoteDirectory, int UseDefaults);
  int __fastcall DoFullSynchronizeDirectories(
    UnicodeString & LocalDirectory, UnicodeString & RemoteDirectory, TSynchronizeMode & Mode,
    int Params, bool & SaveMode, int UseDefaults);
  void __fastcall StandaloneEdit(const UnicodeString & FileName);
  bool __fastcall CanPasteFromClipBoard();
  virtual void __fastcall PasteFromClipBoard();
  virtual void __fastcall CopyFilesToClipboard(TOperationSide Side, bool OnFocused);
  void __fastcall ToggleQueueVisibility();
  virtual UnicodeString __fastcall PathForCaption();
  void __fastcall FileListFromClipboard();
  void __fastcall SelectAll(TOperationSide Side, TSelectMode Mode);
  void __fastcall SelectByMask(TOperationSide Side, bool Select);
  void __fastcall RestoreSelectedNames(TOperationSide Side);
  void __fastcall SelectSameExt(bool Select);
  void __fastcall PreferencesDialog(TPreferencesMode APreferencesMode);
  virtual void __fastcall BeforeAction();
  void __fastcall FileSystemInfo();
  void __fastcall SessionGenerateUrl();
  void __fastcall FileGenerateUrl();
  void __fastcall ReadDirectoryCancelled();
  void __fastcall SynchronizeBrowsingChanged();
  void __fastcall ToggleShowHiddenFiles();
  void __fastcall SetFormatSizeBytes(TFormatBytesStyle Style);
  void __fastcall ToggleAutoReadDirectoryAfterOp();
  void __fastcall PopupTrayBalloon(TTerminal * Terminal, const UnicodeString & Str,
    TQueryType Type, Exception * E = NULL, unsigned int Seconds = 0,
    TNotifyEvent OnBalloonClick = NULL, TObject * UserData = NULL);
  void __fastcall RemoteFindFiles();
  virtual void __fastcall HistoryGo(TOperationSide Side, int Index);
  void __fastcall UpdateTaskbarList(ITaskbarList3 * TaskbarList);
  virtual void __fastcall DisplaySystemContextMenu();
  virtual void __fastcall GoToAddress() = 0;
  bool __fastcall CanConsole();
  bool __fastcall CanChangePassword();
  void __fastcall ChangePassword();
  bool __fastcall CanPrivateKeyUpload();
  void __fastcall PrivateKeyUpload();
  bool __fastcall IsComponentPossible(Byte Component);
  void __fastcall ReplaceTerminal(TManagedTerminal * value);
  virtual void __fastcall BrowseFile(const UnicodeString & FileName);
  void __fastcall CloseApp();
  virtual bool SupportsLocalBrowser();
  virtual bool IsSideLocalBrowser(TOperationSide Side);
  virtual UnicodeString GetLocalBrowserSessionTitle(TManagedTerminal * Session);
  virtual int GetNewTabActionImageIndex();
  virtual int GetNewTabTabImageIndex(TOperationSide Side);
  void CalculateDirectorySizes(TOperationSide Side);
  void AutoSizeColumns(TOperationSide Side);
  virtual void ResetLayoutColumns(TOperationSide Side) = 0;
  void QueueResetLayoutColumns();
  void IncrementalSearchStart();
  virtual void * SaveFocus();
  virtual void RestoreFocus(void * Focus);
  virtual void __fastcall UpdateControls();
  TThumbnailDownloadQueueItem * AddThumbnailDownloadQueueItem(TManagedTerminal * ATerminal);
  void PostThumbnailVisibleQueueQuery(int Index, const UnicodeString & FileName);
  void PostThumbnailDrawRequest(int Index);
  void ChangeDirViewStyle(TOperationSide Side, TDirViewStyle DirViewStyle);

  __property bool ComponentVisible[Byte Component] = { read = GetComponentVisible, write = SetComponentVisible };
  __property bool EnableFocusedOperation[TOperationSide Side] = { read = GetEnableFocusedOperation, index = 0 };
  __property bool EnableSelectedOperation[TOperationSide Side] = { read = GetEnableSelectedOperation, index = 0 };
  __property bool EnableFocusedFileOperation[TOperationSide Side] = { read = GetEnableFocusedOperation, index = 1 };
  __property bool EnableSelectedFileOperation[TOperationSide Side] = { read = GetEnableSelectedOperation, index = 1 };
  __property bool HasDirView[TOperationSide Side] = { read = GetHasDirView };
  __property TManagedTerminal * ManagedSession = { read = FManagedSession, write = SetManagedSession };
  __property TManagedTerminal * Terminal = { read = GetTerminal };
  __property TTerminalQueue * Queue = { read = FQueue, write = SetQueue };
  __property TColor SessionColor = { read = FSessionColor, write = SetSessionColor };
  __property bool StandaloneOperation = { read = FStandaloneOperation, write = FStandaloneOperation };
};
//---------------------------------------------------------------------------
class TAutoBatch
{
public:
  TAutoBatch(TCustomScpExplorerForm * Form);
  ~TAutoBatch();

private:
  TCustomScpExplorerForm * FForm;
  void * FBatchStorage;
};
//---------------------------------------------------------------------------
#endif
