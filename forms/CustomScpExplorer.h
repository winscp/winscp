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
class TEditorPreferences;
class TTransferPresetNoteData;
struct TEditedFileData;
//---------------------------------------------------------------------------
enum TActionAllowed { aaShortCut, aaUpdate, aaExecute };
enum TActionFlag { afLocal = 1, afRemote = 2, afExplorer = 4 , afCommander = 8 };
enum TExecuteFileBy { efShell = 1, efInternalEditor = 2, efExternalEditor = 3, efDefaultEditor = 100 };
enum TPanelExport { pePath, peFileList, peFullFileList, peUrl };
enum TPanelExportDestination { pedClipboard, pedCommandLine };
//---------------------------------------------------------------------------
struct TCustomCommandParam
{
  TCustomCommandParam();
  AnsiString Name;
  AnsiString Command;
  int Params;
};
//---------------------------------------------------------------------------
class TCustomScpExplorerForm : public TForm
{
__published:
  TPanel *RemotePanel;
  TTBXStatusBar *RemoteStatusBar;
  TUnixDirView *RemoteDirView;
  TTBXDock *TopDock;
  TListView *QueueView2;
  TPanel *QueuePanel;
  TSplitter *QueueSplitter;
  TTBXToolbar *QueueToolbar;
  TTBXDock *QueueDock;
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
  TSplitter *RemotePanelSplitter;
  TTBXItem *TBXItem194;
  TTBXItem *TBXItem195;
  TTBXItem *TBXItem210;
  void __fastcall RemoteDirViewContextPopup(TObject *Sender,
    const TPoint &MousePos, bool &Handled);
  void __fastcall RemoteDirViewGetSelectFilter(
    TCustomDirView *Sender, bool Select, TFileFilter &Filter);
  void __fastcall ApplicationHint(TObject * Sender);
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
  void __fastcall QueueView2ContextPopup(TObject *Sender, TPoint &MousePos,
    bool &Handled);
  void __fastcall QueueView2Deletion(TObject *Sender, TListItem *Item);
  void __fastcall QueueView2StartDrag(TObject *Sender,
    TDragObject *&DragObject);
  void __fastcall QueueView2DragOver(TObject *Sender, TObject *Source,
    int X, int Y, TDragState State, bool &Accept);
  void __fastcall QueueView2DragDrop(TObject *Sender, TObject *Source,
    int X, int Y);
  void __fastcall QueueView2Enter(TObject *Sender);
  void __fastcall QueueView2SelectItem(TObject *Sender, TListItem *Item,
    bool Selected);
  void __fastcall RemoteFileControlDDFileOperation(TObject * Sender,
    int Effect, AnsiString SourcePath, AnsiString TargetPath,
    bool & DoOperation);
  void __fastcall RemoteFileContolDDChooseEffect(TObject * Sender,
    int grfKeyState, int & dwEffect);
  void __fastcall RemoteFileControlDDDragFileName(TObject * Sender,
    TRemoteFile * File, AnsiString & FileName);
  void __fastcall RemoteFileControlDDDragDetect(TObject * Sender,
    int grfKeyState, const TPoint & DetectStart, const TPoint & Point,
    TDragDetectStatus DragStatus);
  void __fastcall RemoteFileControlDDQueryContinueDrag(TObject *Sender,
          BOOL FEscapePressed, int grfKeyState, HRESULT &Result);
  void __fastcall RemoteDirViewEnter(TObject *Sender);
  void __fastcall RemoteDriveViewEnter(TObject *Sender);
  void __fastcall DirViewMatchMask(TObject *Sender, AnsiString FileName,
    bool Directory, __int64 Size, AnsiString Masks, bool &Matches);
  void __fastcall RemoteDirViewGetOverlay(TObject *Sender, TListItem *Item,
    WORD &Indexes);
  void __fastcall DirViewHistoryChange(TCustomDirView *Sender);
  void __fastcall RemoteStatusBarClick(TObject *Sender);
  void __fastcall DirViewLoaded(TObject *Sender);
  void __fastcall ToolbarGetBaseSize(TTBCustomToolbar * Toolbar, TPoint & ASize);
  void __fastcall FormConstrainedResize(TObject * Sender, int & MinWidth,
    int  &MinHeight, int  &MaxWidth, int  &MaxHeight);
  void __fastcall SessionColorPaletteChange(TObject * Sender);
  void __fastcall StatusBarPanelDblClick(TTBXCustomStatusBar * Sender,
    TTBXStatusPanel * Panel);
  void __fastcall RemotePathComboBoxAdjustImageIndex(
    TTBXComboBoxItem * Sender, const AnsiString AText, int AIndex,
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

private:
  TTerminal * FTerminal;
  TTerminalQueue * FQueue;
  TTerminalQueueStatus * FQueueStatus;
  TCriticalSection * FQueueStatusSection;
  bool FQueueStatusInvalidated;
  bool FQueueItemInvalidated;
  bool FFormRestored;
  bool FAutoOperation;
  bool FForceExecution;
  bool FShowStatusBarHint;
  AnsiString FStatusBarHint;
  bool FIgnoreNextSysCommand;
  TStringList * FErrorList;
  HANDLE FDDExtMutex;
  AnsiString FDragExtFakeDirectory;
  TStrings * FDelayedDeletionList;
  TTimer * FDelayedDeletionTimer;
  TStrings * FDDFileList;
  __int64 FDDTotalSize;
  AnsiString FDragDropSshTerminate;
  HINSTANCE FOle32Library;
  HCURSOR FDragMoveCursor;
  AnsiString FDragTempDir;
  bool FRefreshLocalDirectory;
  bool FRefreshRemoteDirectory;
  TListItem * FQueueActedItem;
  TQueueController * FQueueController;
  int FLastDropEffect;
  bool FPendingTempSpaceWarn;
  TEditorManager * FEditorManager;
  TStrings * FCapturedLog;
  bool FDragDropOperation;
  AnsiString FCopyParamDefault;
  AnsiString FCopyParamAutoSelected;
  bool FEditingFocusedAdHocCommand;
  TList * FDocks;
  TSynchronizeController * FSynchronizeController;
  AnsiString FTransferDropDownHint;
  int FTransferListHoverIndex;
  TColor FSessionColor;
  TTrayIcon * FTrayIcon;
  TCustomCommandParam FLastCustomCommand;

  bool __fastcall GetEnableFocusedOperation(TOperationSide Side, int FilesOnly);
  bool __fastcall GetEnableSelectedOperation(TOperationSide Side, int FilesOnly);
  void __fastcall SetTerminal(TTerminal * value);
  void __fastcall SetQueue(TTerminalQueue * value);
  void __fastcall SessionComboPopup(TTBCustomItem * Sender, bool FromLink);
  void __fastcall SessionComboDrawItem(TTBXCustomList * Sender, TCanvas * ACanvas,
    const TRect & ARect, int AIndex, int AHoverIndex, bool & DrawDefault);
  void __fastcall SessionComboChange(TObject * Sender, const AnsiString Text);
  void __fastcall TransferListChange(TObject * Sender);
  void __fastcall TransferListDrawItem(TTBXCustomList * Sender, TCanvas * ACanvas,
    const TRect & ARect, int AIndex, int AHoverIndex, bool & DrawDefault);
  void __fastcall CloseInternalEditor(TObject * Sender);
  void __fastcall ForceCloseInternalEditor(TObject * Sender);
  void __fastcall TerminalCaptureLog(const AnsiString & AddedLine, bool StdError);
  void __fastcall HistoryItemClick(System::TObject* Sender);
  void __fastcall UpdateHistoryMenu(TOperationSide Side, bool Back);
  void __fastcall AdHocCustomCommandValidate(const AnsiString & Command,
    int Params);
  void __fastcall SetDockAllowDrag(bool value);
  void __fastcall QueueSplitterDblClick(TObject * Sender);
  void __fastcall ApplicationMinimize(TObject * Sender);
  void __fastcall ApplicationRestore(TObject * Sender);
  bool __fastcall MainWindowHook(TMessage & Message);

protected:
  TOperationSide FCurrentSide;
  TControl * FDDTargetControl;
  TProgressForm * FProgressForm;
  TSynchronizeProgressForm * FSynchronizeProgressForm;
  HANDLE FDDExtMapFile;
  bool FDDMoveSlipped;
  TTimer * FUserActionTimer;
  TQueueItemProxy * FPendingQueueActionItem;
  TTBXPopupMenu * FHistoryMenu[2][2];
  bool FNoTransferPresetAutoSelect;
  TStrings * FNotes;
  TTimer * FNoteTimer;
  TDateTime FNoteShown;
  AnsiString FNote;
  TObject * FNoteData;
  AnsiString FNoteHints;
  TNotifyEvent FOnNoteClick;
  unsigned int FLockLevel;
  TImageList * FSystemImageList;
  bool FAlternativeDelete;

  virtual bool __fastcall CopyParamDialog(TTransferDirection Direction,
    TTransferType Type, bool Temp, TStrings * FileList,
    AnsiString & TargetDirectory, TGUICopyParamType & CopyParam, bool Confirm,
    bool DragDrop);
  virtual bool __fastcall RemoteTransferDialog(TTerminal *& Session,
    AnsiString & Target, AnsiString & FileMask, bool & DirectCopy,
    bool NoConfirmation, bool Move);
  virtual void __fastcall CreateParams(TCreateParams & Params);
  void __fastcall DeleteFiles(TOperationSide Side, TStrings * FileList, bool Alternative);
  void __fastcall RemoteTransferFiles(TStrings * FileList, bool NoConfirmation, bool Move);
  virtual void __fastcall DoDirViewExecFile(TObject * Sender, TListItem * Item, bool & AllowExec);
  virtual TControl * __fastcall GetComponent(Byte Component);
  bool __fastcall GetComponentVisible(Word Component);
  virtual Boolean __fastcall GetHasDirView(TOperationSide Side);
  DYNAMIC void __fastcall KeyDown(Word & Key, Classes::TShiftState Shift);
  virtual void __fastcall RestoreFormParams();
  virtual void __fastcall RestoreParams();
  virtual void __fastcall SetComponentVisible(Word Component, bool value);
  virtual void __fastcall ComponentShowing(Word Component, bool value);
  virtual void __fastcall FixControlsPlacement();
  void __fastcall SetProperties(TOperationSide Side, TStrings * FileList);
  void __fastcall CustomCommand(TStrings * FileList, AnsiString Name,
    AnsiString Command, int Params);
  virtual void __fastcall TerminalChanging();
  virtual void __fastcall TerminalChanged();
  virtual void __fastcall QueueChanged();
  void __fastcall InitStatusBar();
  void __fastcall UpdateStatusBar();
  virtual void __fastcall UpdateStatusPanelText(TTBXStatusPanel * Panel);
  virtual void __fastcall DoOperationFinished(TFileOperation Operation,
    TOperationSide Side, bool Temp, const AnsiString FileName, bool Success,
    bool & DisconnectWhenFinished);
  virtual void __fastcall DoOpenDirectoryDialog(TOpenDirectoryMode Mode, TOperationSide Side);
  virtual void __fastcall FileOperationProgress(
    TFileOperationProgressType & ProgressData, TCancelStatus & Cancel);
  void __fastcall OperationComplete(const TDateTime & StartTime);
  void __fastcall ExecutedFileChanged(const AnsiString FileName,
    const TEditedFileData & Data, HANDLE UploadCompleteEvent);
  void __fastcall ExecutedFileReload(const AnsiString FileName,
    const TEditedFileData & Data);
  void __fastcall ExecutedFileEarlyClosed(const TEditedFileData & Data,
    bool & KeepOpen);
  void __fastcall CMAppSysCommand(TMessage & Message);
  void __fastcall WMAppCommand(TMessage & Message);
  void __fastcall WMSysCommand(TMessage & Message);
  void __fastcall WMWindowPosChanging(TWMWindowPosMsg & Message);
  virtual void __fastcall SysResizing(unsigned int Cmd);
  DYNAMIC void __fastcall DoShow();
  TStrings * __fastcall CreateVisitedDirectories(TOperationSide Side);
  void __fastcall HandleErrorList(TStringList *& ErrorList);
  void __fastcall TerminalSynchronizeDirectory(const AnsiString LocalDirectory,
    const AnsiString RemoteDirectory, bool & Continue, bool Collect);
  void __fastcall DoSynchronize(TSynchronizeController * Sender,
    const AnsiString LocalDirectory, const AnsiString RemoteDirectory,
    const TCopyParamType & CopyParam, const TSynchronizeParamType & Params,
    TSynchronizeChecklist ** Checklist, TSynchronizeOptions * Options, bool Full);
  void __fastcall DoSynchronizeInvalid(TSynchronizeController * Sender,
    const AnsiString Directory, const AnsiString ErrorStr);
  void __fastcall DoSynchronizeTooManyDirectories(TSynchronizeController * Sender,
    int & MaxDirectories);
  void __fastcall Synchronize(const AnsiString LocalDirectory,
    const AnsiString RemoteDirectory, TSynchronizeMode Mode,
    const TCopyParamType & CopyParam, int Params, TSynchronizeChecklist ** Checklist,
    TSynchronizeOptions * Options);
  void __fastcall GetSynchronizeOptions(int Params, TSynchronizeOptions & Options);
  bool __fastcall SynchronizeAllowSelectedOnly();
  virtual void __fastcall BatchStart(void *& Storage);
  virtual void __fastcall BatchEnd(void * Storage);
  void __fastcall ExecuteFileOperation(TFileOperation Operation, TOperationSide Side,
    TStrings * FileList, bool NoConfirmation, void * Param);
  virtual void __fastcall DDGetTarget(AnsiString & Directory);
  virtual void __fastcall DDExtInitDrag(TFileList * FileList, bool & Created);
  virtual void __fastcall SideEnter(TOperationSide Side);
  virtual TOperationSide __fastcall GetSide(TOperationSide Side);
  virtual void __fastcall PanelExportStore(TOperationSide Side,
    TPanelExport Export, TPanelExportDestination Destination,
    TStringList * ExportData);
  void __fastcall QueueListUpdate(TTerminalQueue * Queue);
  void __fastcall QueueItemUpdate(TTerminalQueue * Queue, TQueueItem * Item);
  void __fastcall UpdateQueueStatus(bool AppIdle);
  void __fastcall RefreshQueueItems(bool AppIdle);
  virtual int __fastcall GetStaticComponentsHeight();
  void __fastcall FillQueueViewItem(TListItem * Item,
    TQueueItemProxy * QueueItem, bool Detail);
  void __fastcall QueueViewDeleteItem(int Index);
  void __fastcall UserActionTimer(TObject * Sender);
  void __fastcall UpdateQueueView();
  bool __fastcall CanCloseQueue();
  virtual bool __fastcall IsFileControl(TObject * Control, TOperationSide Side);
  virtual void __fastcall ReloadLocalDirectory(const AnsiString Directory = "");
  virtual bool __fastcall PanelOperation(TOperationSide Side, bool DragDrop);
  void __fastcall DoWarnLackOfTempSpace(const AnsiString Path,
    __int64 RequiredSpace, bool & Continue);
  void __fastcall AddDelayedDirectoryDeletion(const AnsiString TempDir, int SecDelay);
  void __fastcall DoDelayedDeletion(TObject * Sender);
  TDragDropFilesEx * __fastcall DragDropFiles(TObject * Sender);
  void __fastcall RemoteFileControlDDTargetDrop();
  void __fastcall RemoteFileControlFileOperation(TObject * Sender,
    TFileOperation Operation, bool NoConfirmation, void * Param);
  void __fastcall DDDownload(TStrings * FilesToCopy,
    const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params);
  bool __fastcall EnsureCommandSessionFallback(TFSCapability Capability);
  bool __fastcall CommandSessionFallback();
  void __fastcall FileTerminalClosed(const AnsiString FileName,
    TEditedFileData & Data, TObject * Token, void * Arg);
  void __fastcall FileConfigurationChanged(const AnsiString FileName,
    TEditedFileData & Data, TObject * Token, void * Arg);
  void __fastcall CustomExecuteFile(TOperationSide Side,
    TExecuteFileBy ExecuteFileBy, AnsiString FileName, AnsiString OriginalFileName,
    const TEditorPreferences * ExternalEditor, AnsiString LocalRootDirectory);
  bool __fastcall RemoteExecuteForceText(TExecuteFileBy ExecuteFileBy,
    const TEditorPreferences * ExternalEditor);
  void __fastcall ExecuteFileNormalize(TExecuteFileBy & ExecuteFileBy,
    const TEditorPreferences *& ExternalEditor, const AnsiString & FileName,
    bool Local, const TFileMasks::TParams & MaskParams);
  AnsiString __fastcall TemporaryDirectoryForRemoteFiles(TCopyParamType CopyParam,
    AnsiString & RootDirectory);
  void __fastcall TemporarilyDownloadFiles(TStrings * FileList, bool ForceText,
    AnsiString & RootTempDir, AnsiString & TempDir, bool AllFiles, bool GetTargetNames,
    bool AutoOperation);
  TTBXPopupMenu * __fastcall HistoryMenu(TOperationSide Side, bool Back);
  AnsiString __fastcall FileStatusBarText(const TStatusFileInfo & FileInfo);
  void __fastcall UpdateFileStatusBar(TTBXStatusBar * StatusBar,
    const TStatusFileInfo & FileInfo, int Panel);
  virtual void __fastcall DoDirViewLoaded(TCustomDirView * Sender);
  virtual void __fastcall UpdateControls();
  void __fastcall UpdateTransferList();
  void __fastcall UpdateTransferLabel();
  void __fastcall StartUpdates();
  void __fastcall TransferPresetAutoSelect();
  virtual void __fastcall GetTransferPresetAutoSelectData(TCopyParamRuleData & Data);
  int __fastcall CustomCommandState(const AnsiString & Command, int Params, bool OnFocused);
  inline bool __fastcall CustomCommandRemoteAllowed();
  void __fastcall LoadToolbarsLayoutStr(AnsiString LayoutStr);
  AnsiString __fastcall GetToolbarsLayoutStr();
  virtual void __fastcall Dispatch(void * Message);
  void __fastcall PostComponentHide(unsigned short Component);
  void __fastcall GetSpaceAvailable(const AnsiString Path,
    TSpaceAvailable & ASpaceAvailable, bool & Close);
  void __fastcall CalculateSizeEvent(TStrings * FileList, __int64 & Size,
    TCalculateSizeStats & Stats, bool & Close);
  void __fastcall CalculateChecksum(const AnsiString & Alg, TStrings * FileList,
    TCalculatedChecksumEvent OnCalculatedChecksum, bool & Close);
  void __fastcall UpdateCustomCommandsToolbar();
  virtual void __fastcall UpdateActions();
  void __fastcall SetSessionColor(TColor value);
  void __fastcall NoteTimer(TObject * Sender);
  void __fastcall AddNote(AnsiString Note, bool UpdateNow = true);
  void __fastcall PostNote(AnsiString Note, unsigned int Seconds,
    TNotifyEvent OnNoteClick, TObject * NoteData);
  bool __fastcall CancelNote();
  void __fastcall UpdatesChecked();
  void __fastcall UpdatesNoteClicked(TObject * Sender);
  void __fastcall TransferPresetNoteClicked(TObject * Sender);
  void __fastcall TransferPresetNoteMessage(TTransferPresetNoteData * NoteData,
    bool AllowNeverAskAgain);
  void __fastcall UpdateTrayIcon();
  void __fastcall TrayIconClick(TObject * Sender);
  void __fastcall Notify(TTerminal * Terminal, AnsiString Message,
    TQueryType Type, bool Important = false, TNotifyEvent OnClick = NULL,
    Exception * E = NULL);
  bool __fastcall OpenInNewWindow();
  virtual void __fastcall UpdateSessionData(TSessionData * Data);
  virtual void __fastcall UpdateRemotePathComboBox(
    TTBXComboBoxItem * RemotePathComboBox, bool TextOnly);
  virtual void __fastcall ToolbarItemResize(TTBXCustomDropDownItem * Item, int Width);
  void __fastcall ClickToolbarItem(TTBCustomItem * Item, bool PositionCursor);

public:
  virtual __fastcall ~TCustomScpExplorerForm();
  void __fastcall AddBookmark(TOperationSide Side);
  virtual void __fastcall AddEditLink(bool Add);
  bool __fastcall CanAddEditLink();
  bool __fastcall LinkFocused();
  virtual Boolean __fastcall AllowedAction(TAction * Action, TActionAllowed Allowed) = 0;
  virtual void __fastcall ConfigurationChanged();
  void __fastcall CreateDirectory(TOperationSide Side);
  void __fastcall ExecuteFileOperation(TFileOperation Operation, TOperationSide Side,
    bool OnFocused, bool NoConfirmation = false, void * Param = NULL);
  void __fastcall AdHocCustomCommand(bool OnFocused);
  virtual TCustomDirView * __fastcall DirView(TOperationSide Side);
  virtual void __fastcall ChangePath(TOperationSide Side) = 0;
  virtual void __fastcall StoreParams();
  int __fastcall CustomCommandState(const AnsiString & Description, bool OnFocused);
  bool __fastcall GetLastCustomCommand(bool OnFocused,
    TCustomCommandParam & CustomCommand, int & State);
  void __fastcall LastCustomCommand(bool OnFocused);
  void __fastcall LockWindow();
  void __fastcall UnlockWindow();

  void __fastcall NewSession();
  void __fastcall DuplicateSession();
  void __fastcall CloseSession();
  void __fastcall OpenDirectory(TOperationSide Side);
  virtual void __fastcall HomeDirectory(TOperationSide Side);
  void __fastcall OpenStoredSession(TSessionData * Data);
  void __fastcall Idle(bool AppIdle);
  __fastcall TCustomScpExplorerForm(TComponent* Owner);
  void __fastcall SaveCurrentSession();
  virtual void __fastcall CompareDirectories();
  void __fastcall ExecuteCurrentFile();
  virtual void __fastcall OpenConsole(AnsiString Command = "");
  virtual void __fastcall UpdateTerminal(TTerminal * Terminal);
  virtual void __fastcall SynchronizeDirectories();
  virtual void __fastcall FullSynchronizeDirectories() = 0;
  virtual void __fastcall ExploreLocalDirectory();
  virtual void __fastcall GoToCommandLine();
  virtual void __fastcall GoToTree();
  virtual void __fastcall PanelExport(TOperationSide Side, TPanelExport Export,
    TPanelExportDestination Destination, bool OnFocused = false);
  void __fastcall ExecuteFile(TOperationSide Side, TExecuteFileBy ExecuteFileBy,
    const TEditorPreferences * ExternalEditor = NULL, bool AllSelected = false,
    bool OnFocused = false);
  void __fastcall EditNew(TOperationSide Side);
  bool __fastcall AllowQueueOperation(TQueueOperation Operation, void ** Param = NULL);
  void __fastcall ExecuteQueueOperation(TQueueOperation Operation, void * Param = NULL);
  TQueueOperation __fastcall DefaultQueueOperation();
  void __fastcall LastTerminalClosed(TObject * Sender);
  void __fastcall TerminalClosed(TObject * Sender);
  void __fastcall TerminalListChanged(TObject * Sender);
  void __fastcall ApplicationTitleChanged();
  int __fastcall MoreMessageDialog(const AnsiString Message,
    TStrings * MoreMessages, TQueryType Type, int Answers,
    AnsiString HelpKeyword, const TMessageParams * Params = NULL,
    TTerminal * Terminal = NULL);
  void __fastcall OperationFinished(TFileOperation Operation, TOperationSide Side,
    bool Temp, const AnsiString FileName, bool Success, bool & DisconnectWhenFinished);
  void __fastcall OperationProgress(TFileOperationProgressType & ProgressData, TCancelStatus & Cancel);
  void __fastcall ShowExtendedException(TTerminal * Terminal, Exception * E);
  void __fastcall InactiveTerminalException(TTerminal * Terminal, Exception * E);
  void __fastcall QueueEvent(TTerminal * Terminal, TTerminalQueue * Queue, TQueueEvent Event);
  bool __fastcall DoSynchronizeDirectories(AnsiString & LocalDirectory,
    AnsiString & RemoteDirectory, bool UseDefaults);
  bool __fastcall DoFullSynchronizeDirectories(AnsiString & LocalDirectory,
    AnsiString & RemoteDirectory, TSynchronizeMode & Mode, bool & SaveMode,
    bool UseDefaults);
  bool __fastcall CanPasteFromClipBoard();
  void __fastcall PasteFromClipBoard();
  void __fastcall ToggleQueueVisibility();
  virtual AnsiString __fastcall PathForCaption();
  void __fastcall FileListFromClipboard();
  void __fastcall PreferencesDialog(TPreferencesMode APreferencesMode);
  void __fastcall WhatsThis();
  virtual void __fastcall BeforeAction();
  void __fastcall FileSystemInfo();
  void __fastcall SessionColorPick();
  void __fastcall ReadDirectoryCancelled();
  void __fastcall SynchronizeBrowsingChanged();
  void __fastcall ToggleShowHiddenFiles();
  void __fastcall ToggleAutoReadDirectoryAfterOp();
  void __fastcall PopupTrayBalloon(TTerminal * Terminal, const AnsiString & Str,
    TQueryType Type, Exception * E = NULL, unsigned int Seconds = 0);

  __property bool ComponentVisible[Word Component] = { read = GetComponentVisible, write = SetComponentVisible };
  __property bool EnableFocusedOperation[TOperationSide Side] = { read = GetEnableFocusedOperation, index = 0 };
  __property bool EnableSelectedOperation[TOperationSide Side] = { read = GetEnableSelectedOperation, index = 0 };
  __property bool EnableFocusedFileOperation[TOperationSide Side] = { read = GetEnableFocusedOperation, index = 1 };
  __property bool EnableSelectedFileOperation[TOperationSide Side] = { read = GetEnableSelectedOperation, index = 1 };
  __property bool HasDirView[TOperationSide Side] = { read = GetHasDirView };
  __property TTerminal * Terminal = { read = FTerminal, write = SetTerminal };
  __property TTerminalQueue * Queue = { read = FQueue, write = SetQueue };
  __property TColor SessionColor = { read = FSessionColor, write = SetSessionColor };
};
//---------------------------------------------------------------------------
#endif
