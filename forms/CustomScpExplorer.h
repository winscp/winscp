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
#include <AssociatedStatusBar.hpp>
#include <ToolWin.hpp>

#include <WinInterface.h>
//---------------------------------------------------------------------------
class TProgressForm;
//---------------------------------------------------------------------------
enum TActionAllowed { aaShortCut, aaUpdate, aaExecute };
enum TActionFlag { afLocal = 1, afRemote = 2, afExplorer = 4 , afCommander = 8 };
enum TExecuteFileBy { efDefault, efEditor, efAlternativeEditor };
//---------------------------------------------------------------------------
class TCustomScpExplorerForm : public TForm
{
__published:
  TPanel *RemotePanel;
  TAssociatedStatusBar *RemoteStatusBar;
  TUnixDirView *RemoteDirView;
  TCoolBar *TopCoolBar;
  void __fastcall RemoteDirViewGetCopyParam(TUnixDirView *Sender,
    TTransferDirection Direction, TTransferType Type,
    AnsiString &TargetDirectory, TStrings *FileList,
    TCopyParamType &CopyParam);
  void __fastcall RemoteDirViewContextPopup(TObject *Sender,
    const TPoint &MousePos, bool &Handled);
  void __fastcall DirViewEnter(TObject *Sender);
  void __fastcall RemoteDirViewGetSelectFilter(
    TCustomDirView *Sender, bool Select, TFileFilter &Filter);
  void __fastcall SessionStatusBarDrawPanel(TStatusBar *StatusBar,
    TStatusPanel *Panel, const TRect &Rect);
  void __fastcall SessionStatusBarMouseMove(TObject *Sender,
    TShiftState Shift, int X, int Y);
  void __fastcall ApplicationHint(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall DropDownButtonMenu(TObject *Sender);
  void __fastcall RemoteDirViewDisplayProperties(TObject *Sender);
  void __fastcall DirViewColumnRightClick(TObject *Sender,
    TListColumn *Column, TPoint &Point);
  void __fastcall DirViewExecFile(TObject *Sender, TListItem *Item, bool &AllowExec);
  void __fastcall ToolBarResize(TObject *Sender);
  void __fastcall RemoteDirViewWarnLackOfTempSpace(TUnixDirView *Sender,
    const AnsiString Path, __int64 RequiredSpace, bool &Continue);
  void __fastcall DirViewDDDragEnter(TObject *Sender,
    _di_IDataObject DataObj, int grfKeyState, const TPoint &Point,
    int &dwEffect, bool &Accept);
  void __fastcall DirViewDDDragLeave(TObject *Sender);
private:
  TTerminal * FTerminal;
  bool FFormRestored;
  bool FAutoOperation;
  AnsiString FExecutedFile;
  int FExecutedFileTimestamp;
  TExecuteFileBy FFileExecutedBy;
  bool FForceExecution;

  Boolean __fastcall GetEnableFocusedOperation(TOperationSide Side);
  Boolean __fastcall GetEnableSelectedOperation(TOperationSide Side);
  void __fastcall SetTerminal(TTerminal * value);
protected:
  TCustomDirView * FLastDirView;
  TCustomDirView * FDDTargetDirView;
  TProgressForm * FProgressForm;

  virtual bool __fastcall CopyParamDialog(TTransferDirection Direction,
    TTransferType Type, bool DragDrop, TStrings * FileList,
    AnsiString & TargetDirectory, TCopyParamType & CopyParam, bool Confirm);
  virtual void __fastcall CreateParams(TCreateParams & Params);
  void __fastcall DeleteFiles(TOperationSide Side, TStrings * FileList);
  virtual void __fastcall DoDirViewExecFile(TObject * Sender, TListItem * Item, bool & AllowExec);
  virtual TControl * __fastcall GetComponent(Byte Component);
  bool __fastcall GetComponentVisible(Word Component);
  virtual Boolean __fastcall GetHasDirView(TOperationSide Side);
  DYNAMIC void __fastcall KeyDown(Word & Key, Classes::TShiftState Shift);
  void __fastcall OperationFinished(TOperationSide Side, bool DragDrop,
    const AnsiString FileName, bool Success, bool & DisconnectWhenFinished);
  void __fastcall OperationProgress(TFileOperationProgressType & ProgressData, TCancelStatus & Cancel);
  virtual void __fastcall RestoreFormParams();
  virtual void __fastcall RestoreParams();
  virtual void __fastcall SetComponentVisible(Word Component, bool value);
  void __fastcall SetProperties(TOperationSide Side, TStrings * FileList);
  virtual void __fastcall TerminalChanged();
  void __fastcall UpdateStatusBar();
  virtual void __fastcall DoOperationFinished(TOperationSide Side, bool DragDrop,
    const AnsiString FileName, bool Success, bool & DisconnectWhenFinished);
  void __fastcall DoOpenDirectoryDialog(TOpenDirectoryMode Mode, TOperationSide Side,
    const AnsiString Bookmark = "");
  virtual void __fastcall SaveSessionData(TSessionData * aSessionData);
  virtual void __fastcall FileOperationProgress(
    TFileOperationProgressType & ProgressData, TCancelStatus & Cancel);
  void __fastcall ExecutedFileChanged(TObject * Sender);
public:
  virtual __fastcall ~TCustomScpExplorerForm();
  void __fastcall AddBookmark(TOperationSide Side);
  virtual void __fastcall AddEditLink();
  virtual Boolean __fastcall AllowedAction(TAction * Action, TActionAllowed Allowed) = 0;
  virtual void __fastcall ConfigurationChanged();
  void __fastcall CreateDirectory(TOperationSide Side);
  void __fastcall ExecuteFileOperation(TFileOperation Operation, TOperationSide Side,
    bool OnFocused, bool NoConfirmation = False);
  virtual TCustomDirView * __fastcall DirView(TOperationSide Side);
  virtual void __fastcall ChangePath(TOperationSide Side) = 0;
  virtual void __fastcall StoreParams();
  void __fastcall NewSession();
  void __fastcall OpenDirectory(TOperationSide Side);
  void __fastcall OpenStoredSession(TSessionData * Data);
  void __fastcall SessionIdle();
  __fastcall TCustomScpExplorerForm(TComponent* Owner);
  void __fastcall SaveCurrentSession();
  virtual void __fastcall CompareDirectories();
  void __fastcall ExecuteCurrentFile();
  void __fastcall OpenConsole();
  virtual void __fastcall UpdateSessionData(TSessionData * Data = NULL);
  virtual void __fastcall SynchronizeDirectories();
  virtual void __fastcall ExploreLocalDirectory();
  void __fastcall ExecuteFile(TOperationSide Side, TExecuteFileBy ExecuteFileBy);
  __property bool ComponentVisible[Word Component] = { read = GetComponentVisible, write = SetComponentVisible };
  __property bool EnableFocusedOperation[TOperationSide Side] = { read = GetEnableFocusedOperation };
  __property bool EnableSelectedOperation[TOperationSide Side] = { read = GetEnableSelectedOperation };
  __property bool HasDirView[TOperationSide Side] = { read = GetHasDirView };
  __property TTerminal * Terminal = { read = FTerminal, write = SetTerminal };
};
//---------------------------------------------------------------------------
#endif
