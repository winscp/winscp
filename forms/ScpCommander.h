//---------------------------------------------------------------------------
#ifndef ScpCommanderH
#define ScpCommanderH
//---------------------------------------------------------------------------
#include "CustomScpExplorer.h"

#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <AssociatedStatusBar.hpp>
#include <CustomDirView.hpp>
#include <CustomUnixDirView.hpp>
#include <IEListView.hpp>
#include <NortonLikeListView.hpp>
#include <UnixDirView.h>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ToolWin.hpp>
#include <DirView.hpp>
#include <CustomPathComboBox.hpp>
#include <IEComboBox.hpp>
#include <IEPathComboBox.hpp>
#include <PathLabel.hpp>
#include <UnixPathComboBox.h>
#include <ToolbarPanel.hpp>

#include <WinInterface.h>

#include <Synchronize.h>
//---------------------------------------------------------------------------
class TScpCommanderForm : public TCustomScpExplorerForm
{
__published:
  TSplitter *Splitter;
  TPanel *LocalPanel;
  TAssociatedStatusBar *LocalStatusBar;
  TDirView *LocalDirView;
  TPathLabel *LocalPathLabel;
  TPathLabel *RemotePathLabel;
  TCoolBar *LocalCoolBar;
  TCoolBar *RemoteCoolBar;
  TToolbarPanel *ToolbarPanel;
  TStatusBar *StatusBar;
  TToolBar *MenuToolBar;
  TToolButton *ToolButton2;
  TToolButton *ToolButton19;
  TToolButton *ToolButton1;
  TToolButton *ToolButton3;
  TToolButton *ToolButton4;
  TToolBar *SelectionToolbar;
  TToolButton *ToolButton23;
  TToolButton *ToolButton24;
  TToolButton *ToolButton25;
  TToolButton *ToolButton26;
  TToolButton *ToolButton28;
  TToolButton *ToolButton29;
  TToolBar *PreferencesToolbar;
  TToolButton *ToolButton33;
  TToolButton *ToolButton35;
  TToolButton *ToolButton36;
  TToolBar *SessionToolbar;
  TToolButton *ToolButton30;
  TToolButton *ToolButton31;
  TToolButton *ToolButton32;
  TToolButton *SavedSessionsButton;
  TToolBar *CommandToolBar;
  TToolButton *ToolButton5;
  TToolButton *ToolButton6;
  TToolButton *ToolButton17;
  TToolButton *ToolButton27;
  TToolButton *ToolButton34;
  TToolButton *ToolButton16;
  TToolButton *ToolButton37;
  TToolButton *ToolButton38;
  TToolBar *ToolBar1;
  TToolButton *LocalBackButton;
  TToolButton *LocalForwardButton;
  TToolBar *ToolBar2;
  TToolButton *ToolButton57;
  TToolButton *ToolButton58;
  TToolButton *ToolButton59;
  TToolButton *ToolButton60;
  TToolButton *ToolButton61;
  TToolButton *ToolButton62;
  TToolBar *ToolBar3;
  TToolButton *RemoteBackButton;
  TToolButton *RemoteForwardButton;
  TToolBar *ToolBar4;
  TToolButton *ToolButton87;
  TToolButton *ToolButton88;
  TToolButton *ToolButton89;
  TToolButton *ToolButton90;
  TToolButton *ToolButton91;
  TToolButton *ToolButton92;
  TToolButton *ToolButton7;
  TToolBar *SortToolbar;
  TToolButton *ToolButton8;
  TToolButton *ToolButton9;
  TToolButton *ToolButton10;
  TToolButton *ToolButton11;
  TToolButton *ToolButton13;
  TToolButton *ToolButton15;
  TToolButton *ToolButton12;
  TToolButton *ToolButton14;
  TToolButton *ToolButton18;
  TToolButton *ToolButton20;
  TToolButton *ToolButton21;
  TToolButton *ToolButton22;
  TToolBar *CommandsToolbar;
  TToolButton *ToolButton39;
  TToolButton *ToolButton40;
  TToolButton *ToolButton41;
  TToolBar *ToolBar5;
  TIEPathComboBox *LocalPathComboBox;
  TToolBar *ToolBar6;
  TUnixPathComboBox *RemotePathComboBox;
  TToolButton *ToolButton42;
  TToolButton *ToolButton43;
  TToolButton *ToolButton44;
  TToolButton *ToolButton45;
  TToolButton *ToolButton46;
  TToolButton *ToolButton47;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall SplitterMoved(TObject *Sender);
  void __fastcall SplitterCanResize(TObject *Sender, int &NewSize,
    bool &Accept);
  void __fastcall SplitterDblClick(TObject *Sender);
  void __fastcall PathComboBoxCloseUp(TObject *Sender,
    bool Canceled);
  void __fastcall FormResize(TObject *Sender);
  void __fastcall LocalDirViewChangeDetected(TObject *Sender);
  void __fastcall LocalDirViewExecFile(TObject *Sender, TListItem *Item,
          bool &AllowExec);
  void __fastcall LocalDirViewDDDragEnter(TObject *Sender,
          IDataObject *DataObj, int grfKeyState, TPoint &Point,
          int &dwEffect, bool &Accept);
  void __fastcall DirViewLoaded(TObject *Sender);
private:
  TCustomDirView * FDirViewToSelect;
  float FLastLocalPanelWidth;
  float FLocalPanelWidth;
  int FLastWidth;
  bool FSynchronisingBrowse;
  TSynchronizationStatus FSynchronization;
  TSynchronizeParamType FSynchronizeParams;
  TSynchronizeDialog * FSynchronizeDialog;
  AnsiString FPrevPath[2];
  void __fastcall SetLocalPanelWidth(float value);
  float __fastcall GetLocalPanelWidth();
protected:
  virtual Boolean __fastcall CopyParamDialog(TTransferDirection Direction,
    TTransferType Type, Boolean DragDrop, TStrings * FileList,
    AnsiString & TargetDirectory, TCopyParamType & CopyParam, Boolean Confirm);
  virtual TCustomDirView * __fastcall DirView(TOperationSide Side);
  virtual void __fastcall ExecuteFileOperation(::TFileOperation Operation, TOperationSide Side, Boolean OnFocused);
  TControl * __fastcall GetComponent(Byte Component);
  virtual void __fastcall RestoreFormParams();
  virtual void __fastcall RestoreParams();
  virtual void __fastcall SetComponentVisible(Word Component, Boolean value);
  virtual void __fastcall TerminalChanged();
  virtual void __fastcall ConfigurationChanged();
  //virtual void __fastcall DoDirViewExecFile(TObject * Sender, TListItem * Item, bool & AllowExec);
  virtual Boolean __fastcall GetHasDirView(TOperationSide Side);
  DYNAMIC void __fastcall KeyDown(Word & Key, Classes::TShiftState Shift);
  void __fastcall UpdateControls();
  void __fastcall SynchronizeStartStop(System::TObject* Sender, Boolean Start, TSynchronizeParamType Params);
  void __fastcall SynchronizeNow();
  virtual void __fastcall DoOperationFinished(TOperationSide Side, Boolean DragDrop,
    const AnsiString FileName, Boolean Success, Boolean & DisconnectWhenFinished);
  virtual void __fastcall SaveSessionData(TSessionData * aSessionData);
  virtual void __fastcall FileOperationProgress(
    TFileOperationProgressType & ProgressData, TCancelStatus & Cancel);
public:
  virtual void __fastcall AddEditLink();
  __fastcall TScpCommanderForm(TComponent* Owner);
  virtual Boolean __fastcall AllowedAction(TAction * Action, TActionAllowed Allowed);
  virtual void __fastcall ChangePath(TOperationSide Side);
  virtual void __fastcall CompareDirectories();
  virtual void __fastcall UpdateSessionData(TSessionData * Data = NULL);
  virtual void __fastcall SynchronizeDirectories();
  virtual void __fastcall StoreParams();
  virtual void __fastcall ExploreLocalDirectory();
  __property float LocalPanelWidth = { read = GetLocalPanelWidth, write = SetLocalPanelWidth };
};
//---------------------------------------------------------------------------
#endif
