//---------------------------------------------------------------------------
#ifndef ScpCommanderH
#define ScpCommanderH
//---------------------------------------------------------------------------
#include "CustomScpExplorer.h"

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
#include <DirView.hpp>
#include <CustomPathComboBox.hpp>
#include <IEComboBox.hpp>
#include <IEPathComboBox.hpp>
#include <PathLabel.hpp>
#include <UnixPathComboBox.h>

#include <WinInterface.h>

#include "HistoryComboBox.hpp"
#include "CustomDriveView.hpp"
#include "DriveView.hpp"
#include "UnixDriveView.h"
#include "TB2Dock.hpp"
#include "TB2Item.hpp"
#include "TB2Toolbar.hpp"
#include "TBX.hpp"
#include <Menus.hpp>
#include "TB2ExtItems.hpp"
#include "TBXExtItems.hpp"
#include "TBXLists.hpp"
#include "TBXStatusBars.hpp"
//---------------------------------------------------------------------------
class TScpCommanderForm : public TCustomScpExplorerForm
{
__published:
  TSplitter *Splitter;
  TPanel *LocalPanel;
  TTBXStatusBar *LocalStatusBar;
  TDirView *LocalDirView;
  TPathLabel *LocalPathLabel;
  TPathLabel *RemotePathLabel;
  TTBXStatusBar *StatusBar;
  TPanel *CommandLinePanel;
  THistoryComboBox *CommandLineCombo;
  TPathLabel *CommandLineLabel;
  TLabel *CommandLinePromptLabel;
  TDriveView *LocalDriveView;
  TSplitter *LocalPanelSplitter;
  TTBXToolbar *SessionToolbar;
  TTBXItem *TBXItem123;
  TTBXSeparatorItem *TBXSeparatorItem34;
  TTBXComboBoxItem *SessionCombo;
  TTBXItem *TBXItem124;
  TTBXSeparatorItem *TBXSeparatorItem35;
  TTBXSubmenuItem *TBXSubmenuItem23;
  TTBXItem *TBXItem125;
  TTBXToolbar *PreferencesToolbar;
  TTBXItem *TBXItem126;
  TTBXSeparatorItem *TBXSeparatorItem36;
  TTBXItem *TBXItem127;
  TTBXSubmenuItem *TBXSubmenuItem24;
  TTBXItem *TBXItem128;
  TTBXItem *TBXItem129;
  TTBXItem *TBXItem130;
  TTBXToolbar *SelectionToolbar;
  TTBXItem *TBXItem131;
  TTBXItem *TBXItem132;
  TTBXSeparatorItem *TBXSeparatorItem37;
  TTBXItem *TBXItem133;
  TTBXItem *TBXItem134;
  TTBXItem *TBXItem135;
  TTBXToolbar *CommandToolbar;
  TTBXItem *TBXItem136;
  TTBXItem *TBXItem137;
  TTBXSeparatorItem *TBXSeparatorItem38;
  TTBXItem *TBXItem138;
  TTBXItem *TBXItem139;
  TTBXItem *TBXItem140;
  TTBXItem *TBXItem141;
  TTBXItem *TBXItem142;
  TTBXSeparatorItem *TBXSeparatorItem39;
  TTBXItem *TBXItem143;
  TTBXItem *TBXItem144;
  TTBXToolbar *SortToolbar;
  TTBXItem *TBXItem145;
  TTBXSeparatorItem *TBXSeparatorItem40;
  TTBXItem *TBXItem146;
  TTBXItem *TBXItem147;
  TTBXItem *TBXItem148;
  TTBXItem *TBXItem149;
  TTBXItem *TBXItem150;
  TTBXItem *TBXItem151;
  TTBXItem *TBXItem152;
  TTBXItem *TBXItem153;
  TTBXToolbar *CommandsToolbar;
  TTBXItem *TBXItem154;
  TTBXItem *TBXItem155;
  TTBXItem *TBXItem156;
  TTBXSeparatorItem *TBXSeparatorItem41;
  TTBXItem *TBXItem157;
  TTBXSeparatorItem *TBXSeparatorItem42;
  TTBXItem *TBXItem158;
  TTBXToolbar *MenuToolbar;
  TTBXSubmenuItem *LocalMenuButton;
  TTBXItem *TBXItem1;
  TTBXSeparatorItem *TBXSeparatorItem1;
  TTBXSubmenuItem *TBXSubmenuItem2;
  TTBXItem *TBXItem2;
  TTBXItem *TBXItem3;
  TTBXSeparatorItem *TBXSeparatorItem2;
  TTBXItem *TBXItem4;
  TTBXItem *TBXItem5;
  TTBXItem *TBXItem6;
  TTBXSeparatorItem *TBXSeparatorItem3;
  TTBXItem *TBXItem7;
  TTBXItem *TBXItem8;
  TTBXItem *TBXItem9;
  TTBXItem *TBXItem10;
  TTBXItem *TBXItem11;
  TTBXSeparatorItem *TBXSeparatorItem4;
  TTBXSubmenuItem *TBXSubmenuItem3;
  TTBXItem *TBXItem12;
  TTBXSeparatorItem *TBXSeparatorItem5;
  TTBXItem *TBXItem13;
  TTBXItem *TBXItem14;
  TTBXItem *TBXItem15;
  TTBXItem *TBXItem16;
  TTBXItem *TBXItem17;
  TTBXItem *TBXItem18;
  TTBXSubmenuItem *TBXSubmenuItem4;
  TTBXItem *TBXItem19;
  TTBXItem *TBXItem20;
  TTBXItem *TBXItem21;
  TTBXItem *TBXItem22;
  TTBXItem *TBXItem23;
  TTBXSubmenuItem *TBXSubmenuItem18;
  TTBXItem *TBXItem107;
  TTBXItem *TBXItem108;
  TTBXItem *TBXItem109;
  TTBXItem *TBXItem110;
  TTBXItem *TBXItem111;
  TTBXItem *TBXItem112;
  TTBXSubmenuItem *TBXSubmenuItem5;
  TTBXItem *TBXItem24;
  TTBXSeparatorItem *TBXSeparatorItem6;
  TTBXItem *TBXItem25;
  TTBXItem *TBXItem26;
  TTBXItem *TBXItem27;
  TTBXItem *TBXItem28;
  TTBXItem *TBXItem29;
  TTBXSeparatorItem *TBXSeparatorItem7;
  TTBXItem *TBXItem30;
  TTBXItem *TBXItem31;
  TTBXItem *TBXItem32;
  TTBXItem *TBXItem33;
  TTBXItem *TBXItem34;
  TTBXItem *TBXItem35;
  TTBXItem *TBXItem36;
  TTBXSeparatorItem *TBXSeparatorItem8;
  TTBXSubmenuItem *CustomCommandsMenu;
  TTBXSubmenuItem *TBXSubmenuItem6;
  TTBXItem *TBXItem37;
  TTBXItem *TBXItem38;
  TTBXItem *TBXItem39;
  TTBXItem *TBXItem40;
  TTBXSeparatorItem *TBXSeparatorItem9;
  TTBXItem *TBXItem41;
  TTBXSubmenuItem *TBXSubmenuItem7;
  TTBXItem *TBXItem42;
  TTBXItem *TBXItem43;
  TTBXItem *TBXItem44;
  TTBXItem *TBXItem45;
  TTBXSubmenuItem *TBXSubmenuItem8;
  TTBXItem *TBXItem46;
  TTBXSeparatorItem *TBXSeparatorItem10;
  TTBXItem *TBXItem47;
  TTBXItem *TBXItem48;
  TTBXItem *TBXItem49;
  TTBXSeparatorItem *TBXSeparatorItem11;
  TTBXItem *TBXItem50;
  TTBXItem *TBXItem51;
  TTBXSeparatorItem *TBXSeparatorItem12;
  TTBXItem *TBXItem52;
  TTBXItem *TBXItem53;
  TTBXSeparatorItem *TBXSeparatorItem13;
  TTBXItem *TBXItem54;
  TTBXItem *TBXItem55;
  TTBXSeparatorItem *TBXSeparatorItem14;
  TTBXItem *TBXItem56;
  TTBXItem *TBXItem57;
  TTBXSeparatorItem *TBXSeparatorItem15;
  TTBXItem *TBXItem58;
  TTBXSubmenuItem *TBXSubmenuItem19;
  TTBXItem *TBXItem113;
  TTBXSubmenuItem *TBXSubmenuItem20;
  TTBXSeparatorItem *TBXSeparatorItem29;
  TTBXSubmenuItem *TBXSubmenuItem21;
  TTBXItem *TBXItem114;
  TTBXItem *TBXItem115;
  TTBXSubmenuItem *TBXSubmenuItem9;
  TTBXSubmenuItem *TBXSubmenuItem10;
  TTBXItem *TBXItem59;
  TTBXItem *TBXItem60;
  TTBXItem *TBXItem61;
  TTBXItem *TBXItem62;
  TTBXItem *TBXItem63;
  TTBXItem *TBXItem64;
  TTBXSubmenuItem *TBXSubmenuItem11;
  TTBXItem *TBXItem65;
  TTBXItem *TBXItem66;
  TTBXSeparatorItem *TBXSeparatorItem16;
  TTBXItem *TBXItem67;
  TTBXSeparatorItem *TBXSeparatorItem17;
  TTBXItem *TBXItem68;
  TTBXSubmenuItem *TBXSubmenuItem12;
  TTBXItem *TBXItem69;
  TTBXItem *TBXItem70;
  TTBXSeparatorItem *TBXSeparatorItem18;
  TTBXItem *TBXItem71;
  TTBXSeparatorItem *TBXSeparatorItem19;
  TTBXItem *TBXItem72;
  TTBXSeparatorItem *TBXSeparatorItem20;
  TTBXItem *TBXItem73;
  TTBXItem *TBXItem74;
  TTBXItem *TBXItem75;
  TTBXItem *TBXItem76;
  TTBXSubmenuItem *TBXSubmenuItem14;
  TTBXItem *TBXItem77;
  TTBXItem *TBXItem78;
  TTBXItem *TBXItem79;
  TTBXSeparatorItem *TBXSeparatorItem21;
  TTBXItem *TBXItem80;
  TTBXSeparatorItem *TBXSeparatorItem22;
  TTBXItem *TBXItem81;
  TTBXSeparatorItem *TBXSeparatorItem23;
  TTBXItem *TBXItem82;
  TTBXSubmenuItem *RemoteMenuButton;
  TTBXItem *TBXItem83;
  TTBXSeparatorItem *TBXSeparatorItem24;
  TTBXSubmenuItem *TBXSubmenuItem15;
  TTBXItem *TBXItem84;
  TTBXSeparatorItem *TBXSeparatorItem25;
  TTBXItem *TBXItem85;
  TTBXItem *TBXItem86;
  TTBXItem *TBXItem87;
  TTBXSeparatorItem *TBXSeparatorItem26;
  TTBXItem *TBXItem88;
  TTBXItem *TBXItem89;
  TTBXItem *TBXItem90;
  TTBXItem *TBXItem91;
  TTBXItem *TBXItem92;
  TTBXSeparatorItem *TBXSeparatorItem27;
  TTBXSubmenuItem *TBXSubmenuItem16;
  TTBXItem *TBXItem93;
  TTBXSeparatorItem *TBXSeparatorItem28;
  TTBXItem *TBXItem94;
  TTBXItem *TBXItem95;
  TTBXItem *TBXItem96;
  TTBXItem *TBXItem97;
  TTBXItem *TBXItem98;
  TTBXItem *TBXItem99;
  TTBXItem *TBXItem100;
  TTBXSubmenuItem *TBXSubmenuItem17;
  TTBXItem *TBXItem101;
  TTBXItem *TBXItem102;
  TTBXItem *TBXItem103;
  TTBXItem *TBXItem104;
  TTBXItem *TBXItem105;
  TTBXItem *TBXItem106;
  TTBXSubmenuItem *TBXSubmenuItem22;
  TTBXItem *TBXItem116;
  TTBXSeparatorItem *TBXSeparatorItem30;
  TTBXItem *TBXItem117;
  TTBXItem *TBXItem118;
  TTBXItem *TBXItem119;
  TTBXSeparatorItem *TBXSeparatorItem31;
  TTBXItem *TBXItem120;
  TTBXSeparatorItem *TBXSeparatorItem32;
  TTBXItem *TBXItem121;
  TTBXSeparatorItem *TBXSeparatorItem33;
  TTBXItem *TBXItem122;
  TTBXDock *LocalTopDock;
  TTBXToolbar *LocalPathToolbar;
  TTBControlItem *TBControlItem1;
  TIEPathComboBox *LocalPathComboBox;
  TTBXToolbar *LocalHistoryToolbar;
  TTBXSubmenuItem *LocalBackButton;
  TTBXSubmenuItem *LocalForwardButton;
  TTBXToolbar *LocalNavigationToolbar;
  TTBXItem *TBXItem159;
  TTBXItem *TBXItem160;
  TTBXItem *TBXItem161;
  TTBXItem *TBXItem162;
  TTBXSeparatorItem *TBXSeparatorItem43;
  TTBXItem *TBXItem163;
  TTBXItem *TBXItem164;
  TTBXDock *LocalBottomDock;
  TTBXDock *RemoteTopDock;
  TTBXToolbar *RemotePathToolbar;
  TTBXToolbar *RemoteHistoryToolbar;
  TTBXSubmenuItem *RemoteBackButton;
  TTBXSubmenuItem *RemoteForwardButton;
  TTBXToolbar *RemoteNavigationToolbar;
  TTBXItem *TBXItem165;
  TTBXItem *TBXItem166;
  TTBXItem *TBXItem167;
  TTBXItem *TBXItem168;
  TTBXSeparatorItem *TBXSeparatorItem44;
  TTBXItem *TBXItem169;
  TTBXItem *TBXItem170;
  TTBControlItem *TBControlItem2;
  TUnixPathComboBox *RemotePathComboBox;
  TTBXDock *RemoteBottomDock;
  TTBXDock *BottomDock;
  TTBXToolbar *ToolbarToolbar;
  TTBXItem *TBXItem171;
  TTBXItem *TBXItem172;
  TTBXItem *TBXItem173;
  TTBXItem *TBXItem174;
  TTBXItem *TBXItem175;
  TTBXItem *TBXItem176;
  TTBXItem *TBXItem177;
  TTBXItem *TBXItem178;
  TTBXItem *TBXItem179;
  TTBXToolbar *UpdatesToolbar;
  TTBXSubmenuItem *TBXSubmenuItem1;
  TTBXItem *TBXItem180;
  TTBXItem *TBXItem181;
  TTBXItem *TBXItem182;
  TTBXSeparatorItem *TBXSeparatorItem45;
  TTBXItem *TBXItem183;
  TTBXItem *TBXItem184;
  TTBXSeparatorItem *TBXSeparatorItem46;
  TTBXItem *TBXItem185;
  TTBXItem *TBXItem186;
  TTBXItem *TBXItem187;
  TTBXToolbar *TransferToolbar;
  TTBXComboBoxItem *TransferCombo;
  TTBXItem *TBXItem188;
  TTBXItem *TBXItem189;
  TTBXItem *TBXItem190;
  void __fastcall SplitterMoved(TObject *Sender);
  void __fastcall SplitterCanResize(TObject *Sender, int &NewSize,
    bool &Accept);
  void __fastcall SplitterDblClick(TObject *Sender);
  void __fastcall PanelSplitterDblClick(TObject * Sender);
  void __fastcall PathComboBoxCloseUp(TObject *Sender,
    bool Canceled);
  void __fastcall LocalDirViewExecFile(TObject *Sender, TListItem *Item,
    bool &AllowExec);
  void __fastcall LocalFileControlDDDragEnter(TObject *Sender,
    IDataObject *DataObj, int grfKeyState, TPoint &Point,
    int &dwEffect, bool &Accept);
  void __fastcall LocalFileControlDDDragOver(TObject *Sender, int grfKeyState,
    TPoint &Point, int &dwEffect);
  void __fastcall LocalFileControlDDFileOperation(TObject *Sender,
    int dwEffect, AnsiString SourcePath, AnsiString TargetPath,
    bool &DoOperation);
  void __fastcall RemoteFileControlDDFileOperationExecuted(TObject *Sender,
    int dwEffect, AnsiString SourcePath, AnsiString TargetPath);
  void __fastcall CommandLineComboKeyDown(TObject *Sender, WORD &Key,
    TShiftState Shift);
  void __fastcall CommandLineComboDropDown(TObject *Sender);
  void __fastcall CommandLineComboEnter(TObject *Sender);
  void __fastcall CommandLineComboExit(TObject *Sender);
  void __fastcall LocalDirViewDDTargetHasDropHandler(TObject *Sender,
    TListItem *Item, int &Effect, bool &DropHandler);
  void __fastcall StatusBarDblClick(TObject *Sender);
  void __fastcall LocalFileControlDDMenuPopup(TObject *Sender, HMENU AMenu,
    IDataObject *DataObj, int AMinCustCmd, int grfKeyState, TPoint &pt);
  void __fastcall PathLabelDblClick(TObject *Sender);
  void __fastcall LocalDirViewEnter(TObject *Sender);
  void __fastcall LocalPathLabelGetStatus(TCustomPathLabel *Sender,
    bool &Active);
  void __fastcall RemotePathLabelGetStatus(TCustomPathLabel *Sender,
    bool &Active);
  void __fastcall LocalDriveViewEnter(TObject *Sender);
  void __fastcall LocalPathLabelPathClick(TCustomPathLabel *Sender,
    AnsiString Path);
  void __fastcall RemotePathLabelPathClick(TCustomPathLabel *Sender,
    AnsiString Path);
  void __fastcall LocalDirViewFileIconForName(TObject *Sender,
          TListItem *Item, AnsiString &FileName);
  void __fastcall LocalDirViewUpdateStatusBar(TObject *Sender,
          const TStatusFileInfo &FileInfo);
  void __fastcall RemoteDirViewUpdateStatusBar(TObject *Sender,
          const TStatusFileInfo &FileInfo);
  void __fastcall LocalStatusBarClick(TObject *Sender);

private:
  TCustomDirView * FDirViewToSelect;
  float FLastLeftPanelWidth;
  float FLeftPanelWidth;
  int FNormalPanelsWidth;
  int FLastWidth;
  bool FSynchronisingBrowse;
  TStrings * FInternalDDDownloadList;
  AnsiString FPrevPath[2];
  bool FFirstTerminal;
  AnsiString FDDExtTarget;
  bool FCommandLineComboPopulated;

  void __fastcall SetLeftPanelWidth(float value);
  float __fastcall GetLeftPanelWidth();
  inline TPanel * __fastcall Panel(bool Left);

protected:
  virtual bool __fastcall CopyParamDialog(TTransferDirection Direction,
    TTransferType Type, bool Temp, TStrings * FileList,
    AnsiString & TargetDirectory, TGUICopyParamType & CopyParam, bool Confirm);
  virtual TCustomDirView * __fastcall DirView(TOperationSide Side);
  TControl * __fastcall GetComponent(Byte Component);
  virtual void __fastcall RestoreFormParams();
  virtual void __fastcall RestoreParams();
  virtual void __fastcall FixControlsPlacement();
  virtual void __fastcall TerminalChanging();
  virtual void __fastcall TerminalChanged();
  virtual void __fastcall ConfigurationChanged();
  virtual bool __fastcall GetHasDirView(TOperationSide Side);
  virtual void __fastcall UpdateControls();
  virtual void __fastcall FileOperationProgress(
    TFileOperationProgressType & ProgressData, TCancelStatus & Cancel);
  virtual void __fastcall DoOpenDirectoryDialog(TOpenDirectoryMode Mode,
    TOperationSide Side);
  bool __fastcall InternalDDDownload(AnsiString & TargetDirectory);
  virtual void __fastcall DDGetTarget(AnsiString & Directory);
  virtual void __fastcall DDExtInitDrag(TFileList * FileList, bool & Created);
  virtual void __fastcall SideEnter(TOperationSide Side);
  void __fastcall SaveCommandLine();
  void __fastcall ExecuteCommandLine();
  virtual void __fastcall PanelExportStore(TOperationSide Side,
    TPanelExport Export, TPanelExportDestination Destination,
    TStringList * ExportData);
  void __fastcall CommandLinePopulate();
  virtual int __fastcall GetStaticComponentsHeight();
  DYNAMIC void __fastcall Resize();
  DYNAMIC void __fastcall DoShow();
  virtual void __fastcall SysResizing(unsigned int Cmd);
  virtual void __fastcall BatchStart(void *& Storage);
  virtual void __fastcall BatchEnd(void * Storage);
  virtual bool __fastcall IsFileControl(TObject * Control, TOperationSide Side);
  virtual void __fastcall ReloadLocalDirectory(const AnsiString Directory = "");
  virtual bool __fastcall PanelOperation(TOperationSide Side, bool DragDrop);
  virtual void __fastcall DoDirViewLoaded(TCustomDirView * Sender);
  virtual void __fastcall GetTransferPresetAutoSelectData(TCopyParamRuleData & Data);

public:
  __fastcall TScpCommanderForm(TComponent* Owner);
  virtual __fastcall ~TScpCommanderForm();
  
  virtual void __fastcall AddEditLink();
  virtual bool __fastcall AllowedAction(TAction * Action, TActionAllowed Allowed);
  virtual void __fastcall ChangePath(TOperationSide Side);
  virtual void __fastcall CompareDirectories();
  virtual void __fastcall UpdateSessionData(TSessionData * Data = NULL);
  virtual void __fastcall SynchronizeDirectories();
  virtual void __fastcall FullSynchronizeDirectories();
  virtual void __fastcall StoreParams();
  virtual void __fastcall ExploreLocalDirectory();
  virtual void __fastcall GoToCommandLine();
  virtual void __fastcall GoToTree();
  virtual void __fastcall OpenConsole(AnsiString Command = "");
  virtual AnsiString __fastcall PathForCaption();

  __property float LeftPanelWidth = { read = GetLeftPanelWidth, write = SetLeftPanelWidth };
};
//---------------------------------------------------------------------------
#endif
