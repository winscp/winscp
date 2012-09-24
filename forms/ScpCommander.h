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
#include <PathLabel.hpp>

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
#include "TBXToolPals.hpp"
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
  TDriveView *LocalDriveView;
  TSplitter *LocalPanelSplitter;
  TTBXToolbar *SessionToolbar;
  TTBXItem *TBXItem123;
  TTBXSeparatorItem *TBXSeparatorItem34;
  TTBXItem *TBXItem124;
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
  TTBXSeparatorItem *TBXSeparatorItem6;
  TTBXItem *TBXItem25;
  TTBXItem *TBXItem26;
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
  TTBXSubmenuItem *QueueSubmenuItem;
  TTBXItem *TBXItem46;
  TTBXItem *QueueEnableItem2;
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
  TTBXToolbar *LocalHistoryToolbar;
  TTBXSubmenuItem *LocalBackButton;
  TTBXSubmenuItem *LocalForwardButton;
  TTBXToolbar *LocalNavigationToolbar;
  TTBXItem *TBXItem159;
  TTBXItem *TBXItem160;
  TTBXItem *TBXItem161;
  TTBXItem *TBXItem162;
  TTBXSeparatorItem *TBXSeparatorItem43;
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
  TTBXItem *TBXItem170;
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
  TTBXItem *TBXItem188;
  TTBXItem *TBXItem190;
  TTBXSeparatorItem *TBXSeparatorItem47;
  TTBXItem *TBXItem191;
  TTBXItem *TBXItem192;
  TTBXItem *TBXItem193;
  TTBXItem *TBXItem196;
  TTBXItem *TBXItem197;
  TTBXSeparatorItem *TBXSeparatorItem48;
  TTBXSubmenuItem *TBXSubmenuItem13;
  TTBXItem *TBXItem198;
  TTBXItem *TBXItem199;
  TTBXSubmenuItem *TBXSubmenuItem25;
  TTBXItem *TBXItem27;
  TTBXItem *TBXItem200;
  TTBXSubmenuItem *TBXSubmenuItem26;
  TTBXItem *TBXItem24;
  TTBXItem *TBXItem209;
  TTBXItem *TBXItem28;
  TTBXToolbar *UploadDownloadToolbar;
  TTBXItem *TBXItem212;
  TTBXItem *TBXItem213;
  TTBXItem *TBXItem214;
  TTBXToolbar *CustomCommandsToolbar;
  TTBXItem *TBXItem215;
  TTBXSeparatorItem *TBXSeparatorItem49;
  TTBXColorItem *ColorMenuItem;
  TTBXSeparatorItem *TBXSeparatorItem50;
  TTBXSeparatorItem *TBXSeparatorItem51;
  TTBXColorPalette *SessionColorPalette;
  TTBXDropDownItem *TransferDropDown;
  TTBXStringList *TransferList;
  TTBXLabelItem *TransferLabel;
  TTBXSeparatorItem *TBXSeparatorItem52;
  TTBXItem *TBXItem189;
  TTBXItem *TBXItem218;
  TTBXItem *TBXItem219;
  TTBXComboBoxItem *RemotePathComboBox;
  TTBXComboBoxItem *LocalPathComboBox;
  TTBXToolbar *CommandLineToolbar;
  TTBXComboBoxItem *CommandLineCombo;
  TTBXLabelItem *CommandLinePromptLabel;
  TTBXItem *TBXItem163;
  TTBXItem *TBXItem169;
  TTBXComboBoxItem *QueueSpeedComboBoxItem;
  TTBXItem *TBXItem220;
  TTBXItem *TBXItem221;
  TTBXSubmenuItem *TBXSubmenuItem8;
  TTBXItem *TBXItem222;
  TTBXItem *TBXItem223;
  TTBXItem *TBXItem224;
  TTBXItem *TBXItem210;
  TTBXItem *TBXItem227;
  TTBXItem *TBXItem228;
  TTBXItem *TBXItem229;
  void __fastcall SplitterMoved(TObject *Sender);
  void __fastcall SplitterCanResize(TObject *Sender, int &NewSize,
    bool &Accept);
  void __fastcall SplitterDblClick(TObject *Sender);
  void __fastcall PanelSplitterDblClick(TObject * Sender);
  void __fastcall LocalDirViewExecFile(TObject *Sender, TListItem *Item,
    bool &AllowExec);
  void __fastcall LocalFileControlDDDragEnter(TObject *Sender,
    IDataObject *DataObj, int grfKeyState, TPoint &Point,
    int &dwEffect, bool &Accept);
  void __fastcall LocalFileControlDDDragOver(TObject *Sender, int grfKeyState,
    TPoint &Point, int &dwEffect);
  void __fastcall LocalFileControlDDFileOperation(TObject *Sender,
    int dwEffect, UnicodeString SourcePath, UnicodeString TargetPath,
    bool &DoOperation);
  void __fastcall RemoteFileControlDDFileOperationExecuted(TObject *Sender,
    int dwEffect, UnicodeString SourcePath, UnicodeString TargetPath);
  void __fastcall LocalDirViewDDTargetHasDropHandler(TObject *Sender,
    TListItem *Item, int &Effect, bool &DropHandler);
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
    UnicodeString Path);
  void __fastcall RemotePathLabelPathClick(TCustomPathLabel *Sender,
    UnicodeString Path);
  void __fastcall LocalDirViewFileIconForName(TObject *Sender,
          TListItem *Item, UnicodeString &FileName);
  void __fastcall LocalDirViewUpdateStatusBar(TObject *Sender,
          const TStatusFileInfo &FileInfo);
  void __fastcall RemoteDirViewUpdateStatusBar(TObject *Sender,
          const TStatusFileInfo &FileInfo);
  void __fastcall LocalStatusBarClick(TObject *Sender);
  void __fastcall RemoteDirViewPathChange(TCustomDirView *Sender);
  void __fastcall LocalDirViewPathChange(TCustomDirView *Sender);
  void __fastcall LocalPathComboBoxCancel(TObject *Sender);
  void __fastcall LocalPathComboBoxAdjustImageIndex(
    TTBXComboBoxItem * Sender, const UnicodeString AText, int AIndex, int & ImageIndex);
  void __fastcall LocalPathComboBoxItemClick(TObject * Sender);
  void __fastcall CommandLineComboPopup(TTBCustomItem *Sender,
          bool FromLink);
  void __fastcall CommandLineComboBeginEdit(TTBEditItem *Sender,
          TTBEditItemViewer *Viewer, TEdit *EditControl);
  void __fastcall LocalDriveViewRefreshDrives(TObject *Sender);
  void __fastcall QueueSubmenuItemPopup(TTBCustomItem *Sender,
          bool FromLink);
  void __fastcall DirViewHistoryGo(TCustomDirView *Sender, int Index,
          bool &Cancel);

private:
  bool FConstructed;
  double FLastLeftPanelWidth;
  double FLeftPanelWidth;
  int FNormalPanelsWidth;
  int FLastWidth;
  bool FSynchronisingBrowse;
  TStrings * FInternalDDDownloadList;
  UnicodeString FPrevPath[2];
  bool FFirstTerminal;
  UnicodeString FDDExtTarget;
  bool FCommandLineComboPopulated;
  TStrings* FLocalPathComboBoxPaths;
  unsigned int FSpecialFolders;
  TEdit * FCommandLineComboEdit;
  TWndMethod FToolbarEditOldWndProc;
  bool FPanelsRestored;

  void __fastcall SetLeftPanelWidth(double value);
  double __fastcall GetLeftPanelWidth();
  inline TPanel * __fastcall Panel(bool Left);
  TPanel * __fastcall CurrentPanel();
  void __fastcall CommandLineComboEditWndProc(TMessage & Message);
  void __fastcall ExitToolbar();

protected:
  virtual bool __fastcall CopyParamDialog(TTransferDirection Direction,
    TTransferType Type, bool Temp, TStrings * FileList,
    UnicodeString & TargetDirectory, TGUICopyParamType & CopyParam, bool Confirm,
    bool DragDrop);
  virtual TCustomDirView * __fastcall DirView(TOperationSide Side);
  TControl * __fastcall GetComponent(Byte Component);
  virtual void __fastcall RestoreFormParams();
  virtual void __fastcall RestoreParams();
  virtual void __fastcall FixControlsPlacement();
  virtual void __fastcall TerminalChanged();
  virtual void __fastcall ConfigurationChanged();
  virtual bool __fastcall GetHasDirView(TOperationSide Side);
  virtual void __fastcall UpdateControls();
  virtual void __fastcall FileOperationProgress(
    TFileOperationProgressType & ProgressData, TCancelStatus & Cancel);
  virtual void __fastcall DoOpenDirectoryDialog(TOpenDirectoryMode Mode,
    TOperationSide Side);
  bool __fastcall InternalDDDownload(UnicodeString & TargetDirectory);
  virtual bool __fastcall DDGetTarget(UnicodeString & Directory, bool & Internal);
  virtual void __fastcall DDExtInitDrag(TFileList * FileList, bool & Created);
  virtual void __fastcall SideEnter(TOperationSide Side);
  void __fastcall SaveCommandLine();
  bool __fastcall ExecuteCommandLine();
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
  virtual void __fastcall ReloadLocalDirectory(const UnicodeString Directory = L"");
  virtual bool __fastcall PanelOperation(TOperationSide Side, bool DragDrop);
  virtual void __fastcall DoDirViewLoaded(TCustomDirView * Sender);
  virtual void __fastcall GetTransferPresetAutoSelectData(TCopyParamRuleData & Data);
  virtual void __fastcall UpdateSessionData(TSessionData * Data);
  void __fastcall SynchronizeBrowsing(TCustomDirView * ADirView);
  void __fastcall SynchronizeBrowsing(TCustomDirView * ADirView, UnicodeString PrevPath,
    UnicodeString & NewPath, bool Create);
  void __fastcall SynchronizeBrowsingLocal(UnicodeString PrevPath, UnicodeString & NewPath, bool Create);
  void __fastcall SynchronizeBrowsingRemote(UnicodeString PrevPath, UnicodeString & NewPath, bool Create);
  void __fastcall LocalPathComboUpdateDrives();
  void __fastcall LocalPathComboUpdate();
  virtual void __fastcall ToolbarItemResize(TTBXCustomDropDownItem * Item, int Width);
  void __fastcall DoOpenBookmark(UnicodeString Local, UnicodeString Remote);
  virtual bool __fastcall OpenBookmark(UnicodeString Local, UnicodeString Remote);
  virtual void __fastcall DoFocusRemotePath(UnicodeString Path);
  UnicodeString __fastcall ChangeFilePath(UnicodeString Path, TOperationSide Side);

public:
  __fastcall TScpCommanderForm(TComponent* Owner);
  virtual __fastcall ~TScpCommanderForm();

  virtual void __fastcall AddEditLink(bool Add);
  virtual bool __fastcall AllowedAction(TAction * Action, TActionAllowed Allowed);
  virtual void __fastcall ChangePath(TOperationSide Side);
  virtual void __fastcall CompareDirectories();
  virtual void __fastcall UpdateTerminal(TTerminal * Terminal);
  virtual void __fastcall SynchronizeDirectories();
  virtual void __fastcall FullSynchronizeDirectories();
  virtual void __fastcall StoreParams();
  virtual void __fastcall ExploreLocalDirectory();
  virtual void __fastcall GoToCommandLine();
  virtual void __fastcall GoToTree();
  virtual void __fastcall OpenConsole(UnicodeString Command = L"");
  virtual UnicodeString __fastcall PathForCaption();
  virtual void __fastcall BeforeAction();
  virtual void __fastcall HomeDirectory(TOperationSide Side);
  virtual void __fastcall HistoryGo(TOperationSide Side, int Index);

  __property double LeftPanelWidth = { read = GetLeftPanelWidth, write = SetLeftPanelWidth };
};
//---------------------------------------------------------------------------
#endif
