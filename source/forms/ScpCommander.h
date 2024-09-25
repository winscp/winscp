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
#include "ThemePageControl.h"
#include <Vcl.AppEvnts.hpp>
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
  TTBXToolbar *SessionToolbar2;
  TTBXSeparatorItem *TBXSeparatorItem34;
  TTBXItem *TBXItem124;
  TTBXSubmenuItem *TBXSubmenuItem23;
  TTBXItem *TBXItem125;
  TTBXToolbar *PreferencesToolbar;
  TTBXItem *TBXItem126;
  TTBXSeparatorItem *TBXSeparatorItem36;
  TTBXSubmenuItem *TBXSubmenuItem24;
  TTBXItem *TBXItem128;
  TTBXItem *TBXItem129;
  TTBXItem *TBXItem130;
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
  TTBXSubmenuItem *LocalColumnsSubmenuItem;
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
  TTBXSubmenuItem *TBXItem26;
  TTBXItem *TBXItem29;
  TTBXSeparatorItem *TBXSeparatorItem7;
  TTBXSubmenuItem *CurrentCopyItem;
  TTBXItem *CurrentCopyToItem;
  TTBXItem *CurrentMoveItem;
  TTBXItem *CurrentMoveToItem;
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
  TTBXItem *TBXItem57;
  TTBXSeparatorItem *TBXSeparatorItem15;
  TTBXItem *TBXItem58;
  TTBXSubmenuItem *TBXSubmenuItem20;
  TTBXSeparatorItem *TBXSeparatorItem29;
  TTBXSubmenuItem *TBXSubmenuItem21;
  TTBXItem *TBXItem114;
  TTBXItem *TBXItem115;
  TTBXSubmenuItem *TBXSubmenuItem9;
  TTBXSubmenuItem *TBXSubmenuItem10;
  TTBXItem *TBXItem60;
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
  TTBXSubmenuItem *RemoteColumnsSubmenuItem;
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
  TTBXToolbar *Toolbar2Toolbar;
  TTBXItem *TBXItem171;
  TTBXItem *TBXItem172;
  TTBXItem *CurrentCopyToolbar2Item;
  TTBXItem *CurrentMoveToolbar2Item;
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
  TTBXItem *TBXItem27;
  TTBXSubmenuItem *TBXSubmenuItem26;
  TTBXItem *TBXItem24;
  TTBXItem *TBXItem209;
  TTBXItem *TBXItem28;
  TTBXToolbar *CustomCommandsToolbar;
  TTBXItem *TBXItem215;
  TTBXSeparatorItem *TBXSeparatorItem49;
  TTBXColorItem *ColorMenuItem;
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
  TTBXSubmenuItem *LocalOpenDirButton;
  TTBXSubmenuItem *RemoteOpenDirButton;
  TTBXComboBoxItem *QueueSpeedComboBoxItem;
  TTBXItem *TBXItem220;
  TTBXItem *TBXItem221;
  TTBXSubmenuItem *TBXSubmenuItem8;
  TTBXItem *TBXItem222;
  TTBXItem *TBXItem223;
  TTBXItem *TBXItem224;
  TTBXItem *TBXItem210;
  TTBXSubmenuItem *TBXItem228;
  TTBXSubmenuItem *TBXItem229;
  TTBXItem *TBXItem230;
  TTBXSubmenuItem *TBXSubmenuItem231;
  TTBXToolbar *LocalFileToolbar;
  TTBXSubmenuItem *LocalCopyItem;
  TTBXItem *LocalMoveItem;
  TTBXItem *TBXItem233;
  TTBXItem *TBXItem234;
  TTBXSubmenuItem *TBXItem235;
  TTBXSeparatorItem *TBXSeparatorItem35;
  TTBXSubmenuItem *TBXItem236;
  TTBXSeparatorItem *TBXSeparatorItem54;
  TTBXToolbar *RemoteFileToolbar;
  TTBXSubmenuItem *RemoteCopyItem;
  TTBXItem *RemoteMoveItem;
  TTBXSeparatorItem *TBXSeparatorItem55;
  TTBXItem *TBXItem240;
  TTBXItem *TBXItem241;
  TTBXSubmenuItem *TBXItem242;
  TTBXSubmenuItem *TBXItem243;
  TTBXSeparatorItem *TBXSeparatorItem56;
  TTBXItem *TBXItem59;
  TTBXItem *TBXItem136;
  TTBXLabelItem *TransferSettingsLabelItem;
  TTBXToolbar *LocalSelectionToolbar;
  TTBXItem *TBXItem30;
  TTBXItem *TBXItem32;
  TTBXItem *TBXItem137;
  TTBXToolbar *RemoteSelectionToolbar;
  TTBXItem *TBXItem138;
  TTBXItem *TBXItem139;
  TTBXItem *TBXItem140;
  TTBXItem *TBXItem61;
  TTBXItem *TBXItem131;
  TTBXSeparatorItem *TBXSeparatorItem37;
  TTBXItem *TBXItem132;
  TTBXItem *TBXItem133;
  TTBXSeparatorItem *TBXSeparatorItem38;
  TPanel *QueueSeparatorPanel;
  TTBXSeparatorItem *TBXSeparatorItem39;
  TTBXItem *TBXItem134;
  TTBXItem *TBXItem56;
  TTBXSeparatorItem *TBXSeparatorItem50;
  TTBXItem *TBXItem135;
  TTBXItem *TBXItem141;
  TTBXItem *TBXItem142;
  TTBXItem *TBXItem143;
  TTBXItem *TBXItem144;
  TTBXItem *CurrentCopyNonQueueItem;
  TTBXItem *CurrentCopyQueueItem;
  TTBXSeparatorItem *TBXSeparatorItem51;
  TTBXItem *TBXItem174;
  TTBXSeparatorItem *TBXSeparatorItem58;
  TTBXItem *TBXItem200;
  TTBXSeparatorItem *TBXSeparatorItem59;
  TTBXSeparatorItem *TBXSeparatorItem60;
  TTBXSeparatorItem *TBXSeparatorItem61;
  TTBXItem *TBXItem212;
  TTBXItem *TBXItem213;
  TTBXSubmenuItem *TBXSubmenuItem25;
  TTBXItem *TBXItem214;
  TTBXItem *TBXItem216;
  TTBXItem *TBXItem217;
  TTBXSubmenuItem *TBXSubmenuItem28;
  TTBXItem *TBXItem227;
  TTBXSubmenuItem *RemoteNewSubmenuItem;
  TTBXItem *TBXItem244;
  TTBXItem *TBXItem246;
  TTBXItem *TBXItem247;
  TTBXSubmenuItem *LocalNewSubmenuItem;
  TTBXItem *TBXItem248;
  TTBXItem *TBXItem249;
  TTBXItem *TBXItem250;
  TTBXItem *TBXItem76;
  TTBXItem *TBXItem127;
  TTBXSeparatorItem *TBXSeparatorItem62;
  TTBXItem *TBXItem163;
  TTBXItem *TBXItem169;
  TTBXSeparatorItem *TBXSeparatorItem63;
  TTBXItem *TBXItem237;
  TTBXItem *TBXItem245;
  TTBXSeparatorItem *TBXSeparatorItem64;
  TTBXItem *TBXItem251;
  TTBXItem *TBXItem252;
  TTBXItem *TBXItem253;
  TTBXItem *TBXItem255;
  TTBXSeparatorItem *TBXSeparatorItem65;
  TTBXItem *TBXItem256;
  TDriveView *OtherLocalDriveView;
  TDirView *OtherLocalDirView;
  TTBXItem *TBXItem257;
  TTBXSubmenuItem *TBXSubmenuItem29;
  TTBXSeparatorItem *TBXSeparatorItem53;
  TTBXSeparatorItem *TBXSeparatorItem66;
  TTBXItem *TBXItem31;
  TTBXSubmenuItem *TBXSubmenuItem30;
  TTBXItem *TBXItem33;
  TTBXSubmenuItem *TBXSubmenuItem31;
  TTBXItem *TBXItem123;
  TTBXItem *TBXItem231;
  TTBXSeparatorItem *TBXSeparatorItem67;
  TTBXItem *TBXItem232;
  TTBXSeparatorItem *TBXSeparatorItem68;
  TTBXItem *TBXItem238;
  TTBXItem *TBXItem239;
  TTBXSeparatorItem *TBXSeparatorItem69;
  TTBXItem *TBXItem113;
  TTBXItem *TBXItem258;
  TTBXSeparatorItem *TBXSeparatorItem70;
  TTBXItem *TBXItem259;
  TTBXSeparatorItem *TBXSeparatorItem71;
  TTBXItem *TBXItem260;
  TTBXItem *TBXItem261;
  TTBXItem *TBXItem262;
  TTBXSeparatorItem *TBXSeparatorItem72;
  TTBXItem *TBXItem263;
  TTBXSeparatorItem *TBXSeparatorItem73;
  TTBXItem *TBXItem264;
  TTBXItem *TBXItem265;
  TTBXItem *TBXItem266;
  TTBXSeparatorItem *TBXSeparatorItem74;
  TTBXItem *TBXItem267;
  TTBXSubmenuItem *TBXSubmenuItem4;
  TTBXItem *TBXItem269;
  TTBXSeparatorItem *TBXSeparatorItem75;
  TTBXItem *TBXItem268;
  TTBXSubmenuItem *TBXSubmenuItem17;
  TTBXItem *TBXItem270;
  TTBXSeparatorItem *TBXSeparatorItem76;
  TTBXItem *TBXItem271;
  TTBXSubmenuItem *TBXItem272;
  TTBXItem *TBXItem273;
  TTBXItem *TBXItem274;
  TTBXItem *TBXItem275;
  TTBXSubmenuItem *TBXSubmenuItem19;
  TTBXItem *TBXItem276;
  TTBXSeparatorItem *TBXSeparatorItem77;
  TTBXItem *TBXItem277;
  TTBXSubmenuItem *TBXSubmenuItem32;
  TTBXItem *TBXItem278;
  TTBXSeparatorItem *TBXSeparatorItem78;
  TTBXItem *TBXItem279;
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
  void __fastcall LocalFileControlDDFileOperation(
    TObject *Sender, int dwEffect, UnicodeString SourcePath, UnicodeString TargetPath,
    bool Paste, bool &DoOperation);
  void __fastcall RemoteFileControlDDFileOperationExecuted(TObject *Sender,
    int dwEffect, UnicodeString SourcePath, UnicodeString TargetPath);
  void __fastcall LocalDirViewDDTargetHasDropHandler(TObject *Sender,
    TListItem *Item, int &Effect, bool &DropHandler);
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
          UnicodeString &FileName);
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
  void __fastcall LocalDriveViewRefreshDrives(TObject *Sender, bool Global);
  void __fastcall QueueSubmenuItemPopup(TTBCustomItem *Sender,
          bool FromLink);
  void __fastcall DirViewHistoryGo(TCustomDirView *Sender, int Index,
          bool &Cancel);
  void __fastcall LocalDirViewContextPopup(TObject *Sender, TPoint &MousePos, bool &Handled);
  void __fastcall LocalStatusBarPanelClick(TTBXCustomStatusBar *Sender, TTBXStatusPanel *Panel);
  void __fastcall RemoteStatusBarPanelClick(TTBXCustomStatusBar *Sender, TTBXStatusPanel *Panel);
  void __fastcall RemotePathLabelMaskClick(TObject *Sender);
  void __fastcall LocalPathLabelMaskClick(TObject *Sender);
  void __fastcall LocalOpenDirButtonPopup(TTBCustomItem *Sender, bool FromLink);
  void __fastcall RemoteOpenDirButtonPopup(TTBCustomItem *Sender, bool FromLink);
  void __fastcall OtherLocalDirViewEnter(TObject *Sender);
  void __fastcall OtherLocalDriveViewEnter(TObject *Sender);
  void __fastcall OtherLocalDirViewContextPopup(TObject *Sender, TPoint &MousePos, bool &Handled);
  void __fastcall OtherLocalDirViewUpdateStatusBar(TObject *Sender, const TStatusFileInfo &FileInfo);
  void __fastcall OtherLocalDirViewPathChange(TCustomDirView *Sender);
  void __fastcall LocalDriveViewNeedHiddenDirectories(TObject *Sender);

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
  UnicodeString FDDFakeFileTarget;
  bool FCommandLineComboPopulated;
  TStrings* FLocalPathComboBoxPaths;
  int FLocalSpecialPaths;
  unsigned int FSpecialFolders;
  TEdit * FCommandLineComboEdit;
  TWndMethod FToolbarEditOldWndProc;
  bool FPanelsRestored;
  bool FExplorerKeyboardShortcuts;
  bool FForceSystemContextMenu;

  void __fastcall SetLeftPanelWidth(double value);
  double __fastcall GetLeftPanelWidth();
  inline TPanel * __fastcall Panel(bool Left);
  TPanel * __fastcall CurrentPanel();
  void __fastcall CommandLineComboEditWndProc(TMessage & Message);
  void __fastcall ExitToolbar();
  void __fastcall UpdateToolbar2ItemCaption(TTBCustomItem * Item);
  void __fastcall SetShortcuts();
  void __fastcall UpdatePanelsPathLabelsStatus();
  void DoLocalDefaultDirectory(
    TDirView * DirView, const UnicodeString & LastPath, const UnicodeString & StartupSequenceTag);
  void __fastcall LocalDefaultDirectory();
  TOperationSide __fastcall GetOtherSize(TOperationSide Side);
  void __fastcall DoLocalDirViewContextPopup(TOperationSide Side, TPoint & MousePos, bool & Handled);
  void __fastcall DoUpdateFileStatusBar(
    TObject * Sender, TTBXStatusBar * StatusBar, const TStatusFileInfo & FileInfo, TOperationSide Side);

protected:
  virtual bool __fastcall CopyParamDialog(TTransferDirection Direction,
    TTransferType Type, bool Temp, TStrings * FileList,
    UnicodeString & TargetDirectory, TGUICopyParamType & CopyParam, bool Confirm,
    bool DragDrop, int Options);
  virtual TCustomDirView * __fastcall DirView(TOperationSide Side);
  virtual TCustomDriveView * __fastcall DriveView(TOperationSide Side);
  virtual bool __fastcall DirViewEnabled(TOperationSide Side);
  TControl * __fastcall GetComponent(Byte Component);
  virtual void __fastcall RestoreFormParams();
  void __fastcall RestorePanelParams(
    TCustomDirView * DirView, TControl * DriveControl, TTBXStatusBar * StatusBar,
    const TScpCommanderPanelConfiguration & PanelConfiguration);
  void __fastcall StorePanelParams(
    TCustomDirView * DirView, TControl * DriveControl, TTBXStatusBar * StatusBar,
    TScpCommanderPanelConfiguration & PanelConfiguration);
  virtual void __fastcall RestoreParams();
  virtual void __fastcall FixControlsPlacement();
  virtual void __fastcall SessionChanged(bool Replaced);
  virtual void __fastcall ConfigurationChanged();
  virtual bool __fastcall GetHasDirView(TOperationSide Side);
  virtual TCustomDirView * GetCurrentLocalBrowser();
  virtual void UpdatePanelControls(TCustomDirView * ADirView, TCustomDriveView * ADriveView);
  virtual void __fastcall FileOperationProgress(
    TFileOperationProgressType & ProgressData);
  virtual void __fastcall DoOpenDirectoryDialog(TOpenDirectoryMode Mode,
    TOperationSide Side);
  bool __fastcall InternalDDDownload(UnicodeString & TargetDirectory);
  virtual bool __fastcall DDGetTarget(
    UnicodeString & Directory, bool & ForceQueue, UnicodeString & CounterName);
  virtual void __fastcall DDFakeFileInitDrag(TFileList * FileList, bool & Created);
  virtual void __fastcall SideEnter(TOperationSide Side);
  void __fastcall SaveCommandLine();
  bool __fastcall ExecuteCommandLine();
  virtual void __fastcall PanelExportStore(TOperationSide Side,
    TPanelExport Export, TPanelExportDestination Destination,
    TStrings * ExportData);
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
  void __fastcall CreateLocalDirectory(const UnicodeString & Path);
  void __fastcall CreateRemoteDirectory(const UnicodeString & Path);
  void __fastcall LocalPathComboUpdateDrives();
  void __fastcall LocalPathComboUpdate(TCustomDirView * ADirView, TTBXComboBoxItem * PathComboBox);
  virtual void __fastcall ToolbarItemResize(TTBXCustomDropDownItem * Item, int Width);
  void __fastcall DoOpenBookmark(UnicodeString Local, UnicodeString Remote);
  virtual bool __fastcall OpenBookmark(TOperationSide Side, TBookmark * Bookmark);
  virtual void __fastcall DoFocusRemotePath(TTerminal * Terminal, const UnicodeString & Path);
  UnicodeString __fastcall ChangeFilePath(UnicodeString Path, TOperationSide Side);
  virtual bool __fastcall EligibleForImageDisplayMode(TTBCustomItem * Item);
  virtual bool __fastcall UpdateToolbarDisplayMode();
  virtual void __fastcall QueueLabelUpdateStatus();
  virtual void __fastcall StartingWithoutSession();
  virtual void __fastcall UpdateImages();
  virtual void __fastcall FileColorsChanged();
  virtual void __fastcall ThemeChanged();
  void __fastcall DoPathLabelPathClick(TOperationSide Side, const UnicodeString & Path);
  virtual void __fastcall DoRemotePathComboBoxAdjustImageIndex(
    TTBXComboBoxItem * Sender, const UnicodeString AText, int AIndex, int & ImageIndex);
  virtual void __fastcall DoRemotePathComboBoxCancel(TObject * Sender);
  void __fastcall DoLocalDirViewPathChange(TCustomDirView * Sender, TTBXComboBoxItem * PathComboBox);
  void __fastcall DoLocalPathComboBoxAdjustImageIndex(TTBXComboBoxItem * Sender, const UnicodeString AText, int AIndex, int & ImageIndex);
  void __fastcall DoLocalPathComboBoxItemClick(TDirView * ADirView, TTBXComboBoxItem * PathComboBox);
  virtual void __fastcall DoRemotePathComboBoxItemClick(TObject * Sender);
  virtual void __fastcall UpdateRemotePathComboBox(bool TextOnly);
  void __fastcall SetToolbar2ItemAction(TTBXItem * Item, TBasicAction * Action);
  virtual void __fastcall NeedSession(bool Startup);
  void RestoreSessionLocalDirView(TDirView * ALocalDirView, const UnicodeString & LocalDirectory);
  void AnnounceLocalStates(bool RestoreState, bool LocalBrowser, TObject * State, TObject * OtherState);
  virtual UnicodeString GetTabHintDetails(TManagedTerminal * ASession);
  virtual UnicodeString GetNewTabHintDetails();

public:
  __fastcall TScpCommanderForm(TComponent* Owner);
  virtual __fastcall ~TScpCommanderForm();

  virtual void __fastcall AddEditLink(TOperationSide Side, bool Add);
  virtual bool __fastcall AllowedAction(TAction * Action, TActionAllowed Allowed);
  virtual void __fastcall ChangePath(TOperationSide Side);
  virtual void __fastcall CompareDirectories();
  virtual void __fastcall UpdateSession(TManagedTerminal * Terminal);
  virtual void __fastcall SynchronizeDirectories();
  virtual void __fastcall FullSynchronizeDirectories();
  virtual void __fastcall StoreParams();
  virtual void __fastcall ExploreLocalDirectory(TOperationSide Side);
  virtual void __fastcall GoToCommandLine();
  virtual void __fastcall GoToTree();
  virtual void __fastcall OpenConsole(UnicodeString Command = L"");
  virtual UnicodeString __fastcall PathForCaption();
  virtual void __fastcall HomeDirectory(TOperationSide Side);
  virtual void __fastcall HistoryGo(TOperationSide Side, int Index);
  virtual void __fastcall DisplaySystemContextMenu();
  virtual void __fastcall GoToAddress();
  virtual void __fastcall CopyFilesToClipboard(TOperationSide Side, bool OnFocused);
  virtual void __fastcall PasteFromClipBoard();
  virtual void __fastcall BrowseFile(const UnicodeString & FileName);
  virtual bool SupportsLocalBrowser();
  virtual bool IsSideLocalBrowser(TOperationSide Side);
  virtual bool IsLocalBrowserMode();
  virtual void LocalLocalCopy(
    ::TFileOperation Operation, TOperationSide Side, bool OnFocused, bool NoConfirmation, bool DragDrop, unsigned int Flags);
  virtual UnicodeString GetLocalBrowserSessionTitle(TManagedTerminal * Session);
  virtual TManagedTerminal * GetReplacementForLastSession();
  virtual void NewTab(TOperationSide Side, bool AllowReverse);
  virtual int GetNewTabActionImageIndex();
  virtual int GetNewTabTabImageIndex(TOperationSide Side);
  virtual UnicodeString __fastcall DefaultDownloadTargetDirectory();
  virtual bool SupportedSession(TSessionData * SessionData);
  virtual void ResetLayoutColumns(TOperationSide Side);
  virtual void * SaveFocus();
  virtual void RestoreFocus(void * Focus);
  virtual void __fastcall UpdateControls();

  __property double LeftPanelWidth = { read = GetLeftPanelWidth, write = SetLeftPanelWidth };
};
//---------------------------------------------------------------------------
#endif
