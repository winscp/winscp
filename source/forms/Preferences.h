//----------------------------------------------------------------------------
#ifndef PreferencesH
#define PreferencesH
//----------------------------------------------------------------------------
#include "ComboEdit.hpp"
#include "GeneralSettings.h"
#include "LogSettings.h"
#include "UpDownEdit.hpp"
#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Mask.hpp>
#include <Vcl.StdCtrls.hpp>
//----------------------------------------------------------------------------
#include "GeneralSettings.h"
#include "LogSettings.h"
#include "UpDownEdit.hpp"
#include <Dialogs.hpp>
#include <PasTools.hpp>
//----------------------------------------------------------------------------
class TCustomCommandList;
class TEditorList;
//----------------------------------------------------------------------------
class TPreferencesDialog : public TForm
{
__published:
  TButton *OKButton;
  TButton *CloseButton;
  TPanel *MainPanel;
  TPageControl *PageControl;
  TTabSheet *PreferencesSheet;
  TGroupBox *CommonPreferencesGroup;
  TCheckBox *ConfirmOverwritingCheck;
  TCheckBox *ConfirmDeletingCheck;
  TCheckBox *ConfirmClosingSessionCheck;
  TCheckBox *DDTransferConfirmationCheck;
  TCheckBox *ContinueOnErrorCheck;
  TTabSheet *LogSheet;
  TLoggingFrame *LoggingFrame;
  TTabSheet *GeneralSheet;
  TLabel *Label1;
  TGeneralSettingsFrame *GeneralSettingsFrame;
  TTabSheet *PanelsSheet;
  TGroupBox *PanelsRemoteDirectoryGroup;
  TCheckBox *ShowInaccesibleDirectoriesCheck;
  TGroupBox *PanelsCommonGroup;
  TCheckBox *ShowHiddenFilesCheck;
  TCheckBox *DefaultDirIsHomeCheck;
  TTabSheet *CommanderSheet;
  TLabel *Label3;
  TGroupBox *PanelsGroup;
  TCheckBox *PreserveLocalDirectoryCheck;
  TGroupBox *CommanderMiscGroup;
  TCheckBox *UseLocationProfilesCheck;
  TGroupBox *CompareCriterionsGroup;
  TCheckBox *CompareByTimeCheck;
  TCheckBox *CompareBySizeCheck;
  TTabSheet *ExplorerSheet;
  TLabel *Label4;
  TGroupBox *GroupBox2;
  TCheckBox *ShowFullAddressCheck;
  TTabSheet *EditorSheet;
  TGroupBox *EditorPreferenceGroup;
  TGroupBox *InternalEditorGroup;
  TLabel *EditorFontLabel;
  TButton *EditorFontButton;
  TTabSheet *IntegrationSheet;
  TGroupBox *ShellIconsGroup;
  TButton *DesktopIconButton;
  TButton *QuickLaunchIconButton;
  TButton *SendToHookButton;
  TTabSheet *CustomCommandsSheet;
  TGroupBox *CustomCommandsGroup;
  TListView *CustomCommandsView;
  TButton *AddCommandButton;
  TButton *RemoveCommandButton;
  TButton *UpCommandButton;
  TButton *DownCommandButton;
  TButton *EditCommandButton;
  TPanel *LeftPanel;
  TTreeView *NavigationTree;
  TCheckBox *DeleteToRecycleBinCheck;
  TButton *RegisterAsUrlHandlerButton;
  TTabSheet *DragDropSheet;
  TGroupBox *DragDropDownloadsGroup;
  TLabel *DDExtEnabledLabel;
  TLabel *DDExtDisabledLabel;
  TRadioButton *DDExtEnabledButton;
  TRadioButton *DDExtDisabledButton;
  TPanel *DDExtDisabledPanel;
  TCheckBox *DDWarnLackOfTempSpaceCheck;
  TCheckBox *DDWarnOnMoveCheck;
  TCheckBox *ConfirmExitOnCompletionCheck;
  TTabSheet *QueueSheet;
  TGroupBox *QueueGroup;
  TUpDownEdit *QueueTransferLimitEdit;
  TLabel *Label5;
  TGroupBox *QueueViewGroup;
  TRadioButton *QueueViewShowButton;
  TRadioButton *QueueViewHideWhenEmptyButton;
  TRadioButton *QueueViewHideButton;
  TCheckBox *QueueAutoPopupCheck;
  TCheckBox *QueueCheck;
  TCheckBox *DDAllowMoveInitCheck;
  TCheckBox *RememberPasswordCheck;
  TCheckBox *ConfirmResumeCheck;
  TTabSheet *StorageSheet;
  TGroupBox *StorageGroup;
  TRadioButton *RegistryStorageButton;
  TRadioButton *IniFileStorageButton2;
  TGroupBox *NotificationsGroup;
  TCheckBox *BeepOnFinishCheck;
  TUpDownEdit *BeepOnFinishAfterEdit;
  TLabel *BeepOnFinishAfterText;
  TTabSheet *TransferEnduranceSheet;
  TGroupBox *ResumeBox;
  TLabel *ResumeThresholdUnitLabel;
  TRadioButton *ResumeOnButton;
  TRadioButton *ResumeSmartButton;
  TRadioButton *ResumeOffButton;
  TUpDownEdit *ResumeThresholdEdit;
  TCheckBox *ConfirmCommandSessionCheck;
  TGroupBox *TemporaryDirectoryGrouo;
  TRadioButton *DDSystemTemporaryDirectoryButton;
  TRadioButton *DDCustomTemporaryDirectoryButton;
  TDirectoryEdit *DDTemporaryDirectoryEdit;
  TLabel *Label6;
  TCheckBox *TemporaryDirectoryCleanupCheck;
  TCheckBox *ConfirmTemporaryDirectoryCleanupCheck;
  TGroupBox *OtherStorageGroup;
  TLabel *RandomSeedFileLabel;
  TFilenameEdit *RandomSeedFileEdit;
  TCheckBox *SwappedPanelsCheck;
  TCheckBox *PreservePanelStateCheck;
  TButton *AddSearchPathButton;
  TCheckBox *QueueNoConfirmationCheck;
  TCheckBox *EditorWordWrapCheck;
  TGroupBox *PathInCaptionGroup;
  TRadioButton *PathInCaptionFullButton;
  TRadioButton *PathInCaptionShortButton;
  TRadioButton *PathInCaptionNoneButton;
  TTabSheet *UpdatesSheet;
  TGroupBox *UpdatesGroup;
  TGroupBox *UpdatesProxyGroup;
  TLabel *UpdatesProxyHostLabel;
  TLabel *UpdatesProxyPortLabel;
  TUpDownEdit *UpdatesProxyPortEdit;
  TEdit *UpdatesProxyHostEdit;
  TRadioButton *UpdatesProxyCheck;
  TTabSheet *CopyParamListSheet;
  TGroupBox *CopyParamListGroup;
  TListView *CopyParamListView;
  TButton *AddCopyParamButton;
  TButton *RemoveCopyParamButton;
  TButton *UpCopyParamButton;
  TButton *DownCopyParamButton;
  TButton *EditCopyParamButton;
  TButton *DuplicateCopyParamButton;
  TCheckBox *CopyParamAutoSelectNoticeCheck;
  TButton *HelpButton;
  TGroupBox *ThemeGroup;
  TLabel *Label7;
  TComboBox *ThemeCombo;
  TListView *EditorListView2;
  TButton *AddEditorButton;
  TButton *EditEditorButton;
  TButton *UpEditorButton;
  TButton *DownEditorButton;
  TButton *RemoveEditorButton;
  TGroupBox *DoubleClickGroup;
  TCheckBox *CopyOnDoubleClickConfirmationCheck;
  TLabel *DoubleClickActionLabel;
  TComboBox *DoubleClickActionCombo;
  TLabel *Label8;
  TComboBox *NortonLikeModeCombo;
  TStaticText *ShellIconsText;
  TCheckBox *FullRowSelectCheck;
  TGroupBox *SessionReopenGroup;
  TLabel *SessionReopenAutoLabel;
  TCheckBox *SessionReopenAutoCheck;
  TLabel *SessionReopenAutoSecLabel;
  TUpDownEdit *SessionReopenAutoEdit;
  TTabSheet *WindowSheet;
  TCheckBox *AutoReadDirectoryAfterOpCheck;
  TCheckBox *ConfirmRecyclingCheck;
  TGroupBox *WindowMiscellaneousGroup;
  TCheckBox *MinimizeToTrayCheck;
  TCheckBox *BalloonNotificationsCheck;
  TTabSheet *IntegrationAppSheet;
  TGroupBox *ExternalAppsGroup;
  TLabel *Label2;
  TEdit *PuttyPathEdit;
  TCheckBox *PuttyPasswordCheck2;
  TCheckBox *AutoOpenInPuttyCheck;
  TButton *PuttyPathBrowseButton;
  TButton *PuttyPathResetButton;
  TCheckBox *TelnetForFtpInPuttyCheck;
  TRadioButton *UpdatesDirectCheck;
  TRadioButton *UpdatesAutoCheck;
  TLabel *Label9;
  TUpDownEdit *EditorTabSizeEdit;
  TCheckBox *ConfirmTransferringCheck;
  TGroupBox *UpdatesOptionsGroup;
  TCheckBox *QueueIndividuallyCheck;
  TCheckBox *SessionReopenAutoIdleCheck;
  TCheckBox *RenameWholeNameCheck;
  TCheckBox *TreeOnLeftCheck;
  TCheckBox *TemporaryDirectoryAppendSessionCheck;
  TCheckBox *TemporaryDirectoryAppendPathCheck;
  TLabel *SessionReopenTimeoutLabel;
  TUpDownEdit *SessionReopenTimeoutEdit;
  TLabel *SessionReopenTimeoutSecLabel;
  TTabSheet *SecuritySheet;
  TGroupBox *MasterPasswordGroup;
  TButton *SetMasterPasswordButton;
  TCheckBox *UseMasterPasswordCheck;
  TTabSheet *NetworkSheet;
  TGroupBox *ExternalIpAddressGroupBox;
  TRadioButton *RetrieveExternalIpAddressButton;
  TRadioButton *CustomExternalIpAddressButton;
  TEdit *CustomExternalIpAddressEdit;
  TCheckBox *FormatSizeBytesCheck;
  TCheckBox *SessionReopenAutoStallCheck;
  TLabel *SessionReopenAutoStallLabel;
  TUpDownEdit *SessionReopenAutoStallEdit;
  TLabel *SessionReopenAutoStallSecLabel;
  TCheckBox *EnableQueueByDefaultCheck;
  TLabel *Label11;
  TComboBox *EditorEncodingCombo;
  TCheckBox *RefreshRemotePanelCheck;
  TUpDownEdit *RefreshRemotePanelIntervalEdit;
  TLabel *RefreshRemoteDirectoryUnitLabel;
  TLabel *Label12;
  TComboBox *UpdatesPeriodCombo;
  TLabel *UpdatesBetaVersionsLabel;
  TComboBox *UpdatesBetaVersionsCombo;
  TCheckBox *CollectUsageCheck;
  TButton *UsageViewButton;
  TTabSheet *PanelRemoteSheet;
  TGroupBox *WorkspacesGroup;
  TCheckBox *AutoSaveWorkspaceCheck;
  TComboBox *AutoWorkspaceCombo;
  TCheckBox *AutoSaveWorkspacePasswordsCheck;
  TLabel *CopyParamLabel;
  TGroupBox *ConnectionsGroup;
  TCheckBox *TryFtpWhenSshFailsCheck;
  TLabel *QueueKeepDoneItemsCheck;
  TComboBox *QueueKeepDoneItemsForCombo;
  TLabel *Label10;
  TComboBox *ExplorerKeyboardShortcutsCombo;
  TLabel *SessionReopenAutoIdleLabel;
  TUpDownEdit *SessionReopenAutoIdleEdit;
  TLabel *SessionReopenAutoIdleSecLabel;
  TTabSheet *PanelLocalSheet;
  TGroupBox *LocalPanelGroup;
  TCheckBox *SystemContextMenuCheck;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall EditorFontButtonClick(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall IconButtonClick(TObject *Sender);
  void __fastcall CustomCommandsViewData(TObject *Sender, TListItem *Item);
  void __fastcall ListViewSelectItem(TObject *Sender,
          TListItem *Item, bool Selected);
  void __fastcall CustomCommandsViewKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
  void __fastcall AddEditCommandButtonClick(TObject *Sender);
  void __fastcall RemoveCommandButtonClick(TObject *Sender);
  void __fastcall UpDownCommandButtonClick(TObject *Sender);
  void __fastcall ListViewStartDrag(TObject *Sender,
          TDragObject *&DragObject);
  void __fastcall CustomCommandsViewDragDrop(TObject *Sender,
          TObject *Source, int X, int Y);
  void __fastcall ListViewDragOver(TObject *Sender,
          TObject *Source, int X, int Y, TDragState State, bool &Accept);
  void __fastcall NavigationTreeChange(TObject *Sender, TTreeNode *Node);
  void __fastcall PageControlChange(TObject *Sender);
  void __fastcall RegisterAsUrlHandlerButtonClick(TObject *Sender);
  void __fastcall DDExtLabelClick(TObject *Sender);
  void __fastcall CustomCommandsViewDblClick(TObject *Sender);
  void __fastcall AddSearchPathButtonClick(TObject *Sender);
  void __fastcall EditorFontLabelDblClick(TObject *Sender);
  void __fastcall CopyParamListViewData(TObject *Sender, TListItem *Item);
  void __fastcall CopyParamListViewDragDrop(TObject *Sender,
          TObject *Source, int X, int Y);
  void __fastcall UpDownCopyParamButtonClick(TObject *Sender);
  void __fastcall RemoveCopyParamButtonClick(TObject *Sender);
  void __fastcall CopyParamListViewKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
  void __fastcall AddCopyParamButtonClick(TObject *Sender);
  void __fastcall CopyParamListViewDblClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall EditorListView2DragDrop(TObject *Sender, TObject *Source,
          int X, int Y);
  void __fastcall UpDownEditorButtonClick(TObject *Sender);
  void __fastcall RemoveEditorButtonClick(TObject *Sender);
  void __fastcall AddEditEditorButtonClick(TObject *Sender);
  void __fastcall EditorListView2DblClick(TObject *Sender);
  void __fastcall EditorListView2KeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
  void __fastcall EditorListView2Data(TObject *Sender, TListItem *Item);
  void __fastcall PuttyPathBrowseButtonClick(TObject *Sender);
  void __fastcall PuttyPathResetButtonClick(TObject *Sender);
  void __fastcall PathEditBeforeDialog(TObject *Sender,
          UnicodeString &Name, bool &Action);
  void __fastcall PathEditAfterDialog(TObject *Sender,
          UnicodeString &Name, bool &Action);
  void __fastcall NavigationTreeCollapsing(TObject *Sender,
          TTreeNode *Node, bool &AllowCollapse);
  void __fastcall ListViewEndDrag(TObject *Sender,
          TObject *Target, int X, int Y);
  void __fastcall RandomSeedFileEditCreateEditDialog(TObject *Sender,
          TFileDialogKind DialogKind, TOpenDialog *&Dialog);
  void __fastcall SessionReopenTimeoutEditSetValue(TObject *Sender,
          Extended Value, UnicodeString &Text, bool &Handed);
  void __fastcall SessionReopenTimeoutEditGetValue(TObject *Sender,
          UnicodeString Text, Extended &Value, bool &Handed);
  void __fastcall UseMasterPasswordCheckClick(TObject *Sender);
  void __fastcall SetMasterPasswordButtonClick(TObject *Sender);
  void __fastcall UsageViewButtonClick(TObject *Sender);
  void __fastcall DuplicateCopyParamButtonClick(TObject *Sender);
  void __fastcall EditCopyParamButtonClick(TObject *Sender);
  void __fastcall CopyParamLabelClick(TObject *Sender);
  void __fastcall CopyParamListViewCustomDrawItem(TCustomListView *Sender, TListItem *Item,
          TCustomDrawState State, bool &DefaultDraw);

private:
  TPreferencesMode FPreferencesMode;
  TFont * FEditorFont;
  TCustomCommandList * FCustomCommandList;
  TCopyParamList * FCopyParamList;
  TEditorList * FEditorList;
  bool FCustomCommandChanging;
  bool FAfterFilenameEditDialog;
  int FListViewDragSource;
  int FListViewDragDest;
  TPreferencesDialogData * FDialogData;
  UnicodeString FBeforeDialogPath;
  TCopyParamType FCopyParams;
  TListViewScrollOnDragOver * FCustomCommandsScrollOnDragOver;
  TListViewScrollOnDragOver * FCopyParamScrollOnDragOver;
  TListViewScrollOnDragOver * FEditorScrollOnDragOver;
  bool FNoUpdate;
  void __fastcall SetPreferencesMode(TPreferencesMode value);
  void __fastcall CMDialogKey(TWMKeyDown & Message);
  void __fastcall WMHelp(TWMHelp & Message);
  UnicodeString __fastcall TabSample(UnicodeString Values);
  void __fastcall AddEditCopyParam(TCopyParamPresetMode Mode);
  const TCopyParamType * GetCopyParam(int Index);
public:
  virtual __fastcall ~TPreferencesDialog();
  bool __fastcall Execute(TPreferencesDialogData * DialogData);
  virtual __fastcall TPreferencesDialog(TComponent* AOwner);
  __property TPreferencesMode PreferencesMode = { read = FPreferencesMode, write = SetPreferencesMode };
protected:
  void __fastcall LoadConfiguration();
  void __fastcall SaveConfiguration();
  void __fastcall UpdateControls();
  void __fastcall UpdateCustomCommandsView();
  void __fastcall UpdateCopyParamListView();
  void __fastcall UpdateEditorListView();
  void __fastcall CustomCommandMove(int Source, int Dest);
  void __fastcall CopyParamMove(int Source, int Dest);
  void __fastcall EditorMove(int Source, int Dest);
  bool __fastcall AllowListViewDrag(TObject * Sender, int X, int Y);
  void __fastcall PrepareNavigationTree(TTreeView * Tree);
  virtual void __fastcall Dispatch(void * Message);
  TListViewScrollOnDragOver * __fastcall ScrollOnDragOver(TObject * ListView);
};
//----------------------------------------------------------------------------
#endif
