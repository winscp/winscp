//----------------------------------------------------------------------------
#ifndef PreferencesH
#define PreferencesH
//----------------------------------------------------------------------------
#include <vcl\System.hpp>
#include <vcl\Windows.hpp>
#include <vcl\SysUtils.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Graphics.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Buttons.hpp>
#include <vcl\ExtCtrls.hpp>
#include <ComCtrls.hpp>
#include <Comboedit.hpp>
#include <Mask.hpp>
#include <ComboEdit.hpp>

#include "CopyParams.h"
#include "GeneralSettings.h"
#include "LogSettings.h"
#include "UpDownEdit.hpp"
#include "IEComboBox.hpp"
#include "HistoryComboBox.hpp"
#include "PasswordEdit.hpp"
#include <Dialogs.hpp>
#include <PasTools.hpp>
//----------------------------------------------------------------------------
class TCustomCommands;
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
  TTabSheet *TransferSheet;
  TCopyParamsFrame *CopyParamsFrame;
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
  TRadioButton *UpdatesNeverButton;
  TRadioButton *UpdatesDailyButton;
  TRadioButton *UpdatesWeeklyButton;
  TRadioButton *UpdatesMonthlyButton;
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
  TGroupBox *CopyParamListOptionsGroup;
  TCheckBox *CopyParamAutoSelectNoticeCheck;
  TButton *HelpButton;
  TGroupBox *ThemeGroup;
  TLabel *Label7;
  TComboBox *ThemeCombo;
  TListView *EditorListView;
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
  TButton *ExportButton;
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
  void __fastcall FormShow(TObject *Sender);
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall EditorFontButtonClick(TObject *Sender);
  void __fastcall FilenameEditExit(TObject *Sender);
  void __fastcall FilenameEditChange(TObject *Sender);
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
  void __fastcall CopyParamListViewInfoTip(TObject *Sender,
          TListItem *Item, AnsiString &InfoTip);
  void __fastcall CopyParamListViewDragDrop(TObject *Sender,
          TObject *Source, int X, int Y);
  void __fastcall UpDownCopyParamButtonClick(TObject *Sender);
  void __fastcall RemoveCopyParamButtonClick(TObject *Sender);
  void __fastcall CopyParamListViewKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
  void __fastcall AddEditCopyParamButtonClick(TObject *Sender);
  void __fastcall CopyParamListViewDblClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall EditorListViewDragDrop(TObject *Sender, TObject *Source,
          int X, int Y);
  void __fastcall UpDownEditorButtonClick(TObject *Sender);
  void __fastcall RemoveEditorButtonClick(TObject *Sender);
  void __fastcall AddEditEditorButtonClick(TObject *Sender);
  void __fastcall EditorListViewDblClick(TObject *Sender);
  void __fastcall EditorListViewKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
  void __fastcall EditorListViewData(TObject *Sender, TListItem *Item);
  void __fastcall PuttyPathBrowseButtonClick(TObject *Sender);
  void __fastcall PuttyPathResetButtonClick(TObject *Sender);
  void __fastcall ExportButtonClick(TObject *Sender);
  void __fastcall PathEditBeforeDialog(TObject *Sender,
          AnsiString &Name, bool &Action);
  void __fastcall PathEditAfterDialog(TObject *Sender,
          AnsiString &Name, bool &Action);
  void __fastcall NavigationTreeCollapsing(TObject *Sender,
          TTreeNode *Node, bool &AllowCollapse);
  void __fastcall ListViewEndDrag(TObject *Sender,
          TObject *Target, int X, int Y);
  void __fastcall RandomSeedFileEditCreateEditDialog(TObject *Sender,
          TFileDialogKind DialogKind, TOpenDialog *&Dialog);
private:
  TPreferencesMode FPreferencesMode;
  TFont * FEditorFont;
  TCustomCommands * FCustomCommands;
  TCopyParamList * FCopyParamList;
  TEditorList * FEditorList;
  bool FCustomCommandChanging;
  bool FAfterFilenameEditDialog;
  int FListViewDragSource;
  int FListViewDragDest;
  TPreferencesDialogData * FDialogData;
  AnsiString FBeforeDialogPath;
  TListViewScrollOnDragOver * FCustomCommandsScrollOnDragOver;
  TListViewScrollOnDragOver * FCopyParamScrollOnDragOver;
  TListViewScrollOnDragOver * FEditorScrollOnDragOver;
  bool FNoUpdate;
  void __fastcall SetPreferencesMode(TPreferencesMode value);
  void __fastcall CMDialogKey(TWMKeyDown & Message);
  void __fastcall WMHelp(TWMHelp & Message);
  AnsiString __fastcall TabSample(AnsiString Values);
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
