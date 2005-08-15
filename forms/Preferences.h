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
#include <XPThemes.hpp>

#include "CopyParams.h"
#include "GeneralSettings.h"
#include "LogSettings.h"
#include "UpDownEdit.hpp"
#include "IEComboBox.hpp"
#include "HistoryComboBox.hpp"
#include "PasswordEdit.hpp"
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
  TXPGroupBox *CommonPreferencesGroup;
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
  TXPGroupBox *PanelsRemoteDirectoryGroup;
  TCheckBox *ShowInaccesibleDirectoriesCheck;
  TXPGroupBox *PanelsCommonGroup;
  TCheckBox *ShowHiddenFilesCheck;
  TCheckBox *DefaultDirIsHomeCheck;
  TTabSheet *CommanderSheet;
  TLabel *Label3;
  TXPGroupBox *PanelsGroup;
  TCheckBox *PreserveLocalDirectoryCheck;
  TXPGroupBox *CommanderMiscGroup;
  TCheckBox *UseLocationProfilesCheck;
  TXPGroupBox *CompareCriterionsGroup;
  TCheckBox *CompareByTimeCheck;
  TCheckBox *CompareBySizeCheck;
  TTabSheet *ExplorerSheet;
  TLabel *Label4;
  TXPGroupBox *GroupBox2;
  TCheckBox *ShowFullAddressCheck;
  TTabSheet *TransferSheet;
  TCopyParamsFrame *CopyParamsFrame;
  TTabSheet *EditorSheet;
  TXPGroupBox *EditorPreferenceGroup;
  TXPGroupBox *InternalEditorGroup;
  TLabel *EditorFontLabel;
  TButton *EditorFontButton;
  TTabSheet *IntegrationSheet;
  TXPGroupBox *ShellIconsGroup;
  TButton *DesktopIconButton;
  TButton *QuickLaunchIconButton;
  TButton *SendToHookButton;
  TXPGroupBox *ExternalAppsGroup;
  TLabel *Label2;
  TFilenameEdit *PuttyPathEdit;
  TTabSheet *CustomCommandsSheet;
  TXPGroupBox *CustomCommandsGroup;
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
  TXPGroupBox *DragDropDownloadsGroup;
  TLabel *DDExtEnabledLabel;
  TLabel *DDExtDisabledLabel;
  TRadioButton *DDExtEnabledButton;
  TRadioButton *DDExtDisabledButton;
  TPanel *DDExtDisabledPanel;
  TCheckBox *DDWarnLackOfTempSpaceCheck;
  TCheckBox *DDWarnOnMoveCheck;
  TCheckBox *ConfirmExitOnCompletionCheck;
  TTabSheet *QueueSheet;
  TXPGroupBox *QueueGroup;
  TUpDownEdit *QueueTransferLimitEdit;
  TLabel *Label5;
  TXPGroupBox *QueueViewGroup;
  TRadioButton *QueueViewShowButton;
  TRadioButton *QueueViewHideWhenEmptyButton;
  TRadioButton *QueueViewHideButton;
  TCheckBox *QueueAutoPopupCheck;
  TCheckBox *QueueCheck;
  TCheckBox *DDAllowMoveInitCheck;
  TCheckBox *RememberPasswordCheck;
  TCheckBox *ConfirmResumeCheck;
  TTabSheet *StorageSheet;
  TXPGroupBox *StorageGroup;
  TRadioButton *RegistryStorageButton;
  TRadioButton *IniFileStorageButton;
  TXPGroupBox *NotificationsGroup;
  TCheckBox *BeepOnFinishCheck;
  TUpDownEdit *BeepOnFinishAfterEdit;
  TLabel *BeepOnFinishAfterText;
  TTabSheet *TransferResumeSheet;
  TXPGroupBox *ResumeBox;
  TLabel *ResumeThresholdUnitLabel;
  TRadioButton *ResumeOnButton;
  TRadioButton *ResumeSmartButton;
  TRadioButton *ResumeOffButton;
  TUpDownEdit *ResumeThresholdEdit;
  TCheckBox *ConfirmCommandSessionCheck;
  TXPGroupBox *SingleEditorGroup;
  TRadioButton *EditorSingleEditorOnCheck;
  TRadioButton *EditorSingleEditorOffCheck;
  TXPGroupBox *TemporaryDirectoryGrouo;
  TRadioButton *DDSystemTemporaryDirectoryButton;
  TRadioButton *DDCustomTemporaryDirectoryButton;
  TDirectoryEdit *DDTemporaryDirectoryEdit;
  TLabel *Label6;
  TCheckBox *TemporaryDirectoryCleanupCheck;
  TCheckBox *ConfirmTemporaryDirectoryCleanupCheck;
  TCheckBox *PuttyPasswordCheck;
  TXPGroupBox *OtherStorageGroup;
  TLabel *RandomSeedFileLabel;
  TFilenameEdit *RandomSeedFileEdit;
  TCheckBox *SwappedPanelsCheck;
  TCheckBox *PreservePanelStateCheck;
  TButton *AddSearchPathButton;
  TCheckBox *QueueNoConfirmationCheck;
  TCheckBox *EditorWordWrapCheck;
  TXPGroupBox *PathInCaptionGroup;
  TRadioButton *PathInCaptionFullButton;
  TRadioButton *PathInCaptionShortButton;
  TRadioButton *PathInCaptionNoneButton;
  TTabSheet *UpdatesSheet;
  TXPGroupBox *UpdatesGroup;
  TRadioButton *UpdatesNeverButton;
  TRadioButton *UpdatesDailyButton;
  TRadioButton *UpdatesWeeklyButton;
  TRadioButton *UpdatesMonthlyButton;
  TXPGroupBox *UpdatesProxyGroup;
  TLabel *Label15;
  TLabel *Label18;
  TUpDownEdit *UpdatesProxyPortEdit;
  TEdit *UpdatesProxyHostEdit;
  TCheckBox *UpdatesProxyCheck;
  TTabSheet *CopyParamListSheet;
  TXPGroupBox *CopyParamListGroup;
  TListView *CopyParamListView;
  TButton *AddCopyParamButton;
  TButton *RemoveCopyParamButton;
  TButton *UpCopyParamButton;
  TButton *DownCopyParamButton;
  TButton *EditCopyParamButton;
  TButton *DuplicateCopyParamButton;
  TXPGroupBox *CopyParamListOptionsGroup;
  TCheckBox *CopyParamAutoSelectNoticeCheck;
  TButton *HelpButton;
  TXPGroupBox *ThemeGroup;
  TLabel *Label7;
  TComboBox *ThemeCombo;
  TListView *EditorListView;
  TButton *AddEditorButton;
  TButton *EditEditorButton;
  TButton *UpEditorButton;
  TButton *DownEditorButton;
  TButton *RemoveEditorButton;
  TXPGroupBox *DoubleClickGroup;
  TCheckBox *CopyOnDoubleClickConfirmationCheck;
  TLabel *DoubleClickActionLabel;
  TComboBox *DoubleClickActionCombo;
  TLabel *Label8;
  TComboBox *NortonLikeModeCombo;
  TCheckBox *AutoOpenInPuttyCheck;
  TStaticText *ShellIconsText;
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
  void __fastcall SetPreferencesMode(TPreferencesMode value);
  void __fastcall CMDialogKey(TWMKeyDown & Message);
public:
  virtual __fastcall ~TPreferencesDialog();
  bool __fastcall Execute(TPreferencesDialogData * DialogData);
  virtual __fastcall TPreferencesDialog(TComponent* AOwner);
  __property TPreferencesMode PreferencesMode = { read = FPreferencesMode, write = SetPreferencesMode };
protected:
  void __fastcall LoadConfiguration();
  void __fastcall LoggingGetDefaultLogFileName(System::TObject * Sender, AnsiString & DefaultLogFileName);
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
};
//----------------------------------------------------------------------------
#endif
