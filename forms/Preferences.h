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
#include <XPGroupBox.hpp>

#include "CopyParams.h"
#include "GeneralSettings.h"
#include "LogSettings.h"
#include "UpDownEdit.hpp"           
//----------------------------------------------------------------------------
class TPreferencesDialog : public TForm
{
__published:
  TPageControl *PageControl;
  TTabSheet *LogSheet;
  TLoggingFrame *LoggingFrame;
  TTabSheet *GeneralSheet;
  TGeneralSettingsFrame *GeneralSettingsFrame;
  TButton *OKButton;
  TButton *CloseButton;
  TLabel *Label1;
  TTabSheet *PreferencesSheet;
  TXPGroupBox *CommonPreferencesGroup;
  TTabSheet *PanelsSheet;
  TLabel *RandomSeedFileLabel;
  TFilenameEdit *RandomSeedFileEdit;
  TCheckBox *CopyOnDoubleClickCheck;
  TCheckBox *CopyOnDoubleClickConfirmationCheck;
  TXPGroupBox *PanelsRemoteDirectoryGroup;
  TCheckBox *ShowInaccesibleDirectoriesCheck;
  TXPGroupBox *PanelsCommonGroup;
  TCheckBox *ShowHiddenFilesCheck;
  TTabSheet *CommanderSheet;
  TCheckBox *ConfirmOverwritingCheck;
  TXPGroupBox *DragDropPreferencesGroup;
  TXPGroupBox *StorageGroup;
  TRadioButton *RegistryStorageButton;
  TRadioButton *IniFileStorageButton;
  TTabSheet *ExplorerSheet;
  TLabel *Label4;
  TCheckBox *ConfirmDeletingCheck;
  TTabSheet *TransferSheet;
  TCopyParamsFrame *CopyParamsFrame;
  TCheckBox *DefaultDirIsHomeCheck;
  TCheckBox *ConfirmClosingSessionCheck;
  TCheckBox *DDTransferConfirmationCheck;
  TCheckBox *DDAllowMoveCheck;
  TXPGroupBox *PanelsGroup;
  TCheckBox *DeleteToRecycleBinCheck;
  TCheckBox *ExplorerStyleSelectionCheck;
  TXPGroupBox *GroupBox2;
  TCheckBox *ShowFullAddressCheck;
  TRadioButton *DDSystemTemporaryDirectoryButton;
  TRadioButton *DDCustomTemporaryDirectoryButton;
  TLabel *Label5;
  TDirectoryEdit *DDTemporaryDirectoryEdit;
  TCheckBox *DDWarnLackOfTempSpaceCheck;
  TXPGroupBox *ResumeBox;
  TRadioButton *ResumeOnButton;
  TRadioButton *ResumeSmartButton;
  TRadioButton *ResumeOffButton;
  TUpDownEdit *ResumeThresholdEdit;
  TLabel *ResumeThresholdUnitLabel;
  TTabSheet *EditorSheet;
  TXPGroupBox *EditorGroup;
  TRadioButton *EditorInternalButton;
  TRadioButton *EditorExternalButton;
  TFilenameEdit *ExternalEditorEdit;
  TXPGroupBox *EditorFontGroup;
  TLabel *EditorFontLabel;
  TButton *EditorFontButton;
  TXPGroupBox *EditorOptionsGroup;
  TCheckBox *EditorWordWrapCheck;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall EditorFontButtonClick(TObject *Sender);
private:
  TPreferencesMode FPreferencesMode;
  TFont * FEditorFont;
  void __fastcall SetPreferencesMode(TPreferencesMode value);
public:
  virtual __fastcall ~TPreferencesDialog();
  bool __fastcall Execute();
  virtual __fastcall TPreferencesDialog(TComponent* AOwner);
  __property TPreferencesMode PreferencesMode = { read = FPreferencesMode, write = SetPreferencesMode };
protected:
  void __fastcall LoadConfiguration();
  void __fastcall LoggingGetDefaultLogFileName(System::TObject * Sender, AnsiString & DefaultLogFileName);
  void __fastcall SaveConfiguration();
  void __fastcall UpdateControls();
};
//----------------------------------------------------------------------------
#endif
