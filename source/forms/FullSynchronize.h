//---------------------------------------------------------------------------
#ifndef FullSynchronizeH
#define FullSynchronizeH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <HistoryComboBox.h>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Menus.hpp>
//---------------------------------------------------------------------------
#include <WinInterface.h>
#include <CopyParam.h>
#include <GUITools.h>
//---------------------------------------------------------------------------
class TFullSynchronizeDialog : public TForm
{
__published:
  TGroupBox *DirectoriesGroup;
  TButton *OkButton;
  TButton *CancelButton;
  TLabel *LocalDirectoryLabel;
  TLabel *RemoteDirectoryLabel;
  THistoryComboBox *RemoteDirectoryEdit;
  THistoryComboBox *LocalDirectoryEdit;
  TGroupBox *OptionsGroup;
  TCheckBox *SynchronizeDeleteCheck;
  TCheckBox *SynchronizeSelectedOnlyCheck;
  TButton *LocalDirectoryBrowseButton;
  TCheckBox *SynchronizeExistingOnlyCheck;
  TButton *TransferSettingsButton;
  TCheckBox *SynchronizePreviewChangesCheck;
  TGroupBox *DirectionGroup;
  TRadioButton *SynchronizeBothButton;
  TRadioButton *SynchronizeRemoteButton;
  TRadioButton *SynchronizeLocalButton;
  TGroupBox *CompareCriterionsGroup;
  TCheckBox *SynchronizeByTimeCheck;
  TCheckBox *SynchronizeBySizeCheck;
  TCheckBox *SaveSettingsCheck;
  TGroupBox *CopyParamGroup;
  TLabel *CopyParamLabel;
  TButton *HelpButton;
  TGroupBox *ModeGroup;
  TRadioButton *SynchronizeFilesButton;
  TRadioButton *MirrorFilesButton;
  TRadioButton *SynchronizeTimestampsButton;
  TImage *Image;
  TPopupMenu *OkMenu;
  TMenuItem *Start1;
  TMenuItem *StartInNewWindowItem;
  TCheckBox *SynchronizeCaseSensitiveCheck;
  TCheckBox *SynchronizeByChecksumCheck;
  TButton *OtherLocalDirectoryBrowseButton;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall LocalDirectoryBrowseButtonClick(TObject *Sender);
  void __fastcall TransferSettingsButtonClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall CopyParamGroupContextPopup(TObject *Sender,
          TPoint &MousePos, bool &Handled);
  void __fastcall CopyParamGroupClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall TransferSettingsButtonDropDownClick(TObject *Sender);
  void __fastcall Start1Click(TObject *Sender);
  void __fastcall OkButtonDropDownClick(TObject *Sender);
  void __fastcall OkButtonClick(TObject *Sender);
  void __fastcall StartInNewWindowItemClick(TObject *Sender);
  void __fastcall OtherLocalDirectoryBrowseButtonClick(TObject *Sender);

private:
  int FParams;
  bool FSaveMode;
  TSynchronizeMode FOrigMode;
  int FOptions;
  TUsableCopyParamAttrs FCopyParamAttrs;
  TCopyParamType FCopyParams;
  TPopupMenu * FPresetsMenu;
  UnicodeString FPreset;
  UnicodeString FSynchronizeBySizeCaption;
  TFullSynchronizeInNewWindow FOnFullSynchronizeInNewWindow;
  void SetDirectory2(const UnicodeString & value);
  UnicodeString GetDirectory2();
  void SetDirectory1(const UnicodeString & value);
  UnicodeString GetDirectory1();
  void __fastcall SetMode(TSynchronizeMode value);
  TSynchronizeMode __fastcall GetMode();
  void __fastcall SetParams(int value);
  int __fastcall GetParams();
  void __fastcall SetSaveSettings(bool value);
  bool __fastcall GetSaveSettings();
  void __fastcall SetCopyParams(const TCopyParamType & value);
  TCopyParamType __fastcall GetCopyParams();
  void __fastcall CopyParamClick(TObject * Sender);
  int __fastcall ActualCopyParamAttrs();
  void __fastcall CopyParamListPopup(TRect R, int AdditionalOptions);
  bool __fastcall AllowStartInNewWindow();
  bool __fastcall CanStartInNewWindow();
  void __fastcall Submitted();
  void __fastcall StartInNewWindow();
  void DoLocalDirectoryBrowseButtonClick(TComboBox * ComboBox);
  bool CanSynchronizeTimestamps();

public:
  __fastcall TFullSynchronizeDialog(TComponent* Owner);
  virtual __fastcall ~TFullSynchronizeDialog();

  void __fastcall Init(
    int Options, const TUsableCopyParamAttrs & CopyParamAttrs, TFullSynchronizeInNewWindow OnFullSynchronizeInNewWindow);

  bool __fastcall Execute();

  __property UnicodeString Directory1 = { read = GetDirectory1, write = SetDirectory1 };
  __property UnicodeString Directory2 = { read = GetDirectory2, write = SetDirectory2 };
  __property int Params = { read = GetParams, write = SetParams };
  __property TSynchronizeMode Mode = { read = GetMode, write = SetMode };
  __property bool SaveSettings = { read = GetSaveSettings, write = SetSaveSettings };
  __property bool SaveMode = { read = FSaveMode, write = FSaveMode };
  __property TCopyParamType CopyParams = { read = GetCopyParams, write = SetCopyParams };

protected:
  void __fastcall UpdateControls();

  INTERFACE_HOOK
};
//---------------------------------------------------------------------------
#endif
