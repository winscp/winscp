//---------------------------------------------------------------------------
#ifndef FullSynchronizeH
#define FullSynchronizeH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <HistoryComboBox.hpp>

#include <WinInterface.h>
#include <CopyParam.h>
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
  void __fastcall SetRemoteDirectory(const UnicodeString value);
  UnicodeString __fastcall GetRemoteDirectory();
  void __fastcall SetLocalDirectory(const UnicodeString value);
  UnicodeString __fastcall GetLocalDirectory();
  void __fastcall SetMode(TSynchronizeMode value);
  TSynchronizeMode __fastcall GetMode();
  void __fastcall SetParams(int value);
  int __fastcall GetParams();
  void __fastcall SetSaveSettings(bool value);
  bool __fastcall GetSaveSettings();
  void __fastcall SetOptions(int value);
  void __fastcall SetCopyParams(const TCopyParamType & value);
  TCopyParamType __fastcall GetCopyParams();
  void __fastcall CopyParamClick(TObject * Sender);
  int __fastcall ActualCopyParamAttrs();
  void __fastcall CopyParamListPopup(TPoint P, int AdditionalOptions);

public:
  __fastcall TFullSynchronizeDialog(TComponent* Owner);
  virtual __fastcall ~TFullSynchronizeDialog();

  bool __fastcall Execute();

  __property UnicodeString RemoteDirectory = { read = GetRemoteDirectory, write = SetRemoteDirectory };
  __property UnicodeString LocalDirectory = { read = GetLocalDirectory, write = SetLocalDirectory };
  __property int Params = { read = GetParams, write = SetParams };
  __property TSynchronizeMode Mode = { read = GetMode, write = SetMode };
  __property bool SaveSettings = { read = GetSaveSettings, write = SetSaveSettings };
  __property bool SaveMode = { read = FSaveMode, write = FSaveMode };
  __property int Options = { read = FOptions, write = SetOptions };
  __property TUsableCopyParamAttrs CopyParamAttrs = { read = FCopyParamAttrs, write = FCopyParamAttrs };
  __property TCopyParamType CopyParams = { read = GetCopyParams, write = SetCopyParams };

protected:
  void __fastcall UpdateControls();
};
//---------------------------------------------------------------------------
#endif
