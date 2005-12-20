//---------------------------------------------------------------------------
#ifndef FullSynchronizeH
#define FullSynchronizeH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <XPThemes.hpp>
#include <HistoryComboBox.hpp>

#include <WinInterface.h>
#include <CopyParam.h>
//---------------------------------------------------------------------------
class TFullSynchronizeDialog : public TForm
{
__published:
  TXPGroupBox *DirectoriesGroup;
  TButton *OkButton;
  TButton *CancelButton;
  TLabel *LocalDirectoryLabel;
  TLabel *RemoteDirectoryLabel;
  THistoryComboBox *RemoteDirectoryEdit;
  THistoryComboBox *LocalDirectoryEdit;
  TXPGroupBox *OptionsGroup;
  TCheckBox *SynchronizeDeleteCheck;
  TCheckBox *SynchronizeSelectedOnlyCheck;
  TButton *LocalDirectoryBrowseButton;
  TCheckBox *SynchronizeExistingOnlyCheck;
  TButton *TransferSettingsButton;
  TCheckBox *SynchronizePreviewChangesCheck;
  TXPGroupBox *DirectionGroup;
  TRadioButton *SynchronizeBothButton;
  TRadioButton *SynchronizeRemoteButton;
  TRadioButton *SynchronizeLocalButton;
  TCheckBox *SynchronizeTimestampCheck;
  TXPGroupBox *CompareCriterionsGroup;
  TCheckBox *SynchronizeByTimeCheck;
  TCheckBox *SynchronizeBySizeCheck;
  TCheckBox *SaveSettingsCheck;
  TXPGroupBox *CopyParamGroup;
  TLabel *CopyParamLabel;
  TButton *HelpButton;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall LocalDirectoryBrowseButtonClick(TObject *Sender);
  void __fastcall TransferSettingsButtonClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall SynchronizeByTimeSizeCheckClick(TObject *Sender);
  void __fastcall CopyParamGroupContextPopup(TObject *Sender,
          TPoint &MousePos, bool &Handled);
  void __fastcall CopyParamGroupDblClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  
private:
  int FParams;
  bool FSaveMode;
  TSynchronizeMode FOrigMode;
  int FOptions;
  TUsableCopyParamAttrs FCopyParamAttrs;
  TCopyParamType FCopyParams;
  TPopupMenu * FPresetsMenu;
  AnsiString FPreset;
  AnsiString FSynchronizeBySizeCaption;
  void __fastcall SetRemoteDirectory(const AnsiString value);
  AnsiString __fastcall GetRemoteDirectory();
  void __fastcall SetLocalDirectory(const AnsiString value);
  AnsiString __fastcall GetLocalDirectory();
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

public:
  __fastcall TFullSynchronizeDialog(TComponent* Owner);
  virtual __fastcall ~TFullSynchronizeDialog();

  bool __fastcall Execute();

  __property AnsiString RemoteDirectory = { read = GetRemoteDirectory, write = SetRemoteDirectory };
  __property AnsiString LocalDirectory = { read = GetLocalDirectory, write = SetLocalDirectory };
  __property int Params = { read = GetParams, write = SetParams };
  __property TSynchronizeMode Mode = { read = GetMode, write = SetMode };
  __property bool SaveSettings = { read = GetSaveSettings, write = SetSaveSettings };
  __property bool SaveMode = { read = FSaveMode, write = FSaveMode };
  __property int Options = { read = FOptions, write = SetOptions };
  __property TUsableCopyParamAttrs CopyParamAttrs = { read = FCopyParamAttrs, write = FCopyParamAttrs };
  __property TCopyParamType CopyParams = { read = FCopyParams, write = SetCopyParams };

protected:
  void __fastcall UpdateControls();
};
//---------------------------------------------------------------------------
#endif
