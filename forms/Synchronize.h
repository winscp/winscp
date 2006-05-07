//---------------------------------------------------------------------------
#ifndef SynchronizeH
#define SynchronizeH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <XPThemes.hpp>
#include <HistoryComboBox.hpp>

#include <WinInterface.h>
#include "GrayedCheckBox.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TSynchronizeDialog : public TForm
{
__published:
  TXPGroupBox *DirectoriesGroup;
  TButton *StopButton;
  TButton *CancelButton;
  TLabel *LocalDirectoryLabel;
  TLabel *RemoteDirectoryLabel;
  THistoryComboBox *RemoteDirectoryEdit;
  THistoryComboBox *LocalDirectoryEdit;
  TXPGroupBox *OptionsGroup;
  TCheckBox *SynchronizeDeleteCheck;
  TButton *LocalDirectoryBrowseButton;
  TCheckBox *SaveSettingsCheck;
  TCheckBox *SynchronizeExistingOnlyCheck;
  TButton *StartButton;
  TButton *MinimizeButton;
  TButton *TransferSettingsButton;
  TCheckBox *SynchronizeRecursiveCheck;
  TGrayedCheckBox *SynchronizeSynchronizeCheck;
  TXPGroupBox *CopyParamGroup;
  TLabel *CopyParamLabel;
  TButton *HelpButton;
  TCheckBox *SynchronizeSelectedOnlyCheck;
  TPanel *LogPanel;
  TListView *LogView;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall LocalDirectoryBrowseButtonClick(TObject *Sender);
  void __fastcall TransferSettingsButtonClick(TObject *Sender);
  void __fastcall StartButtonClick(TObject *Sender);
  void __fastcall StopButtonClick(TObject *Sender);
  void __fastcall MinimizeButtonClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall CopyParamGroupContextPopup(TObject *Sender,
          TPoint &MousePos, bool &Handled);
  void __fastcall CopyParamGroupDblClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall LogViewKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
  void __fastcall FormKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);

private:
  TSynchronizeParamType FParams;
  TSynchronizeStartStopEvent FOnStartStop;
  TGetSynchronizeOptionsEvent FOnGetOptions;
  int FOptions;
  int FCopyParamAttrs;
  bool FSynchronizing;
  bool FMinimizedByMe;
  bool FAbort;
  bool FClose;
  bool FStartImmediatelly;
  TCopyParamType FCopyParams;
  TPopupMenu * FPresetsMenu;
  AnsiString FPreset;
  TSynchronizeOptions * FSynchronizeOptions;
  static const MaxLogItems;

  void __fastcall SetParams(const TSynchronizeParamType& value);
  TSynchronizeParamType __fastcall GetParams();
  void __fastcall SetSaveSettings(bool value);
  bool __fastcall GetSaveSettings();
  void __fastcall SetCopyParams(const TCopyParamType & value);
  TCopyParamType __fastcall GetCopyParams();
  void __fastcall SetOptions(int value);

protected:
  void __fastcall DoStartStop(bool Start, bool Synchronize);
  void __fastcall DoAbort(TObject * Sender, bool Close);
  void __fastcall DoLog(TSynchronizeController * Controller, TSynchronizeLogEntry Entry,
    const AnsiString Message);
  void __fastcall Stop();
  virtual void __fastcall Dispatch(void * Message);
  void __fastcall CopyParamClick(TObject * Sender);
  void __fastcall ClearLog();
  void __fastcall CopyLog();
  int __fastcall ActualCopyParamAttrs();

public:
  __fastcall TSynchronizeDialog(TComponent * Owner,
    TSynchronizeStartStopEvent OnStartStop, TGetSynchronizeOptionsEvent OnGetOptions,
    bool StartImmediatelly);
  virtual __fastcall ~TSynchronizeDialog();

  bool __fastcall Execute();

  __property TSynchronizeParamType Params = { read = GetParams, write = SetParams };
  __property bool SaveSettings = { read = GetSaveSettings, write = SetSaveSettings };
  __property int Options = { read = FOptions, write = SetOptions };
  __property int CopyParamAttrs = { read = FCopyParamAttrs, write = FCopyParamAttrs };
  __property TCopyParamType CopyParams = { read = GetCopyParams, write = SetCopyParams };

protected:
  void __fastcall UpdateControls();
};
//---------------------------------------------------------------------------
#endif
