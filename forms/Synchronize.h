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
  TCheckBox *SynchronizeNoConfirmationCheck;
  TButton *LocalDirectoryBrowseButton;
  TCheckBox *SaveSettingsCheck;
  TCheckBox *SynchronizeExistingOnlyCheck;
  TButton *StartButton;
  TButton *MinimizeButton;
  TButton *TransferPreferencesButton;
  TCheckBox *SynchronizeRecursiveCheck;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall LocalDirectoryBrowseButtonClick(TObject *Sender);
  void __fastcall DirectoryEditKeyDown(TObject *Sender, WORD &Key,
    TShiftState Shift);
  void __fastcall TransferPreferencesButtonClick(TObject *Sender);
  void __fastcall StartButtonClick(TObject *Sender);
  void __fastcall StopButtonClick(TObject *Sender);
  void __fastcall MinimizeButtonClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);

private:
  TSynchronizeParamType FParams;
  TSynchronizeStartStopEvent FOnStartStop;
  bool FSynchronizing;
  bool FMinimizedByMe;
  bool FAbort;
  bool FClose;

  void __fastcall SetParams(const TSynchronizeParamType& value);
  TSynchronizeParamType __fastcall GetParams();
  void __fastcall SetSaveSettings(bool value);
  bool __fastcall GetSaveSettings();

protected:
  void __fastcall DoStartStop(bool Start);
  void __fastcall DoAbort(TObject * Sender, bool Close);
  void __fastcall Stop();
  virtual void __fastcall Dispatch(void * Message);

public:
  __fastcall TSynchronizeDialog(TComponent* Owner);

  bool __fastcall Execute();

  __property TSynchronizeParamType Params = { read = GetParams, write = SetParams };
  __property TSynchronizeStartStopEvent OnStartStop  = { read=FOnStartStop, write=FOnStartStop };
  __property bool SaveSettings = { read = GetSaveSettings, write = SetSaveSettings };

protected:
  void __fastcall UpdateControls();
};
//---------------------------------------------------------------------------
#endif
