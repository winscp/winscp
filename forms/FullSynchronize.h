//---------------------------------------------------------------------------
#ifndef FullSynchronizeH
#define FullSynchronizeH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <XPGroupBox.hpp>
#include <HistoryComboBox.hpp>

#include <WinInterface.h>
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
  TRadioButton *SynchronizeBothButton;
  TRadioButton *SynchronizeRemoteButton;
  TRadioButton *SynchronizeLocalButton;
  TCheckBox *SynchronizeDeleteCheck;
  TCheckBox *SynchronizeNoConfirmationCheck;
  TButton *LocalDirectoryBrowseButton;
  TCheckBox *SaveSettingsCheck;
  TCheckBox *SynchronizeExistingOnlyCheck;
  TButton *TransferPreferencesButton;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall LocalDirectoryBrowseButtonClick(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall DirectoryEditKeyDown(TObject *Sender, WORD &Key,
    TShiftState Shift);
  void __fastcall TransferPreferencesButtonClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  
private:
  int FParams;
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

public:
  __fastcall TFullSynchronizeDialog(TComponent* Owner);

  bool __fastcall Execute();

  __property AnsiString RemoteDirectory = { read = GetRemoteDirectory, write = SetRemoteDirectory };
  __property AnsiString LocalDirectory = { read = GetLocalDirectory, write = SetLocalDirectory };
  __property int Params = { read = GetParams, write = SetParams };
  __property TSynchronizeMode Mode = { read = GetMode, write = SetMode };
  __property bool SaveSettings = { read = GetSaveSettings, write = SetSaveSettings };

protected:
  void __fastcall UpdateControls();
};
//---------------------------------------------------------------------------
#endif
