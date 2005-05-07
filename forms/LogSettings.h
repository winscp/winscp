//---------------------------------------------------------------------------
#ifndef LogSettingsH
#define LogSettingsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <Mask.hpp>
#include <ComboEdit.hpp>
#include <UpDownEdit.hpp>
#include <XPThemes.hpp>

#include <WinInterface.h>
//---------------------------------------------------------------------------
class TLoggingFrame : public TFrame
{
__published:
  TCheckBox *LoggingCheck;
  TXPGroupBox *LoggingGroup;
  TLabel *LogWindowLinesText;
  TCheckBox *LogToFileCheck;
  TFilenameEdit *LogFileNameEdit;
  TCheckBox *LogShowWindowCheck;
  TRadioButton *LogWindowCompleteButton;
  TRadioButton *LogWindowLinesButton;
  TUpDownEdit *LogWindowLinesEdit;
  TPanel *LogFilePanel;
  TRadioButton *LogFileAppendButton;
  TRadioButton *LogFileOverwriteButton;
  TLabel *LogProtocolLabel;
  TComboBox *LogProtocolCombo;
  void __fastcall LogToFileCheckChange(TObject *Sender);
  void __fastcall DataChange(TObject *Sender);
private:
  TGetDefaultLogFileName FOnGetDefaultLogFileName;
  bool FEnableLogWindow;

  AnsiString __fastcall GetDefaultLogFileName();
  void __fastcall SetEnableLogWindow(bool value);
public:
  void __fastcall LoadConfiguration();
  void __fastcall SaveConfiguration();
  __fastcall TLoggingFrame(TComponent* Owner);
  __property AnsiString DefaultLogFileName = { read = GetDefaultLogFileName };
  __property TGetDefaultLogFileName OnGetDefaultLogFileName = { read = FOnGetDefaultLogFileName, write = FOnGetDefaultLogFileName };
  __property bool EnableLogWindow = { read = FEnableLogWindow, write = SetEnableLogWindow };
protected:
  void __fastcall UpdateControls();
};
//---------------------------------------------------------------------------
extern PACKAGE TLoggingFrame *LoggingFrame;
//---------------------------------------------------------------------------
#endif
