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

#include <WinInterface.h>
//---------------------------------------------------------------------------
class TLoggingFrame : public TFrame
{
__published:
  TCheckBox *LoggingCheck;
  TGroupBox *LoggingGroup;
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
  TStaticText *LogFileNameHintText;
  void __fastcall LogToFileCheckChange(TObject *Sender);
  void __fastcall DataChange(TObject *Sender);
  void __fastcall LogFileNameEditBeforeDialog(TObject *Sender,
          AnsiString &Name, bool &Action);
  void __fastcall LogFileNameEditAfterDialog(TObject *Sender,
          AnsiString &Name, bool &Action);
private:
  bool FEnableLogWindow;
  AnsiString FBeforeDialogPath;

  AnsiString __fastcall GetDefaultLogFileName();
  void __fastcall SetEnableLogWindow(bool value);
public:
  void __fastcall LoadConfiguration();
  void __fastcall SaveConfiguration();
  __fastcall TLoggingFrame(TComponent* Owner);
  __property AnsiString DefaultLogFileName = { read = GetDefaultLogFileName };
  __property bool EnableLogWindow = { read = FEnableLogWindow, write = SetEnableLogWindow };
protected:
  void __fastcall UpdateControls();
};
//---------------------------------------------------------------------------
extern PACKAGE TLoggingFrame *LoggingFrame;
//---------------------------------------------------------------------------
#endif
