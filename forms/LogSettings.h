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
  TGroupBox *LoggingGroup;
  TLabel *LogWindowLinesText;
  TCheckBox *LogToFileCheck;
  TFilenameEdit *LogFileNameEdit2;
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
  TGroupBox *LogGroup;
  TRadioButton *LoggingOffButton;
  TRadioButton *LoggingOnButton;
  TRadioButton *LoggingActionsButton;
  void __fastcall LogToFileCheckChange(TObject *Sender);
  void __fastcall DataChange(TObject *Sender);
  void __fastcall LogFileNameEdit2BeforeDialog(TObject *Sender,
          AnsiString &Name, bool &Action);
  void __fastcall LogFileNameEdit2AfterDialog(TObject *Sender,
          AnsiString &Name, bool &Action);
  void __fastcall LogFileNameEdit2CreateEditDialog(TObject *Sender,
          TFileDialogKind DialogKind, TOpenDialog *&Dialog);
  void __fastcall LoggingButtonClick(TObject *Sender);
private:
  bool FEnableLogWindow;
  AnsiString FBeforeDialogPath;

  AnsiString __fastcall GetLogFileExt();
  void __fastcall SetEnableLogWindow(bool value);
public:
  __fastcall TLoggingFrame(TComponent* Owner);
  void __fastcall LoadConfiguration();
  void __fastcall SaveConfiguration();
  void __fastcall Init();
  __property bool EnableLogWindow = { read = FEnableLogWindow, write = SetEnableLogWindow };
protected:
  void __fastcall UpdateControls();
};
//---------------------------------------------------------------------------
extern PACKAGE TLoggingFrame *LoggingFrame;
//---------------------------------------------------------------------------
#endif
