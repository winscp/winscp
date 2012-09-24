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
  TFilenameEdit *LogFileNameEdit3;
  TCheckBox *LogShowWindowCheck;
  TRadioButton *LogWindowCompleteButton;
  TRadioButton *LogWindowLinesButton;
  TUpDownEdit *LogWindowLinesEdit;
  TPanel *LogFilePanel;
  TRadioButton *LogFileAppendButton;
  TRadioButton *LogFileOverwriteButton;
  TComboBox *LogProtocolCombo;
  TStaticText *LogFileNameHintText;
  TGroupBox *ActionsLoggingGroup;
  TFilenameEdit *ActionsLogFileNameEdit;
  TStaticText *ActionsLogFileNameHintText;
  TCheckBox *EnableActionsLoggingCheck;
  TCheckBox *EnableLoggingCheck;
  void __fastcall DataChange(TObject *Sender);
  void __fastcall LogFileNameEditBeforeDialog(TObject *Sender,
          UnicodeString &Name, bool &Action);
  void __fastcall LogFileNameEditAfterDialog(TObject *Sender,
          UnicodeString &Name, bool &Action);
  void __fastcall LogFileNameEditCreateEditDialog(TObject *Sender,
          TFileDialogKind DialogKind, TOpenDialog *&Dialog);
private:
  bool FEnableLogWindow;
  UnicodeString FBeforeDialogPath;

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
