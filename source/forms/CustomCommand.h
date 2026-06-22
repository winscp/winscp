//---------------------------------------------------------------------------
#ifndef CustomCommandH
#define CustomCommandH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "HistoryComboBox.h"
//---------------------------------------------------------------------------
class TCustomCommands;
//---------------------------------------------------------------------------
class TCustomCommandDialog : public TForm
{
__published:
  TGroupBox *Group;
  TButton *OkButton;
  TButton *CancelButton;
  TLabel *DescriptionLabel;
  TEdit *DescriptionEdit;
  TLabel *Label1;
  THistoryComboBox *CommandEdit;
  TCheckBox *ApplyToDirectoriesCheck;
  TCheckBox *RecursiveCheck;
  TRadioButton *LocalCommandButton;
  TRadioButton *RemoteCommandButton;
  TCheckBox *ShowResultsCheck;
  TButton *HelpButton;
  TCheckBox *CopyResultsCheck;
  TStaticText *HintText;
  TLabel *ShortCutLabel;
  TComboBox *ShortCutCombo;
  TCheckBox *RemoteFilesCheck;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall CommandEditGetData(THistoryComboBox *Sender,
    TObject *& Data);
  void __fastcall CommandEditSetData(THistoryComboBox *Sender,
    TObject * Data);
  void __fastcall FormShow(TObject *Sender);

private:
  TCustomCommandsMode FMode;
  int FParams;
  UnicodeString FOrigDescription;
  const TCustomCommandList * FCustomCommandList;
  TCustomCommandValidate FOnValidate;
  int FOptions;

  void __fastcall SetParams(int value);
  int __fastcall GetParams();
  void __fastcall GetCommand(TCustomCommandType & Command);

protected:
  void __fastcall UpdateControls();

  INTERFACE_HOOK

public:
  __fastcall TCustomCommandDialog(TComponent* Owner,
    const TCustomCommandList * CustomCommandList, TCustomCommandsMode Mode,
    int Options, TCustomCommandValidate OnValidate, const TShortCuts * ShortCuts);

  bool __fastcall Execute(TCustomCommandType & Command);
};
//---------------------------------------------------------------------------
#endif
