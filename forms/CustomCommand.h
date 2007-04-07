//---------------------------------------------------------------------------
#ifndef CustomCommandH
#define CustomCommandH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "HistoryComboBox.hpp"
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
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall CommandEditGetData(THistoryComboBox *Sender,
          Pointer &Data);
  void __fastcall CommandEditSetData(THistoryComboBox *Sender,
          Pointer Data);

private:
  TCustomCommandsMode FMode;
  int FOptions;
  int FParams;
  AnsiString FOrigDescription;
  const TCustomCommands * FCustomCommands;
  TCustomCommandValidate FOnValidate;

  void __fastcall SetCommand(AnsiString value);
  AnsiString __fastcall GetCommand();
  void __fastcall SetDescription(AnsiString value);
  AnsiString __fastcall GetDescription();
  void __fastcall SetParams(int value);
  int __fastcall GetParams();

protected:
  void __fastcall UpdateControls();

public:
  __fastcall TCustomCommandDialog(TComponent* Owner, unsigned int Options);

  bool __fastcall Execute();

  __property AnsiString Command = { read = GetCommand, write = SetCommand };
  __property AnsiString Description = { read = GetDescription, write = SetDescription };
  __property int Params = { read = GetParams, write = SetParams };
  __property TCustomCommandsMode Mode = { read = FMode, write = FMode };
  __property const TCustomCommands * CustomCommands = { read = FCustomCommands, write = FCustomCommands };
  __property TCustomCommandValidate OnValidate = { read = FOnValidate, write = FOnValidate };
};
//---------------------------------------------------------------------------
#endif
