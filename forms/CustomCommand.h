//---------------------------------------------------------------------------
#ifndef CustomCommandH
#define CustomCommandH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "XPThemes.hpp"
//---------------------------------------------------------------------------
class TCustomCommands;
//---------------------------------------------------------------------------
class TCustomCommandDialog : public TForm
{
__published:
  TXPGroupBox *Group;
  TButton *OkButton;
  TButton *CancelButton;
  TLabel *FileNameLabel;
  TEdit *DescriptionEdit;
  TLabel *Label1;
  TEdit *CommandEdit;
  TCheckBox *ApplyToDirectoriesCheck;
  TCheckBox *RecursiveCheck;
  TLabel *CustomCommandsPatternsLabel;
  TRadioButton *LocalCommandButton;
  TRadioButton *RemoteCommandButton;
  TCheckBox *ShowResultsCheck;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall PathEditsKeyDown(TObject *Sender, WORD &Key,
    TShiftState Shift);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);

private:
  bool FEdit;
  int FParams;
  AnsiString FOrigDescription;
  const TCustomCommands * FCustomCommands;

  void __fastcall SetCommand(AnsiString value);
  AnsiString __fastcall GetCommand();
  void __fastcall SetDescription(AnsiString value);
  AnsiString __fastcall GetDescription();
  void __fastcall SetParams(int value);
  int __fastcall GetParams();

protected:
  void __fastcall UpdateControls();

public:
  __fastcall TCustomCommandDialog(TComponent* Owner);

  bool __fastcall Execute();

  __property AnsiString Command = { read = GetCommand, write = SetCommand };
  __property AnsiString Description = { read = GetDescription, write = SetDescription };
  __property int Params = { read = GetParams, write = SetParams };
  __property bool Edit = { read = FEdit, write = FEdit };
  __property const TCustomCommands * CustomCommands = { read = FCustomCommands, write = FCustomCommands };
};
//---------------------------------------------------------------------------
#endif
