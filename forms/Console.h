//----------------------------------------------------------------------------
#ifndef ConsoleH
#define ConsoleH
//----------------------------------------------------------------------------
#include <vcl\System.hpp>
#include <vcl\Windows.hpp>
#include <vcl\SysUtils.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Graphics.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Buttons.hpp>
#include <vcl\ExtCtrls.hpp>
#include <ComCtrls.hpp>
#include <HistoryComboBox.hpp>
#include <PathLabel.hpp>

#include <Terminal.h>
//----------------------------------------------------------------------------
class TConsoleDialog : public TForm
{
__published:
  TMemo *OutputMemo;
  TBevel *Bevel1;
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label4;
  TButton *CancelBtn;
  THistoryComboBox *CommandEdit;
  TButton *ExecuteButton;
  TPathLabel *DirectoryLabel;
  void __fastcall ExecuteButtonClick(TObject *Sender);
  void __fastcall CommandEditChange(TObject *Sender);
  
private:
  TTerminal * FTerminal;
  TTerminal * FLastTerminal;
  TNotifyEvent FOldChangeDirectory;
  TLogAddLineEvent FOldLogAddLine;
  bool FAddOutput;
  TNotifyEvent FPrevTerminalClose;
  
  void __fastcall DoExecuteCommand();
  void __fastcall ExecuteCommand();
  void __fastcall SetTerminal(TTerminal * value);
  void __fastcall TerminalClose(TObject * Sender);
  void __fastcall AddLine(AnsiString Line);

protected:
  void __fastcall DoChangeDirectory(TObject * Sender);
  void __fastcall DoLogAddLine(System::TObject* Sender, const AnsiString AddedLine);
  void __fastcall UpdateControls();
  virtual void __fastcall CreateParams(TCreateParams & Params);

public:
  virtual __fastcall ~TConsoleDialog();
    virtual __fastcall TConsoleDialog(TComponent* AOwner);
  bool __fastcall Execute(const AnsiString Command = "",
    const TStrings * Log = NULL);
  __property TTerminal * Terminal = { read = FTerminal, write = SetTerminal };
};
//----------------------------------------------------------------------------
#endif
