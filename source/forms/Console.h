//----------------------------------------------------------------------------
#ifndef ConsoleH
#define ConsoleH
//----------------------------------------------------------------------------
#include "HistoryComboBox.hpp"
#include "PathLabel.hpp"
#include <System.Classes.hpp>
#include <Vcl.ActnList.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.StdActns.hpp>
#include <Vcl.StdCtrls.hpp>
//----------------------------------------------------------------------------
#include "WinInterface.h"
#include <Terminal.h>
#include "PngImageList.hpp"
#include <Vcl.Imaging.pngimage.hpp>
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
  TButton *HelpButton;
  TPngImageList *Images;
  TPopupMenu *PopupMenu;
  TMenuItem *SelectAllItem;
  TMenuItem *CopyItem;
  TMenuItem *N1;
  TMenuItem *AdjustWindowItem;
  TActionList *ActionList;
  TEditCopy *EditCopy;
  TEditSelectAll *EditSelectAll;
  TAction *AdjustWindow;
  TImage *Image;
  void __fastcall ExecuteButtonClick(TObject *Sender);
  void __fastcall CommandEditChange(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall ActionListExecute(TBasicAction *Action, bool &Handled);
  void __fastcall ActionListUpdate(TBasicAction *Action, bool &Handled);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall OutputMemoContextPopup(TObject *Sender, TPoint &MousePos,
          bool &Handled);

private:
  TTerminal * FTerminal;
  TTerminal * FLastTerminal;
  TNotifyEvent FOldChangeDirectory;
  TNotifyEvent FPrevTerminalClose;
  TRect FAutoBounds;
  bool FClearExceptionOnFail;
  bool FDirectoryChanged;

  void __fastcall DoExecuteCommand();
  void __fastcall ExecuteCommand();
  void __fastcall SetTerminal(TTerminal * value);
  void __fastcall TerminalClose(TObject * Sender);
  void __fastcall AddLine(const UnicodeString & Line, TCaptureOutputType OutputType);

protected:
  void __fastcall DoChangeDirectory(TObject * Sender);
  void __fastcall UpdateControls();
  virtual void __fastcall CreateParams(TCreateParams & Params);
  void __fastcall DoAdjustWindow();

public:
  virtual __fastcall ~TConsoleDialog();
    virtual __fastcall TConsoleDialog(TComponent* AOwner);
  bool __fastcall Execute(const UnicodeString Command = L"",
    const TStrings * Log = NULL);
  __property TTerminal * Terminal = { read = FTerminal, write = SetTerminal };
};
//----------------------------------------------------------------------------
#endif
