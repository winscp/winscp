//---------------------------------------------------------------------------
#ifndef LogH
#define LogH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <LogMemo.h>
#include <ToolWin.hpp>
//---------------------------------------------------------------------------
class TLogForm : public TForm
{
__published:	// IDE-managed Components
  TStatusBar *StatusBar;
  TCoolBar *TopCoolBar;
  TToolBar *ToolBar;
  TToolButton *ToolButton1;
  TToolButton *ToolButton3;
  TToolButton *ToolButton2;
  TToolButton *ToolButton4;
  TToolButton *ToolButton5;
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
  void __fastcall StatusBarResize(TObject *Sender);
private:
  Boolean FFormRestored;
  Boolean FStatusBarFile;
  TLogMemo * FLogMemo;
  TSessionLog * FSessionLog;
  void __fastcall SetLogMemo(TLogMemo * value);
  void __fastcall SetSessionLog(TSessionLog * value);	// User declarations
protected:
  virtual void __fastcall CreateParams(TCreateParams & Params);
  void __fastcall LogMemoChange(TObject * Sender);
  void __fastcall SetStatusBarText(AnsiString Text);
  void __fastcall UpdateControls();
public:
  virtual __fastcall ~TLogForm();		// User declarations
  __fastcall TLogForm(TComponent* Owner);
  __property TLogMemo * LogMemo = { read = FLogMemo, write = SetLogMemo };
  __property TSessionLog * SessionLog = { read = FSessionLog, write = SetSessionLog };
};
//---------------------------------------------------------------------------
extern PACKAGE TLogForm *LogForm;
//---------------------------------------------------------------------------
TLogForm * __fastcall CreateLogForm(TLogMemo *ALogMemo);
TLogForm * __fastcall RequireLogForm(TLogMemo *ALogMemo);
void __fastcall FreeLogForm();
//---------------------------------------------------------------------------
#endif
