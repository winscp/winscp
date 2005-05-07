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
#include "TB2Dock.hpp"
#include "TB2Item.hpp"
#include "TB2Toolbar.hpp"
#include "TBX.hpp"
#include <ActnList.hpp>
#include <Menus.hpp>
#include "TBXStatusBars.hpp"
//---------------------------------------------------------------------------
class TLogForm : public TForm
{
__published:	// IDE-managed Components
  TTBXStatusBar *StatusBar;
  TTBXDock *TopDock;
  TTBXToolbar *Toolbar;
  TTBXItem *TBXItem1;
  TTBXSeparatorItem *TBXSeparatorItem1;
  TTBXItem *TBXItem2;
  TTBXItem *TBXItem3;
  TTBXItem *TBXItem4;
  TTBXSeparatorItem *TBXSeparatorItem2;
  TTBXItem *TBXItem5;
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:
  Boolean FFormRestored;
  TLogMemo * FLogMemo;
  TSessionLog * FSessionLog;
  void __fastcall SetLogMemo(TLogMemo * value);
  void __fastcall SetSessionLog(TSessionLog * value);
protected:
  virtual void __fastcall CreateParams(TCreateParams & Params);
  void __fastcall LogMemoChange(TObject * Sender);
  void __fastcall UpdateControls();
public:
  virtual __fastcall ~TLogForm();
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
