//---------------------------------------------------------------------------
#ifndef ToolsH
#define ToolsH

#include <comctrls.hpp>
#include <WinInterface.h>
//---------------------------------------------------------------------------
void __fastcall CenterFormOn(TForm * Form, TControl * CenterOn);
AnsiString __fastcall GetCoolbarLayoutStr(TCoolBar * CoolBar);
void __fastcall LoadCoolbarLayoutStr(TCoolBar * CoolBar, AnsiString LayoutStr);
void __fastcall SetCoolBandsMinWidth(TCoolBar * CoolBar);
bool __fastcall ExecuteShellAndWait(const AnsiString Path, const AnsiString Params);
void __fastcall CreateDesktopShortCut(const AnsiString &Name,
  const AnsiString &File, const AnsiString & Params, const AnsiString & Description,
  int SpecialFolder = -1);
AnsiString __fastcall GetListViewStr(TListView * ListView);
void __fastcall LoadListViewStr(TListView * ListView, AnsiString LayoutStr);
TFontStyles __fastcall IntToFontStyles(int value);
int __fastcall FontStylesToInt(const TFontStyles value);
void __fastcall ValidateMaskEdit(TComboBox * Edit);
//---------------------------------------------------------------------------
namespace Discmon
{
class TDiscMonitor;
}
//---------------------------------------------------------------------------
class TSynchronizeController;
class TSynchronizeParamType;
typedef void __fastcall (__closure * TSynchronizeEvent)
  (TSynchronizeController * Sender, const AnsiString LocalDirectory,
   const AnsiString RemoteDirectory, const TSynchronizeParamType & Params);
//---------------------------------------------------------------------------
class TSynchronizeController
{
public:
  __fastcall TSynchronizeController(TSynchronizeEvent AOnSynchronize);

  void __fastcall StartStop(TObject * Sender, bool Start,
    const TSynchronizeParamType & Params, TSynchronizeAbortEvent OnAbort);

  __property bool Changed = { read = FChanged }; 

private:
  TSynchronizeEvent FOnSynchronize;
  TSynchronizeParamType * FSynchronizeParams;
  Discmon::TDiscMonitor * FSynchronizeMonitor;
  TSynchronizeAbortEvent FSynchronizeAbort;
  bool FChanged;

  void __fastcall SynchronizeChange(TObject * Sender, const AnsiString Directory);
  void __fastcall SynchronizeAbort(bool Close);
  void __fastcall SynchronizeInvalid(TObject * Sender, const AnsiString Directory);
};
//---------------------------------------------------------------------------
#endif
