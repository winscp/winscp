//---------------------------------------------------------------------------
#ifndef EventHandlerH
#define EventHandlerH

#include "CustomScpExplorer.h"
//---------------------------------------------------------------------------
class TEventHandler : public TObject  {
private:
  TCustomScpExplorerForm * FScpExplorer;
protected:
  void __fastcall DoConfigurationChange();
  void __fastcall ApplicationException(TObject * Sender, Exception * E);
  void __fastcall DoApplicationException(Exception * E, TObject * Sender);
  int __fastcall DoQueryUser(const AnsiString Query, TStrings * MoreMessages,
    int Answers, int Params, TQueryType Type);
public:
  __fastcall TEventHandler();
  virtual __fastcall ~TEventHandler();
  void __fastcall ConfigurationChange(TObject * Sender);

  void __fastcall TerminalQueryUser(TObject * Sender, const AnsiString Query,
    TStrings * MoreMessages, int Answers, int Params,
    int & Answer, TQueryType Type);
  __property TCustomScpExplorerForm * ScpExplorer = { read = FScpExplorer, write = FScpExplorer };
};
//---------------------------------------------------------------------------
#endif
