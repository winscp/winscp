//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <Interface.h>
#include <Log.h>
#include <Net.h>
#include <ScpMain.h>
#include <TextsWin.h>

#include "EventHandler.h"
//---------------------------------------------------------------------------
__fastcall TEventHandler::TEventHandler(): TObject()
{
  FScpExplorer = NULL;
  assert(Application && !Application->OnException);
  Application->OnException = ApplicationException;
}
//---------------------------------------------------------------------------
__fastcall TEventHandler::~TEventHandler()
{
  if (Configuration && (Configuration->OnChange == ConfigurationChange))
    Configuration->OnChange = NULL;
  assert(Application && (Application->OnException == ApplicationException));
  Application->OnException = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TEventHandler::ConfigurationChange(TObject * /*Sender*/)
{
  DoConfigurationChange();
}
//---------------------------------------------------------------------------
void __fastcall TEventHandler::DoConfigurationChange()
{
  assert(Configuration);

  if (!Application->Terminated && Configuration->Logging &&
      (Configuration->LogView == lvWindow))
  {
    RequireLogForm(LogMemo);
  }
    else
  {
    FreeLogForm();
  }

  if (CurrentSSH)
  {
    assert(CurrentSSH->Log);
    CurrentSSH->Log->ReflectSettings();
  }

  if (ScpExplorer) ScpExplorer->ConfigurationChanged();
}
//---------------------------------------------------------------------------
void __fastcall TEventHandler::TerminalQueryUser(TObject * /*Sender*/,
  const AnsiString Query, TStrings * MoreMessages, int Answers,
  int Params, int & Answer, TQueryType Type)
{
  Answer = DoQueryUser(Query, MoreMessages, Answers, Params, Type);
}
//---------------------------------------------------------------------------
Integer __fastcall TEventHandler::DoQueryUser(const AnsiString Query,
  TStrings * MoreMessages, int Answers, int Params, TQueryType Type)
{
  AnsiString AQuery = Query;
  if (Params & qpFatalAbort)
  {
    AQuery = FMTLOAD(WARN_FATAL_ERROR, (AQuery));
  }

  int MessageParams = 0;
  if (Params & qpNeverAskAgainCheck)
  {
    MessageParams |= mpNeverAskAgainCheck;
  }
  
  return MoreMessageDialog(AQuery, MoreMessages, Type, Answers, 0, MessageParams);
}
//---------------------------------------------------------------------------
void __fastcall TEventHandler::ApplicationException(TObject * Sender, Exception * E)
{
  DoApplicationException(E, Sender);
}
//---------------------------------------------------------------------------
void __fastcall TEventHandler::DoApplicationException(Exception * E, TObject * Sender)
{
  ShowExtendedException(E, Sender);
}
