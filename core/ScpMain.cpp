//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <stdio.h>

#include "ScpMain.h"

#define PUTTY_DO_GLOBALS
#include "PuttyIntf.h"

#include "Common.h"
#include "Interface.h"
#include "Configuration.h"
#include "Terminal.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TConfiguration *Configuration;
TStoredSessionList *StoredSessions;
//---------------------------------------------------------------------------
#ifdef SCP_CONSOLE
/* TODO 1 : Won't be needed (while debuggin we hang TSessionLog::OnAddLine to it) */
//class TCallExceptionClass;
//extern TCallExceptionClass *CallExceptionClass;
class TCallExceptionClass : public TObject
{
public:
  void __fastcall PrintLine(TObject* Sender, const AnsiString Line);
  void __fastcall QueryUser(TObject* Sender, const AnsiString Query, int Answers,
    const Boolean FatalAbort, int & Answer);
  void __fastcall ShowException(TObject* Sender, Exception* E);
};
//---------------------------------------------------------------------------
TCallExceptionClass *CallExceptionClass;
//---------------------------------------------------------------------------
void __fastcall TCallExceptionClass::ShowException(TObject* Sender, Exception* E)
{
  ShowExtendedException(E, Sender);
}
//---------------------------------------------------------------------------
void __fastcall TCallExceptionClass::PrintLine(TObject* Sender, const AnsiString Line)
{
  // Used for debugging (e.g. hanged to TSessionLog::OnAddLine);
  puts(Line.c_str());
}
//---------------------------------------------------------------------------
void __fastcall TCallExceptionClass::QueryUser(TObject* Sender, const AnsiString Query,
  int Answers, const bool FatalAbort, int & Answer)
{
  /* TODO : QueryUser: user FatalAbort parametr */
  Answer = MessageDialog(Query, mtConfirmation, Answers, 0);
}
#endif
//---------------------------------------------------------------------------
void Initialize(const AnsiString IniFileName)
{
  // initialize default seed path value same way as putty does (only change filename)
  putty_get_seedpath();
  flags = FLAG_VERBOSE; // verbose log
  Randomize();
#ifdef SCP_CONSOLE
  CallExceptionClass = new TCallExceptionClass();
  Application->OnException = CallExceptionClass->ShowException;
#endif
  Configuration = new TConfiguration();
  if (!IniFileName.IsEmpty()) Configuration->IniFileStorageName = IniFileName;
  CATCH( Configuration->Load(); );
  StoredSessions = new TStoredSessionList();
  CATCH( StoredSessions->Load(); );
}
//---------------------------------------------------------------------------
void Finalize()
{
  delete StoredSessions;
  StoredSessions = NULL;
  Configuration->Save();
  delete Configuration;
  Configuration = NULL;
#ifdef SCP_CONSOLE
  Application->OnException = NULL;
  delete CallExceptionClass;
  CallExceptionClass = NULL;
#endif
}
//---------------------------------------------------------------------------
static AnsiString TranslateRegKey(AnsiString RegKey)
{
  // This is called once even before Configuration is created
  // (see Initialize()) from get_seedpath() (winstore.c).
  // In that case we don't mind that it's looked for in Putty regkey,
  // it's even better.
  if (Configuration)
  {
    int PuttyKeyLen = Configuration->PuttyRegistryStorageKey.Length();
    assert(RegKey.SubString(1, PuttyKeyLen) == Configuration->PuttyRegistryStorageKey);
    RegKey = Configuration->RegistryStorageKey +
      RegKey.SubString(PuttyKeyLen + 1, RegKey.Length() - PuttyKeyLen);
  }
  return RegKey;
}
//---------------------------------------------------------------------------
long RegOpenWinSCPKey(HKEY Key, const char * SubKey,	HKEY * Result)
{
  return RegOpenKey(Key, TranslateRegKey(SubKey).c_str(), Result);
}
//---------------------------------------------------------------------------
long RegCreateWinSCPKey(HKEY Key, const char * SubKey,	HKEY * Result)
{
  return RegCreateKey(Key, TranslateRegKey(SubKey).c_str(), Result);
}
//---------------------------------------------------------------------------

