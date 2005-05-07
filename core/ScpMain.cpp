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
#include "Net.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define CATCH(command) \
  try {command;} catch (Exception &E) {ShowExtendedException(&E);}
//---------------------------------------------------------------------------
TConfiguration *Configuration;
TStoredSessionList *StoredSessions;
TCriticalSection * CoreCriticalSection = NULL;
CRITICAL_SECTION noise_section;
//---------------------------------------------------------------------------
TCoreGuard::TCoreGuard() : TGuard(CoreCriticalSection)
{
}
//---------------------------------------------------------------------------
#ifdef SCP_CONSOLE
/* TODO 1 : Won't be needed (while debuggin we hang TSessionLog::OnAddLine to it) */
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
  ShowExtendedException(E);
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
TQueryParams::TQueryParams(unsigned int AParams, AnsiString AHelpKeyword)
{
  Params = AParams;
  Aliases = NULL;
  AliasesCount = 0;
  Timer = NULL;
  TimerEvent = NULL;
  TimerAnswers = 0;
  HelpKeyword = AHelpKeyword;
}
//---------------------------------------------------------------------------
void Initialize(const AnsiString IniFileName)
{
  // initialize default seed path value same way as putty does (only change filename)
  putty_get_seedpath();
  InitializeCriticalSection(&noise_section);
  flags = FLAG_VERBOSE | FLAG_SYNCAGENT; // verbose log
  default_protocol = ptSSH;
  default_port = 22;

  Randomize();

  CoreCriticalSection = new TCriticalSection();

#ifdef SCP_CONSOLE
  CallExceptionClass = new TCallExceptionClass();
  Application->OnException = CallExceptionClass->ShowException;
#endif
  Configuration = CreateConfiguration();
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

  delete CoreCriticalSection;
  CoreCriticalSection = NULL;

  DeleteCriticalSection(&noise_section);
}
//---------------------------------------------------------------------------
static long OpenWinSCPKey(HKEY Key, const char * SubKey, HKEY * Result, bool CanCreate)
{
  // This is called once even before Configuration is created
  // (see Initialize()) from get_seedpath() (winstore.c).
  // In that case we don't mind that it's looked for in Putty regkey,
  // it's even better.
  long R;
  if (Configuration != NULL)
  {
    assert(Key == HKEY_CURRENT_USER);

    AnsiString RegKey = SubKey;
    int PuttyKeyLen = Configuration->PuttyRegistryStorageKey.Length();
    assert(RegKey.SubString(1, PuttyKeyLen) == Configuration->PuttyRegistryStorageKey);
    RegKey = RegKey.SubString(PuttyKeyLen + 1, RegKey.Length() - PuttyKeyLen);
    if (!RegKey.IsEmpty())
    {
      assert(RegKey[1] == '\\');
      RegKey.Delete(1, 1);
    }
    // we expect this to be called only from verify_host_key() or store_host_key()
    assert(RegKey == "SshHostKeys");
    
    THierarchicalStorage * Storage = Configuration->CreateScpStorage(false);
    Storage->AccessMode = (CanCreate ? smReadWrite : smRead);
    if (Storage->OpenSubKey(RegKey, CanCreate))
    {
      *Result = static_cast<HKEY>(Storage);
      R = ERROR_SUCCESS;
    }
    else
    {
      delete Storage;
      R = ERROR_CANTOPEN;
    }
  }
  else
  {
    if (CanCreate)
    {
      R = RegCreateKey(Key, SubKey, Result);
    }
    else
    {
      R = RegOpenKey(Key, SubKey, Result);
    }
  }
  return R;
}
//---------------------------------------------------------------------------
long RegOpenWinSCPKey(HKEY Key, const char * SubKey, HKEY * Result)
{
  return OpenWinSCPKey(Key, SubKey, Result, false);
}
//---------------------------------------------------------------------------
long RegCreateWinSCPKey(HKEY Key, const char * SubKey,  HKEY * Result)
{
  return OpenWinSCPKey(Key, SubKey, Result, true);
}
//---------------------------------------------------------------------------
long RegQueryWinSCPValueEx(HKEY Key, const char * ValueName, unsigned long * Reserved,
  unsigned long * Type, unsigned char * Data, unsigned long * DataSize)
{
  long R;
  if (Configuration != NULL)
  {
    THierarchicalStorage * Storage = static_cast<THierarchicalStorage *>(Key);
    if (Storage->ValueExists(ValueName))
    {
      AnsiString Value;
      Value = Storage->ReadStringRaw(ValueName, "");
      assert(Type != NULL);
      *Type = REG_SZ;
      char * DataStr = reinterpret_cast<char *>(Data);
      strncpy(DataStr, Value.c_str(), *DataSize);
      DataStr[*DataSize - 1] = '\0';
      *DataSize = strlen(DataStr);
      R = ERROR_SUCCESS;
    }
    else
    {
      R = ERROR_READ_FAULT;
    }
  }
  else
  {
    R = RegQueryValueEx(Key, ValueName, Reserved, Type, Data, DataSize);
  }
  return R;
}
//---------------------------------------------------------------------------
long RegSetWinSCPValueEx(HKEY Key, const char * ValueName, unsigned long Reserved,
  unsigned long Type, const unsigned char * Data, unsigned long DataSize)
{
  long R;
  if (Configuration != NULL)
  {
    assert(Type == REG_SZ);
    THierarchicalStorage * Storage = static_cast<THierarchicalStorage *>(Key);
    AnsiString Value(reinterpret_cast<const char*>(Data), DataSize - 1);
    Storage->WriteStringRaw(ValueName, Value);
    R = ERROR_SUCCESS;
  }
  else
  {
    R = RegSetValueEx(Key, ValueName, Reserved, Type, Data, DataSize);
  }
  return R;
}
//---------------------------------------------------------------------------
long RegCloseWinSCPKey(HKEY Key)
{
  long R;
  if (Configuration != NULL)
  {
    THierarchicalStorage * Storage = static_cast<THierarchicalStorage *>(Key);
    delete Storage;
    R = ERROR_SUCCESS;
  }
  else
  {
    R = RegCloseKey(Key);
  }
  return R;
}
//---------------------------------------------------------------------------





