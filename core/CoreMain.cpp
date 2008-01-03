//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "CoreMain.h"

#include "Common.h"
#include "Interface.h"
#include "Configuration.h"
#include "PuttyIntf.h"
#ifndef NO_FILEZILLA
#include "FileZillaIntf.h"
#endif
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TConfiguration * Configuration = NULL;
TStoredSessionList * StoredSessions = NULL;
//---------------------------------------------------------------------------
TQueryParams::TQueryParams(unsigned int AParams, AnsiString AHelpKeyword)
{
  Params = AParams;
  Aliases = NULL;
  AliasesCount = 0;
  Timer = 0;
  TimerEvent = NULL;
  TimerMessage = "";
  TimerAnswers = 0;
  Timeout = 0;
  TimeoutAnswer = 0;
  HelpKeyword = AHelpKeyword;
}
//---------------------------------------------------------------------------
void CoreInitialize()
{
  // configuration needs to be created before putty is initialized ...
  Configuration = CreateConfiguration();

  PuttyInitialize();
  #ifndef NO_FILEZILLA
  TFileZillaIntf::Initialize();
  #endif

  // ... but some pieces of configuration can be initialized only afterwards
  Configuration->Initialize();

  Randomize();

  try
  {
    Configuration->Load();
  }
  catch (Exception & E)
  {
    ShowExtendedException(&E);
  }

  StoredSessions = new TStoredSessionList();

  try
  {
    StoredSessions->Load();
  }
  catch (Exception & E)
  {
    ShowExtendedException(&E);
  }
}
//---------------------------------------------------------------------------
void CoreFinalize()
{
  try
  {
    // only modified, implicit
    Configuration->Save(false, false);
  }
  catch(Exception & E)
  {
    ShowExtendedException(&E);
  }

  delete StoredSessions;
  StoredSessions = NULL;
  delete Configuration;
  Configuration = NULL;

  #ifndef NO_FILEZILLA
  TFileZillaIntf::Finalize();
  #endif
  PuttyFinalize();
}
//---------------------------------------------------------------------------
void CoreSetResourceModule(void * ResourceHandle)
{
  #ifndef NO_FILEZILLA
  TFileZillaIntf::SetResourceModule(ResourceHandle);
  #else
  USEDPARAM(ResourceHandle);
  #endif
}
//---------------------------------------------------------------------------
