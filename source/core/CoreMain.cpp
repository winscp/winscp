//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "CoreMain.h"

#include "Common.h"
#include "Interface.h"
#include "Configuration.h"
#include "PuttyIntf.h"
#include "Cryptography.h"
#include <DateUtils.hpp>
#ifndef NO_FILEZILLA
#include "FileZillaIntf.h"
#endif
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TConfiguration * Configuration = NULL;
TStoredSessionList * StoredSessions = NULL;
//---------------------------------------------------------------------------
TQueryButtonAlias::TQueryButtonAlias()
{
  OnClick = NULL;
  GroupWith = -1;
}
//---------------------------------------------------------------------------
TQueryParams::TQueryParams(unsigned int AParams, UnicodeString AHelpKeyword)
{
  Params = AParams;
  Aliases = NULL;
  AliasesCount = 0;
  Timer = 0;
  TimerEvent = NULL;
  TimerMessage = L"";
  TimerAnswers = 0;
  TimerQueryType = static_cast<TQueryType>(-1);
  Timeout = 0;
  TimeoutAnswer = 0;
  NoBatchAnswers = 0;
  HelpKeyword = AHelpKeyword;
}
//---------------------------------------------------------------------------
TQueryParams::TQueryParams(const TQueryParams & Source)
{
  Assign(Source);
}
//---------------------------------------------------------------------------
void TQueryParams::Assign(const TQueryParams & Source)
{
  *this = Source;
}
//---------------------------------------------------------------------------
bool __fastcall IsAuthenticationPrompt(TPromptKind Kind)
{
  return
    (Kind == pkUserName) || (Kind == pkPassphrase) || (Kind == pkTIS) ||
    (Kind == pkCryptoCard) || (Kind == pkKeybInteractive) ||
    (Kind == pkPassword) || (Kind == pkNewPassword);
}
//---------------------------------------------------------------------------
bool __fastcall IsPasswordPrompt(TPromptKind Kind, TStrings * Prompts)
{
  return
    (Prompts->Count == 1) && FLAGCLEAR(int(Prompts->Objects[0]), pupEcho) &&
    ((Kind == pkPassword) || (Kind == pkPassphrase) || (Kind == pkKeybInteractive) ||
     (Kind == pkTIS) || (Kind == pkCryptoCard));
}
//---------------------------------------------------------------------------
void CoreInitialize()
{
  Randomize();
  CryptographyInitialize();

  // configuration needs to be created and loaded before putty is initialized,
  // so that random seed path is known
  Configuration = CreateConfiguration();

  try
  {
    Configuration->Load();
  }
  catch (Exception & E)
  {
    ShowExtendedException(&E);
  }

  PuttyInitialize();
  #ifndef NO_FILEZILLA
  TFileZillaIntf::Initialize();
  #endif

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
    Configuration->Save();
  }
  catch(Exception & E)
  {
    ShowExtendedException(&E);
  }

  #ifndef NO_FILEZILLA
  TFileZillaIntf::Finalize();
  #endif
  PuttyFinalize();

  delete StoredSessions;
  StoredSessions = NULL;
  delete Configuration;
  Configuration = NULL;

  CryptographyFinalize();
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
void CoreMaintenanceTask()
{
  DontSaveRandomSeed();
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TOperationVisualizer::TOperationVisualizer(bool UseBusyCursor) :
  FUseBusyCursor(UseBusyCursor)
{
  if (FUseBusyCursor)
  {
    FToken = BusyStart();
  }
}
//---------------------------------------------------------------------------
__fastcall TOperationVisualizer::~TOperationVisualizer()
{
  if (FUseBusyCursor)
  {
    BusyEnd(FToken);
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TInstantOperationVisualizer::TInstantOperationVisualizer() :
  FStart(Now())
{
}
//---------------------------------------------------------------------------
__fastcall TInstantOperationVisualizer::~TInstantOperationVisualizer()
{
  TDateTime Time = Now();
  __int64 Duration = MilliSecondsBetween(Time, FStart);
  const __int64 MinDuration = 250;
  if (Duration < MinDuration)
  {
    Sleep(static_cast<unsigned int>(MinDuration - Duration));
  }
}
//---------------------------------------------------------------------------
// WORKAROUND, suppress warning about unused constants in DateUtils.hpp
#pragma warn -8080
