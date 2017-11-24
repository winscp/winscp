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
#include "FileZillaIntf.h"
#include "NeonIntf.h"
#include "TextsCore.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TConfiguration * Configuration = NULL;
TStoredSessionList * StoredSessions = NULL;
//---------------------------------------------------------------------------
TQueryButtonAlias::TQueryButtonAlias()
{
  OnSubmit = NULL;
  GroupWith = -1;
  ElevationRequired = false;
  MenuButton = false;
}
//---------------------------------------------------------------------------
TQueryButtonAlias TQueryButtonAlias::CreateYesToAllGrouppedWithYes()
{
  TQueryButtonAlias Result;
  Result.Button = qaYesToAll;
  Result.GroupWith = qaYes;
  Result.GrouppedShiftState = TShiftState() << ssShift;
  return Result;
}
//---------------------------------------------------------------------------
TQueryButtonAlias TQueryButtonAlias::CreateNoToAllGrouppedWithNo()
{
  TQueryButtonAlias Result;
  Result.Button = qaNoToAll;
  Result.GroupWith = qaNo;
  Result.GrouppedShiftState = TShiftState() << ssShift;
  return Result;
}
//---------------------------------------------------------------------------
TQueryButtonAlias TQueryButtonAlias::CreateAllAsYesToNewerGrouppedWithYes()
{
  TQueryButtonAlias Result;
  Result.Button = qaAll;
  Result.Alias = LoadStr(YES_TO_NEWER_BUTTON);
  Result.GroupWith = qaYes;
  Result.GrouppedShiftState = TShiftState() << ssCtrl;
  return Result;
}
//---------------------------------------------------------------------------
TQueryButtonAlias TQueryButtonAlias::CreateIgnoreAsRenameGrouppedWithNo()
{
  TQueryButtonAlias Result;
  Result.Button = qaIgnore;
  Result.Alias = LoadStr(RENAME_BUTTON);
  Result.GroupWith = qaNo;
  Result.GrouppedShiftState = TShiftState() << ssCtrl;
  return Result;
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
bool __fastcall IsPasswordOrPassphrasePrompt(TPromptKind Kind, TStrings * Prompts)
{
  return
    (Prompts->Count == 1) && FLAGCLEAR(int(Prompts->Objects[0]), pupEcho) &&
    ((Kind == pkPassword) || (Kind == pkPassphrase) || (Kind == pkKeybInteractive) ||
     (Kind == pkTIS) || (Kind == pkCryptoCard));
}
//---------------------------------------------------------------------------
bool __fastcall IsPasswordPrompt(TPromptKind Kind, TStrings * Prompts)
{
  return
    IsPasswordOrPassphrasePrompt(Kind, Prompts) &&
    (Kind != pkPassphrase);
}
//---------------------------------------------------------------------------
void CoreLoad()
{
  bool SessionList = true;
  std::unique_ptr<THierarchicalStorage> SessionsStorage(Configuration->CreateScpStorage(SessionList));
  THierarchicalStorage * ConfigStorage;
  std::unique_ptr<THierarchicalStorage> ConfigStorageAuto;
  if (!SessionList)
  {
    // can reuse this for configuration
    ConfigStorage = SessionsStorage.get();
  }
  else
  {
    ConfigStorageAuto.reset(Configuration->CreateConfigStorage());
    ConfigStorage = ConfigStorageAuto.get();
  }

  try
  {
    Configuration->Load(ConfigStorage);
  }
  catch (Exception & E)
  {
    ShowExtendedException(&E);
  }

  // should be noop, unless exception occured above
  ConfigStorage->CloseAll();

  StoredSessions = new TStoredSessionList();

  try
  {
    if (SessionsStorage->OpenSubKey(Configuration->StoredSessionsSubKey, false))
    {
      StoredSessions->Load(SessionsStorage.get());
    }
  }
  catch (Exception & E)
  {
    ShowExtendedException(&E);
  }
}
//---------------------------------------------------------------------------
void CoreInitialize()
{
  Randomize();
  CryptographyInitialize();

  // we do not expect configuration re-creation
  DebugAssert(Configuration == NULL);
  // configuration needs to be created and loaded before putty is initialized,
  // so that random seed path is known
  Configuration = CreateConfiguration();

  PuttyInitialize();
  TFileZillaIntf::Initialize();
  NeonInitialize();

  CoreLoad();
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

  NeonFinalize();
  TFileZillaIntf::Finalize();
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
  TFileZillaIntf::SetResourceModule(ResourceHandle);
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
