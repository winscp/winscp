//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <RemoteFiles.h>
#include <Terminal.h>
#include <DiscMon.hpp>
#include <Exceptions.h>
#include "GUIConfiguration.h"
#include "SynchronizeController.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall TSynchronizeController::TSynchronizeController(
  TSynchronizeEvent AOnSynchronize, TSynchronizeInvalidEvent AOnSynchronizeInvalid)
{
  FOnSynchronize = AOnSynchronize;
  FOnSynchronizeInvalid = AOnSynchronizeInvalid;
  FSynchronizeMonitor = NULL;
  FSynchronizeAbort = NULL;
}
//---------------------------------------------------------------------------
__fastcall TSynchronizeController::~TSynchronizeController()
{
  assert(FSynchronizeMonitor == NULL);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeController::StartStop(TObject * Sender,
  bool Start, const TSynchronizeParamType & Params, const TCopyParamType & CopyParam,
  TSynchronizeAbortEvent OnAbort, TSynchronizeThreadsEvent OnSynchronizeThreads)
{
  if (Start)
  {
    try
    {
      if (FLAGSET(Params.Options, soSynchronize) &&
          (FOnSynchronize != NULL))
      {
        try
        {
          FOnSynchronize(this, Params.LocalDirectory,
            Params.RemoteDirectory, CopyParam,
            Params, NULL, true);
        }
        catch(Exception & E)
        {
          SynchronizeAbort(dynamic_cast<EFatal*>(&E) != NULL);
          throw;
        }
      }

      FCopyParam = CopyParam;
      FSynchronizeParams = Params;

      assert(OnAbort);
      FSynchronizeAbort = OnAbort;
      FSynchronizeMonitor = new TDiscMonitor(dynamic_cast<TComponent*>(Sender));
      FSynchronizeMonitor->SubTree = false;
      TMonitorFilters Filters;
      Filters << moFilename << moLastWrite;
      if (FLAGSET(FSynchronizeParams.Options, soRecurse))
      {
        Filters << moDirName;
      }
      FSynchronizeMonitor->Filters = Filters;
      FSynchronizeMonitor->OnFilter = SynchronizeFilter;
      FSynchronizeMonitor->AddDirectory(FSynchronizeParams.LocalDirectory,
        FLAGSET(FSynchronizeParams.Options, soRecurse));
      FSynchronizeMonitor->OnChange = SynchronizeChange;
      FSynchronizeMonitor->OnInvalid = SynchronizeInvalid;
      FSynchronizeMonitor->OnSynchronize = OnSynchronizeThreads;
      FSynchronizeMonitor->Open();
    }
    catch(...)
    {
      SAFE_DESTROY(FSynchronizeMonitor);
      throw;
    }
  }
  else
  {
    SAFE_DESTROY(FSynchronizeMonitor);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeController::SynchronizeChange(
  TObject * /*Sender*/, const AnsiString Directory, bool & SubdirsChanged)
{
  try
  {
    AnsiString RemoteDirectory;
    AnsiString RootLocalDirectory;
    RootLocalDirectory = IncludeTrailingBackslash(FSynchronizeParams.LocalDirectory);
    RemoteDirectory = UnixIncludeTrailingBackslash(FSynchronizeParams.RemoteDirectory);

    AnsiString LocalDirectory = IncludeTrailingBackslash(Directory);

    assert(LocalDirectory.SubString(1, RootLocalDirectory.Length()) ==
      RootLocalDirectory);
    RemoteDirectory = RemoteDirectory +
      ToUnixPath(LocalDirectory.SubString(RootLocalDirectory.Length() + 1,
        LocalDirectory.Length() - RootLocalDirectory.Length()));

    if (FOnSynchronize != NULL)
    {
      TSynchronizeStats Stats;
      FOnSynchronize(this, LocalDirectory, RemoteDirectory, FCopyParam,
        FSynchronizeParams, &Stats, false);
      // note that ObsoleteDirectories may be non-zero even if nothing has changed
      // so this is sub-optimal
      SubdirsChanged =
        (FLAGSET(FSynchronizeParams.Options, soRecurse) &&
         ((Stats.NewDirectories + Stats.RemovedDirectories + Stats.ObsoleteDirectories) > 0));
    }
  }
  catch(Exception & E)
  {
    SynchronizeAbort(dynamic_cast<EFatal*>(&E) != NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeController::SynchronizeAbort(bool Close)
{
  if (FSynchronizeMonitor != NULL)
  {
    FSynchronizeMonitor->Close();
  }
  assert(FSynchronizeAbort);
  FSynchronizeAbort(NULL, Close);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeController::SynchronizeFilter(TObject * /*Sender*/,
  const AnsiString DirectoryName, bool & Add)
{
  Add = FCopyParam.AllowTransfer(DirectoryName, osLocal);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeController::SynchronizeInvalid(
  TObject * /*Sender*/, const AnsiString Directory, const AnsiString ErrorStr)
{
  if (FOnSynchronizeInvalid != NULL)
  {
    FOnSynchronizeInvalid(this, Directory, ErrorStr);
  }

  SynchronizeAbort(false);
}

