//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <RemoteFiles.h>
#include <DiscMon.hpp>
#include <Exceptions.h>
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
  FChanged = false;
}
//---------------------------------------------------------------------------
__fastcall TSynchronizeController::~TSynchronizeController()
{
  assert(FSynchronizeMonitor == NULL);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeController::StartStop(TObject * Sender,
  bool Start, const TSynchronizeParamType & Params, TSynchronizeAbortEvent OnAbort,
  TSynchronizeThreadsEvent OnSynchronizeThreads)
{
  if (Start)
  {
    try
    {
      FSynchronizeParams = Params;
      assert(OnAbort);
      FSynchronizeAbort = OnAbort;
      FSynchronizeMonitor = new TDiscMonitor(dynamic_cast<TComponent*>(Sender));
      FSynchronizeMonitor->SubTree = false;
      TMonitorFilters Filters;
      Filters << moFilename << moLastWrite;
      if (FSynchronizeParams.Recurse)
      {
        Filters << moDirName;
      }
      FSynchronizeMonitor->Filters = Filters;
      FSynchronizeMonitor->AddDirectory(FSynchronizeParams.LocalDirectory,
        FSynchronizeParams.Recurse);
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
  TObject * /*Sender*/, const AnsiString Directory)
{
  try
  {
    FChanged = true;

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
      FOnSynchronize(this, LocalDirectory, RemoteDirectory,
        FSynchronizeParams);
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
  FSynchronizeMonitor->Close();
  assert(FSynchronizeAbort);
  FSynchronizeAbort(NULL, Close);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeController::SynchronizeInvalid(
  TObject * /*Sender*/, const AnsiString Directory)
{
  if (FOnSynchronizeInvalid != NULL)
  {
    FOnSynchronizeInvalid(this, Directory);
  }

  SynchronizeAbort(false);
}

