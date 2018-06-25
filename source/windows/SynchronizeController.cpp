//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <RemoteFiles.h>
#include <Terminal.h>
#include <DiscMon.hpp>
#include <Exceptions.h>
#include "GUIConfiguration.h"
#include "CoreMain.h"
#include "TextsCore.h"
#include "SynchronizeController.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall TSynchronizeController::TSynchronizeController(
  TSynchronizeEvent AOnSynchronize, TSynchronizeInvalidEvent AOnSynchronizeInvalid,
  TSynchronizeTooManyDirectories AOnTooManyDirectories)
{
  FOnSynchronize = AOnSynchronize;
  FOnSynchronizeInvalid = AOnSynchronizeInvalid;
  FOnTooManyDirectories = AOnTooManyDirectories;
  FSynchronizeMonitor = NULL;
  FSynchronizeAbort = NULL;
  FSynchronizeLog = NULL;
  FOptions = NULL;
}
//---------------------------------------------------------------------------
__fastcall TSynchronizeController::~TSynchronizeController()
{
  DebugAssert(FSynchronizeMonitor == NULL);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeController::StartStop(TObject * Sender,
  bool Start, const TSynchronizeParamType & Params, const TCopyParamType & CopyParam,
  TSynchronizeOptions * Options,
  TSynchronizeAbortEvent OnAbort, TSynchronizeThreadsEvent OnSynchronizeThreads,
  TSynchronizeLog OnSynchronizeLog)
{
  if (Start)
  {
    Configuration->Usage->Inc(L"KeepUpToDates");

    try
    {
      DebugAssert(OnSynchronizeLog != NULL);
      FSynchronizeLog = OnSynchronizeLog;

      FOptions = Options;
      if (FLAGSET(Params.Options, soSynchronize) &&
          (FOnSynchronize != NULL))
      {
        FOnSynchronize(this, Params.LocalDirectory,
          Params.RemoteDirectory, CopyParam,
          Params, NULL, FOptions, true);
      }

      FCopyParam = CopyParam;
      FSynchronizeParams = Params;

      DebugAssert(OnAbort);
      FSynchronizeAbort = OnAbort;

      if (FLAGSET(FSynchronizeParams.Options, soRecurse))
      {
        SynchronizeLog(slScan,
          FMTLOAD(SYNCHRONIZE_SCAN, (FSynchronizeParams.LocalDirectory)));
      }

      FSynchronizeMonitor = new TDiscMonitor(dynamic_cast<TComponent*>(Sender));
      FSynchronizeMonitor->SubTree = false;
      TMonitorFilters Filters;
      Filters << moFilename << moLastWrite;
      if (FLAGSET(FSynchronizeParams.Options, soRecurse))
      {
        Filters << moDirName;
      }
      FSynchronizeMonitor->Filters = Filters;
      FSynchronizeMonitor->MaxDirectories = 0;
      FSynchronizeMonitor->ChangeDelay = GUIConfiguration->KeepUpToDateChangeDelay;
      FSynchronizeMonitor->OnTooManyDirectories = SynchronizeTooManyDirectories;
      FSynchronizeMonitor->OnDirectoriesChange = SynchronizeDirectoriesChange;
      FSynchronizeMonitor->OnFilter = SynchronizeFilter;
      FSynchronizeMonitor->AddDirectory(FSynchronizeParams.LocalDirectory,
        FLAGSET(FSynchronizeParams.Options, soRecurse));
      FSynchronizeMonitor->OnChange = SynchronizeChange;
      FSynchronizeMonitor->OnInvalid = SynchronizeInvalid;
      FSynchronizeMonitor->OnSynchronize = OnSynchronizeThreads;
      // get count before open to avoid thread issues
      int Directories = FSynchronizeMonitor->Directories->Count;
      FSynchronizeMonitor->Open();

      SynchronizeLog(slStart, FMTLOAD(SYNCHRONIZE_START, (Directories)));
    }
    catch(...)
    {
      SAFE_DESTROY(FSynchronizeMonitor);
      throw;
    }
  }
  else
  {
    FOptions = NULL;
    SAFE_DESTROY(FSynchronizeMonitor);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeController::SynchronizeChange(
  TObject * /*Sender*/, const UnicodeString Directory, bool & SubdirsChanged)
{
  try
  {
    UnicodeString RemoteDirectory;
    UnicodeString RootLocalDirectory;
    RootLocalDirectory = IncludeTrailingBackslash(FSynchronizeParams.LocalDirectory);
    RemoteDirectory = UnixIncludeTrailingBackslash(FSynchronizeParams.RemoteDirectory);

    UnicodeString LocalDirectory = IncludeTrailingBackslash(Directory);

    DebugAssert(LocalDirectory.SubString(1, RootLocalDirectory.Length()) ==
      RootLocalDirectory);
    RemoteDirectory = RemoteDirectory +
      ToUnixPath(LocalDirectory.SubString(RootLocalDirectory.Length() + 1,
        LocalDirectory.Length() - RootLocalDirectory.Length()));

    SynchronizeLog(slChange, FMTLOAD(SYNCHRONIZE_CHANGE,
      (ExcludeTrailingBackslash(LocalDirectory))));

    if (FOnSynchronize != NULL)
    {
      // this is completelly wrong as the options structure
      // can contain non-root specific options in future
      TSynchronizeOptions * Options =
        ((LocalDirectory == RootLocalDirectory) ? FOptions : NULL);
      TSynchronizeChecklist * Checklist = NULL;
      FOnSynchronize(this, LocalDirectory, RemoteDirectory, FCopyParam,
        FSynchronizeParams, &Checklist, Options, false);
      if (Checklist != NULL)
      {
        try
        {
          if (FLAGSET(FSynchronizeParams.Options, soRecurse))
          {
            SubdirsChanged = false;
            DebugAssert(Checklist != NULL);
            for (int Index = 0; Index < Checklist->Count; Index++)
            {
              const TSynchronizeChecklist::TItem * Item = Checklist->Item[Index];
              // note that there may be action saDeleteRemote even if nothing has changed
              // so this is sub-optimal
              if (Item->IsDirectory)
              {
                if ((Item->Action == TSynchronizeChecklist::saUploadNew) ||
                    (Item->Action == TSynchronizeChecklist::saDeleteRemote))
                {
                  SubdirsChanged = true;
                  break;
                }
                else
                {
                  DebugFail();
                }
              }
            }
          }
          else
          {
            SubdirsChanged = false;
          }
        }
        __finally
        {
          delete Checklist;
        }
      }
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
  DebugAssert(FSynchronizeAbort);
  FSynchronizeAbort(NULL, Close);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeController::LogOperation(TSynchronizeOperation Operation,
  const UnicodeString FileName)
{
  TSynchronizeLogEntry Entry;
  UnicodeString Message;
  switch (Operation)
  {
    case soDelete:
      Entry = slDelete;
      Message = FMTLOAD(SYNCHRONIZE_DELETED, (FileName));
      break;

    default:
      DebugFail();
      // fallthru

    case soUpload:
      Entry = slUpload;
      Message = FMTLOAD(SYNCHRONIZE_UPLOADED, (FileName));
      break;
  }
  SynchronizeLog(Entry, Message);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeController::SynchronizeLog(TSynchronizeLogEntry Entry,
  const UnicodeString Message)
{
  if (FSynchronizeLog != NULL)
  {
    FSynchronizeLog(this, Entry, Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeController::SynchronizeFilter(TObject * /*Sender*/,
  const UnicodeString DirectoryName, bool & Add)
{
  if ((FOptions != NULL) && (FOptions->Filter != NULL))
  {
    if (IncludeTrailingBackslash(ExtractFilePath(DirectoryName)) ==
          IncludeTrailingBackslash(FSynchronizeParams.LocalDirectory))
    {
      int FoundIndex;
      Add = FOptions->Filter->Find(ExtractFileName(DirectoryName), FoundIndex);
    }
  }

  if (Add && !FCopyParam.AllowAnyTransfer()) // optimization
  {
    TFileMasks::TParams MaskParams; // size/time does not matter for directories
    bool Hidden = FLAGSET(FileGetAttrFix(DirectoryName), faHidden);
    // Missing call to GetBaseFileName
    Add = FCopyParam.AllowTransfer(DirectoryName, osLocal, true, MaskParams, Hidden);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeController::SynchronizeInvalid(
  TObject * /*Sender*/, const UnicodeString Directory, const UnicodeString ErrorStr)
{
  if (FOnSynchronizeInvalid != NULL)
  {
    FOnSynchronizeInvalid(this, Directory, ErrorStr);
  }

  SynchronizeAbort(false);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeController::SynchronizeTooManyDirectories(
  TObject * /*Sender*/, int & MaxDirectories)
{
  if (FOnTooManyDirectories != NULL)
  {
    FOnTooManyDirectories(this, MaxDirectories);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeController::SynchronizeDirectoriesChange(
  TObject * /*Sender*/, int Directories)
{
  SynchronizeLog(slDirChange, FMTLOAD(SYNCHRONIZE_START, (Directories)));
}
//---------------------------------------------------------------------------
void __fastcall LogSynchronizeEvent(TTerminal * Terminal, const UnicodeString & Message)
{
  if (Terminal != NULL)
  {
    Terminal->LogEvent(FORMAT("Keep up to date: %s", (Message)));
  }
}
