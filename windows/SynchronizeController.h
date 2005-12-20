//---------------------------------------------------------------------------
#ifndef SynchronizeControllerH
#define SynchronizeControllerH
//---------------------------------------------------------------------------
#include <CopyParam.h>
//---------------------------------------------------------------------------
struct TSynchronizeParamType 
{
  AnsiString LocalDirectory;
  AnsiString RemoteDirectory;
  int Params;
  int Options;
};
//---------------------------------------------------------------------------
class TSynchronizeController;
struct TSynchronizeOptions;
class TSynchronizeChecklist;
typedef void __fastcall (__closure * TSynchronizeAbortEvent)
  (System::TObject * Sender, bool Close);
typedef void __fastcall (__closure * TSynchronizeThreadsEvent)
  (TObject* Sender, TThreadMethod Method);
enum TSynchronizeLogEntry { slScan, slStart, slChange, slUpload, slDelete, slDirChange };
typedef void __fastcall (__closure * TSynchronizeLog)
  (TSynchronizeController * Controller, TSynchronizeLogEntry Entry, const AnsiString Message);
typedef void __fastcall (__closure * TSynchronizeStartStopEvent)
  (System::TObject * Sender, bool Start, const TSynchronizeParamType & Params,
   const TCopyParamType & CopyParam, TSynchronizeOptions * Options,
   TSynchronizeAbortEvent OnAbort, TSynchronizeThreadsEvent OnSynchronizeThreads,
   TSynchronizeLog OnSynchronizeLog);
typedef void __fastcall (__closure * TSynchronizeEvent)
  (TSynchronizeController * Sender, const AnsiString LocalDirectory,
   const AnsiString RemoteDirectory, const TCopyParamType & CopyParam,
   const TSynchronizeParamType & Params, TSynchronizeChecklist ** Checklist,
   TSynchronizeOptions * Options, bool Full);
typedef void __fastcall (__closure * TSynchronizeInvalidEvent)
  (TSynchronizeController * Sender, const AnsiString Directory, const AnsiString ErrorStr);
typedef void __fastcall (__closure * TSynchronizeTooManyDirectories)
  (TSynchronizeController * Sender, int & MaxDirectories);
//---------------------------------------------------------------------------
namespace Discmon
{
class TDiscMonitor;
}
//---------------------------------------------------------------------------
enum TSynchronizeOperation { soUpload, soDelete };
//---------------------------------------------------------------------------
class TSynchronizeController
{
public:
  __fastcall TSynchronizeController(TSynchronizeEvent AOnSynchronize,
    TSynchronizeInvalidEvent AOnSynchronizeInvalid,
    TSynchronizeTooManyDirectories AOnTooManyDirectories);
  __fastcall ~TSynchronizeController();

  void __fastcall StartStop(TObject * Sender, bool Start,
    const TSynchronizeParamType & Params, const TCopyParamType & CopyParam,
    TSynchronizeOptions * Options,
    TSynchronizeAbortEvent OnAbort, TSynchronizeThreadsEvent OnSynchronizeThreads,
    TSynchronizeLog OnSynchronizeLog);
  void __fastcall LogOperation(TSynchronizeOperation Operation, const AnsiString FileName);

private:
  TSynchronizeEvent FOnSynchronize;
  TSynchronizeParamType FSynchronizeParams;
  TSynchronizeOptions * FOptions;
  TSynchronizeThreadsEvent FOnSynchronizeThreads;
  Discmon::TDiscMonitor * FSynchronizeMonitor;
  TSynchronizeAbortEvent FSynchronizeAbort;
  TSynchronizeInvalidEvent FOnSynchronizeInvalid;
  TSynchronizeTooManyDirectories FOnTooManyDirectories;
  TSynchronizeLog FSynchronizeLog;
  TCopyParamType FCopyParam;

  void __fastcall SynchronizeChange(TObject * Sender, const AnsiString Directory,
    bool & SubdirsChanged);
  void __fastcall SynchronizeAbort(bool Close);
  void __fastcall SynchronizeLog(TSynchronizeLogEntry Entry, const AnsiString Message);
  void __fastcall SynchronizeInvalid(TObject * Sender, const AnsiString Directory, 
    const AnsiString ErrorStr);
  void __fastcall SynchronizeFilter(TObject * Sender, const AnsiString DirectoryName, 
    bool & Add);
  void __fastcall SynchronizeTooManyDirectories(TObject * Sender, int & MaxDirectories);
  void __fastcall SynchronizeDirectoriesChange(TObject * Sender, int Directories);
};
//---------------------------------------------------------------------------
#endif
