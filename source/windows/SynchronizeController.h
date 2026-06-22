//---------------------------------------------------------------------------
#ifndef SynchronizeControllerH
#define SynchronizeControllerH
//---------------------------------------------------------------------------
#include <CopyParam.h>
//---------------------------------------------------------------------------
struct TSynchronizeParamType
{
  UnicodeString LocalDirectory;
  UnicodeString RemoteDirectory;
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
enum TSynchronizeLogEntry { slScan, slStart, slChange, slUpload, slDelete, slDirChange, slContinuedError };
typedef void __fastcall (__closure * TSynchronizeLog)
  (TSynchronizeController * Controller, TSynchronizeLogEntry Entry, const UnicodeString Message);
typedef void __fastcall (__closure * TSynchronizeStartStopEvent)
  (System::TObject * Sender, bool Start, const TSynchronizeParamType & Params,
   const TCopyParamType & CopyParam, TSynchronizeOptions * Options,
   TSynchronizeAbortEvent OnAbort, TSynchronizeThreadsEvent OnSynchronizeThreads,
   TSynchronizeLog OnSynchronizeLog);
typedef void __fastcall (__closure * TSynchronizeEvent)
  (TSynchronizeController * Sender, const UnicodeString LocalDirectory,
   const UnicodeString RemoteDirectory, const TCopyParamType & CopyParam,
   const TSynchronizeParamType & Params, TSynchronizeChecklist ** Checklist,
   TSynchronizeOptions * Options, bool Full);
typedef void __fastcall (__closure * TSynchronizeInvalidEvent)
  (TSynchronizeController * Sender, const UnicodeString Directory, const UnicodeString ErrorStr);
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
  void __fastcall LogOperation(TSynchronizeOperation Operation, const UnicodeString FileName);

private:
  TSynchronizeEvent FOnSynchronize;
  TSynchronizeParamType FSynchronizeParams;
  TSynchronizeOptions * FOptions;
  Discmon::TDiscMonitor * FSynchronizeMonitor;
  TSynchronizeAbortEvent FSynchronizeAbort;
  TSynchronizeInvalidEvent FOnSynchronizeInvalid;
  TSynchronizeTooManyDirectories FOnTooManyDirectories;
  TSynchronizeLog FSynchronizeLog;
  TCopyParamType FCopyParam;

  void __fastcall SynchronizeChange(TObject * Sender, const UnicodeString Directory,
    bool & SubdirsChanged);
  void __fastcall SynchronizeAbort(bool Close);
  void __fastcall SynchronizeLog(TSynchronizeLogEntry Entry, const UnicodeString Message);
  void __fastcall SynchronizeInvalid(TObject * Sender, const UnicodeString Directory,
    const UnicodeString ErrorStr);
  void __fastcall SynchronizeFilter(TObject * Sender, const UnicodeString DirectoryName,
    bool & Add);
  void __fastcall SynchronizeTooManyDirectories(TObject * Sender, int & MaxDirectories);
  void __fastcall SynchronizeDirectoriesChange(TObject * Sender, int Directories);
};
//---------------------------------------------------------------------------
void __fastcall LogSynchronizeEvent(TTerminal * Terminal, const UnicodeString & Message);
//---------------------------------------------------------------------------
#endif
