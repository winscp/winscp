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
struct TSynchronizeStats;
typedef void __fastcall (__closure * TSynchronizeAbortEvent)
  (System::TObject * Sender, bool Close);
typedef void __fastcall (__closure * TSynchronizeThreadsEvent)
  (TObject* Sender, TThreadMethod Method);
typedef void __fastcall (__closure * TSynchronizeStartStopEvent)
  (System::TObject * Sender, bool Start, const TSynchronizeParamType & Params,
   const TCopyParamType & CopyParam,
   TSynchronizeAbortEvent OnAbort, TSynchronizeThreadsEvent OnSynchronizeThreads);
typedef void __fastcall (__closure * TSynchronizeEvent)
  (TSynchronizeController * Sender, const AnsiString LocalDirectory,
   const AnsiString RemoteDirectory, const TCopyParamType & CopyParam,
   const TSynchronizeParamType & Params, TSynchronizeStats * Stats, bool Full);
typedef void __fastcall (__closure * TSynchronizeInvalidEvent)
  (TSynchronizeController * Sender, const AnsiString Directory, const AnsiString ErrorStr);
//---------------------------------------------------------------------------
namespace Discmon
{
class TDiscMonitor;
}
//---------------------------------------------------------------------------
class TSynchronizeController
{
public:
  __fastcall TSynchronizeController(TSynchronizeEvent AOnSynchronize,
    TSynchronizeInvalidEvent AOnSynchronizeInvalid);
  __fastcall ~TSynchronizeController();

  void __fastcall StartStop(TObject * Sender, bool Start,
    const TSynchronizeParamType & Params, const TCopyParamType & CopyParam,
    TSynchronizeAbortEvent OnAbort, TSynchronizeThreadsEvent OnSynchronizeThreads);

private:
  TSynchronizeEvent FOnSynchronize;
  TSynchronizeParamType FSynchronizeParams;
  TSynchronizeThreadsEvent FOnSynchronizeThreads;
  Discmon::TDiscMonitor * FSynchronizeMonitor;
  TSynchronizeAbortEvent FSynchronizeAbort;
  TSynchronizeInvalidEvent FOnSynchronizeInvalid;
  TCopyParamType FCopyParam;

  void __fastcall SynchronizeChange(TObject * Sender, const AnsiString Directory,
    bool & SubdirsChanged);
  void __fastcall SynchronizeAbort(bool Close);
  void __fastcall SynchronizeInvalid(TObject * Sender, const AnsiString Directory, 
    const AnsiString ErrorStr);
  void __fastcall SynchronizeFilter(TObject * Sender, const AnsiString DirectoryName, 
    bool & Add);
};
//---------------------------------------------------------------------------
#endif
