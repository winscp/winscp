//---------------------------------------------------------------------------
#ifndef SynchronizeControllerH
#define SynchronizeControllerH
//---------------------------------------------------------------------------
struct TSynchronizeParamType 
{
  AnsiString LocalDirectory;
  AnsiString RemoteDirectory;
  int Params;
  bool Recurse;
};
//---------------------------------------------------------------------------
class TSynchronizeController;
typedef void __fastcall (__closure * TSynchronizeAbortEvent)
  (System::TObject * Sender, bool Close);
typedef void __fastcall (__closure * TSynchronizeThreadsEvent)
  (TObject* Sender, TThreadMethod Method);
typedef void __fastcall (__closure * TSynchronizeStartStopEvent)
  (System::TObject * Sender, bool Start, const TSynchronizeParamType & Params,
   TSynchronizeAbortEvent OnAbort, TSynchronizeThreadsEvent OnSynchronizeThreads);
typedef void __fastcall (__closure * TSynchronizeEvent)
  (TSynchronizeController * Sender, const AnsiString LocalDirectory,
   const AnsiString RemoteDirectory, const TSynchronizeParamType & Params);
typedef void __fastcall (__closure * TSynchronizeInvalidEvent)
  (TSynchronizeController * Sender, const AnsiString Directory);
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
    const TSynchronizeParamType & Params, TSynchronizeAbortEvent OnAbort,
    TSynchronizeThreadsEvent OnSynchronizeThreads);

  __property bool Changed = { read = FChanged }; 

private:
  TSynchronizeEvent FOnSynchronize;
  TSynchronizeParamType FSynchronizeParams;
  TSynchronizeThreadsEvent FOnSynchronizeThreads;
  Discmon::TDiscMonitor * FSynchronizeMonitor;
  TSynchronizeAbortEvent FSynchronizeAbort;
  TSynchronizeInvalidEvent FOnSynchronizeInvalid;
  bool FChanged;

  void __fastcall SynchronizeChange(TObject * Sender, const AnsiString Directory);
  void __fastcall SynchronizeAbort(bool Close);
  void __fastcall SynchronizeInvalid(TObject * Sender, const AnsiString Directory);
};
//---------------------------------------------------------------------------
#endif
