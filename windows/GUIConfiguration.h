//---------------------------------------------------------------------------
#ifndef GUIConfigurationH
#define GUIConfigurationH
//---------------------------------------------------------------------------
#include "Configuration.h"
//---------------------------------------------------------------------------
struct TPasLibModule;
enum TLogView { lvNone, lvWindow, pvPanel };
enum TInterface { ifCommander, ifExplorer };
//---------------------------------------------------------------------------
class TGUIConfiguration : public TConfiguration
{
private:
  bool FCopyParamDialogExpanded;
  bool FErrorDialogExpanded;
  TStrings * FLocales;
  AnsiString FLastLocalesExts;
  bool FContinueOnError;
  AnsiString FPuttyPath;
  AnsiString FPuttySession;
  int FSynchronizeParams;
  TDateTime FIgnoreCancelBeforeFinish;
  bool FQueueAutoPopup;
  int FQueueTransfersLimit;

protected:
  LCID FLocale;

  virtual void __fastcall SaveSpecial(THierarchicalStorage * Storage);
  virtual void __fastcall LoadSpecial(THierarchicalStorage * Storage);
  virtual LCID __fastcall GetLocale();
  void __fastcall SetLocale(LCID value);
  void __fastcall SetLocaleSafe(LCID value);
  virtual HANDLE __fastcall LoadNewResourceModule(LCID Locale,
    AnsiString * FileName = NULL);
  virtual void __fastcall SetResourceModule(HANDLE Instance);
  TStrings * __fastcall GetLocales();
  LCID __fastcall InternalLocale();
  TPasLibModule * __fastcall FindModule(void * Instance);
  void __fastcall FreeResourceModule(HANDLE Instance);

public:
  __fastcall TGUIConfiguration();
  virtual __fastcall ~TGUIConfiguration();
  virtual void __fastcall Default();

  __property bool CopyParamDialogExpanded = { read = FCopyParamDialogExpanded, write = FCopyParamDialogExpanded };
  __property bool ErrorDialogExpanded = { read = FErrorDialogExpanded, write = FErrorDialogExpanded };
  __property bool ContinueOnError = { read = FContinueOnError, write = FContinueOnError };
  __property int SynchronizeParams = { read = FSynchronizeParams, write = FSynchronizeParams };
  __property int QueueTransfersLimit = { read = FQueueTransfersLimit, write = FQueueTransfersLimit };
  __property bool QueueAutoPopup = { read = FQueueAutoPopup, write = FQueueAutoPopup };
  __property LCID Locale = { read = GetLocale, write = SetLocale };
  __property LCID LocaleSafe = { read = GetLocale, write = SetLocaleSafe };
  __property TStrings * Locales = { read = GetLocales };
  __property AnsiString PuttyPath = { read = FPuttyPath, write = FPuttyPath };
  __property AnsiString PuttySession = { read = FPuttySession, write = FPuttySession };
  __property TDateTime IgnoreCancelBeforeFinish = { read = FIgnoreCancelBeforeFinish, write = FIgnoreCancelBeforeFinish };
};
//---------------------------------------------------------------------------
#define GUIConfiguration (dynamic_cast<TGUIConfiguration *>(Configuration))
//---------------------------------------------------------------------------
#endif
