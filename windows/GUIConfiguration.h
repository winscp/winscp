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

protected:
  LCID FLocale;

  virtual void __fastcall SaveSpecial(THierarchicalStorage * Storage);
  virtual void __fastcall LoadSpecial(THierarchicalStorage * Storage);
  virtual LCID __fastcall GetLocale();
  void __fastcall SetLocale(LCID value);
  void __fastcall SetLocaleSafe(LCID value);
  HANDLE LoadNewResourceModule(LCID Locale);
  virtual void __fastcall ReinitLocale();
  TStrings * __fastcall GetLocales();
  LCID __fastcall InternalLocale();
  TPasLibModule * FindModule(void * Instance);

public:
  __fastcall TGUIConfiguration();
  __fastcall ~TGUIConfiguration();
  virtual void __fastcall Default();

  __property bool CopyParamDialogExpanded = { read = FCopyParamDialogExpanded, write = FCopyParamDialogExpanded };
  __property bool ErrorDialogExpanded = { read = FErrorDialogExpanded, write = FErrorDialogExpanded };
  __property bool ContinueOnError = { read = FContinueOnError, write = FContinueOnError };
  __property int SynchronizeParams = { read = FSynchronizeParams, write = FSynchronizeParams };
  __property LCID Locale = { read = GetLocale, write = SetLocale };
  __property LCID LocaleSafe = { read = GetLocale, write = SetLocaleSafe };
  __property TStrings * Locales = { read = GetLocales };
  __property AnsiString PuttyPath = { read = FPuttyPath, write = FPuttyPath };
  __property AnsiString PuttySession = { read = FPuttySession, write = FPuttySession };
};
//---------------------------------------------------------------------------
#define GUIConfiguration (dynamic_cast<TGUIConfiguration *>(Configuration))
//---------------------------------------------------------------------------
#endif
