//---------------------------------------------------------------------------
#ifndef GUIConfigurationH
#define GUIConfigurationH
//---------------------------------------------------------------------------
#include "Configuration.h"
#include "CopyParam.h"
//---------------------------------------------------------------------------
struct TPasLibModule;
enum TLogView { lvNone, lvWindow, pvPanel };
enum TInterface { ifCommander, ifExplorer };
//---------------------------------------------------------------------------
extern const ccLocal;
extern const ccShowResults;
//---------------------------------------------------------------------------
class TGUICopyParamType : public TCopyParamType
{
public:
  __fastcall TGUICopyParamType();
  __fastcall TGUICopyParamType(const TCopyParamType & Source);
  __fastcall TGUICopyParamType(const TGUICopyParamType & Source);

  virtual void __fastcall Default();
  virtual void __fastcall Assign(const TCopyParamType * Source);
  TGUICopyParamType & __fastcall operator =(const TGUICopyParamType & rhp);
  TGUICopyParamType & __fastcall operator =(const TCopyParamType & rhp);

  __property bool Queue = { read = FQueue, write = FQueue };
  __property bool QueueNoConfirmation = { read = FQueueNoConfirmation, write = FQueueNoConfirmation };
  __property bool NewerOnly = { read = FNewerOnly, write = FNewerOnly };

protected:
  void __fastcall GUIDefault();
  void __fastcall GUIAssign(const TGUICopyParamType * Source);

private:
  bool FQueue;
  bool FQueueNoConfirmation;
  bool FNewerOnly;
};
//---------------------------------------------------------------------------
class TGUIConfiguration : public TConfiguration
{
private:
  bool FCopyParamDialogExpanded;
  bool FErrorDialogExpanded;
  TStrings * FLocales;
  AnsiString FLastLocalesExts;
  bool FContinueOnError;
  bool FConfirmCommandSession;
  AnsiString FPuttyPath;
  bool FPuttyPassword;
  AnsiString FPuttySession;
  int FSynchronizeParams;
  bool FSynchronizeRecurse;
  TDateTime FIgnoreCancelBeforeFinish;
  bool FQueueAutoPopup;
  bool FQueueRememberPassword;
  int FQueueTransfersLimit;
  TGUICopyParamType FCopyParam;
  bool FBeepOnFinish;
  TDateTime FBeepOnFinishAfter;
  AnsiString FDefaultPuttyPath;

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
  void __fastcall FreeResourceModule(HANDLE Instance);
  void __fastcall SetCopyParam(TGUICopyParamType value);
  virtual bool __fastcall GetRememberPassword();
  static AnsiString __fastcall PropertyToKey(const AnsiString Property);

public:
  __fastcall TGUIConfiguration();
  virtual __fastcall ~TGUIConfiguration();
  virtual void __fastcall Default();

  HANDLE __fastcall ChangeResourceModule(HANDLE Instance);

  __property bool CopyParamDialogExpanded = { read = FCopyParamDialogExpanded, write = FCopyParamDialogExpanded };
  __property bool ErrorDialogExpanded = { read = FErrorDialogExpanded, write = FErrorDialogExpanded };
  __property bool ContinueOnError = { read = FContinueOnError, write = FContinueOnError };
  __property bool ConfirmCommandSession = { read = FConfirmCommandSession, write = FConfirmCommandSession };
  __property int SynchronizeParams = { read = FSynchronizeParams, write = FSynchronizeParams };
  __property bool SynchronizeRecurse = { read = FSynchronizeRecurse, write = FSynchronizeRecurse };
  __property int QueueTransfersLimit = { read = FQueueTransfersLimit, write = FQueueTransfersLimit };
  __property bool QueueAutoPopup = { read = FQueueAutoPopup, write = FQueueAutoPopup };
  __property bool QueueRememberPassword = { read = FQueueRememberPassword, write = FQueueRememberPassword };
  __property LCID Locale = { read = GetLocale, write = SetLocale };
  __property LCID LocaleSafe = { read = GetLocale, write = SetLocaleSafe };
  __property TStrings * Locales = { read = GetLocales };
  __property AnsiString PuttyPath = { read = FPuttyPath, write = FPuttyPath };
  __property bool PuttyPassword = { read = FPuttyPassword, write = FPuttyPassword };
  __property AnsiString PuttySession = { read = FPuttySession, write = FPuttySession };
  __property TDateTime IgnoreCancelBeforeFinish = { read = FIgnoreCancelBeforeFinish, write = FIgnoreCancelBeforeFinish };
  __property TGUICopyParamType CopyParam = { read = FCopyParam, write = SetCopyParam };
  __property bool BeepOnFinish = { read = FBeepOnFinish, write = FBeepOnFinish };
  __property TDateTime BeepOnFinishAfter = { read = FBeepOnFinishAfter, write = FBeepOnFinishAfter };
};
//---------------------------------------------------------------------------
#define GUIConfiguration (dynamic_cast<TGUIConfiguration *>(Configuration))
//---------------------------------------------------------------------------
#endif
