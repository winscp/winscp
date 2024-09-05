//---------------------------------------------------------------------------
#ifndef CustomWinConfigurationH
#define CustomWinConfigurationH
//---------------------------------------------------------------------------
#include "GUIConfiguration.h"
#define WM_WINSCP_USER   (WM_USER + 0x2000)
// WM_USER_STOP = WM_WINSCP_USER + 2 (in forms/Synchronize.cpp)
// WM_INTERUPT_IDLE = WM_WINSCP_USER + 3 (in windows/ConsoleRunner.cpp)
// WM_COMPONENT_HIDE = WM_WINSCP_USER + 4 (forms/CustomScpExplorer.cpp)
// WM_TRAY_ICON = WM_WINSCP_USER + 5 (forms/CustomScpExplorer.cpp)
// WM_WINSCP_USER + 6 was WM_LOG_UPDATE
#define WM_MANAGES_CAPTION (WM_WINSCP_USER + 7)
#define WM_WANTS_MOUSEWHEEL (WM_WINSCP_USER + 8)
#define WM_CAN_DISPLAY_UPDATES (WM_WINSCP_USER + 9)
// CM_DPICHANGED + 10 (packages/my/PasTools.pas)
#define WM_WANTS_MOUSEWHEEL_INACTIVE (WM_WINSCP_USER + 11)
#define WM_WANTS_SCREEN_TIPS (WM_WINSCP_USER + 12)
// WM_USER_SHCHANGENOTIFY + 13 (packages/filemng/DriveView.pas)
// WM_PASTE_FILES + 14 (forms/CustomScpExplorer.cpp)
#define WM_IS_HIDDEN (WM_WINSCP_USER + 15)
// WM_USER_INVALIDATEITEM + 16 (packages/filemng/DirView.pas)
#define WM_QUEUE_CALLBACK (WM_WINSCP_USER + 17)
//---------------------------------------------------------------------------
#define C(Property) (Property != rhc.Property) ||
struct TSynchronizeChecklistConfiguration
{
  UnicodeString WindowParams;
  UnicodeString ListParams;
  bool __fastcall operator !=(TSynchronizeChecklistConfiguration & rhc)
    { return C(WindowParams) C(ListParams) 0; };
};
typedef TSynchronizeChecklistConfiguration TFindFileConfiguration;
//---------------------------------------------------------------------------
struct TConsoleWinConfiguration
{
  UnicodeString WindowSize;
  bool __fastcall operator !=(TConsoleWinConfiguration & rhc)
    { return C(WindowSize) 0; };
};
//---------------------------------------------------------------------------
enum TIncrementalSearch { isOff = -1, isNameStartOnly, isName, isAll };
//---------------------------------------------------------------------------
struct TLoginDialogConfiguration : public TConsoleWinConfiguration
{
  TIncrementalSearch SiteSearch;
  bool __fastcall operator !=(TLoginDialogConfiguration & rhc)
    { return (TConsoleWinConfiguration::operator !=(rhc)) || C(SiteSearch) 0; };
};
//---------------------------------------------------------------------------
class TCustomWinConfiguration : public TGUIConfiguration
{
static const int MaxHistoryCount = 50;
private:
  TInterface FInterface;
  TInterface FAppliedInterface;
  TStringList * FHistory;
  TStrings * FEmptyHistory;
  TSynchronizeChecklistConfiguration FSynchronizeChecklist;
  TFindFileConfiguration FFindFile;
  TConsoleWinConfiguration FConsoleWin;
  TLoginDialogConfiguration FLoginDialog;
  TInterface FDefaultInterface;
  bool FCanApplyInterfaceImmediately;
  bool FConfirmExitOnCompletion;
  bool FSynchronizeSummary;
  UnicodeString FSessionColors;
  UnicodeString FFontColors;
  bool FCopyShortCutHintShown;
  bool FHttpForWebDAV;
  TNotifyEvent FOnMasterPasswordRecrypt;
  UnicodeString FDefaultFixedWidthFontName;
  int FDefaultFixedWidthFontSize;

  void __fastcall SetInterface(TInterface value);
  void __fastcall SetHistory(const UnicodeString Index, TStrings * value);
  TStrings * __fastcall GetHistory(const UnicodeString Index);
  void __fastcall SetSynchronizeChecklist(TSynchronizeChecklistConfiguration value);
  void __fastcall SetFindFile(TFindFileConfiguration value);
  void __fastcall SetConsoleWin(TConsoleWinConfiguration value);
  void __fastcall SetLoginDialog(TLoginDialogConfiguration value);
  void __fastcall SetConfirmExitOnCompletion(bool value);
  void __fastcall SetSynchronizeSummary(bool value);
  UnicodeString __fastcall GetDefaultFixedWidthFontName();
  int __fastcall GetDefaultFixedWidthFontSize();

protected:
  virtual void __fastcall SaveData(THierarchicalStorage * Storage, bool All);
  virtual void __fastcall LoadData(THierarchicalStorage * Storage);
  virtual void __fastcall LoadAdmin(THierarchicalStorage * Storage);
  virtual void __fastcall Saved();
  void __fastcall ClearHistory();
  virtual void __fastcall DefaultHistory();
  void __fastcall RecryptPasswords(TStrings * RecryptPasswordErrors);
  virtual bool __fastcall GetUseMasterPassword() = 0;
  UnicodeString __fastcall FormatDefaultWindowParams(int Width, int Height);
  UnicodeString __fastcall FormatDefaultWindowSize(int Width, int Height);

public:
  __fastcall TCustomWinConfiguration();
  virtual __fastcall ~TCustomWinConfiguration();
  virtual void __fastcall Default();
  virtual void __fastcall AskForMasterPasswordIfNotSet() = 0;
  void __fastcall AskForMasterPasswordIfNotSetAndNeededToPersistSessionData(TSessionData * SessionData);
  static UnicodeString __fastcall GetValidHistoryKey(UnicodeString Key);

  __property TInterface Interface = { read = FInterface, write = SetInterface };
  __property TInterface AppliedInterface = { read = FAppliedInterface, write = FAppliedInterface };
  __property bool CanApplyInterfaceImmediately = { read = FCanApplyInterfaceImmediately, write = FCanApplyInterfaceImmediately };
  __property TStrings * History[UnicodeString Name] = { read = GetHistory, write = SetHistory };
  __property TSynchronizeChecklistConfiguration SynchronizeChecklist = { read = FSynchronizeChecklist, write = SetSynchronizeChecklist };
  __property TFindFileConfiguration FindFile = { read = FFindFile, write = SetFindFile };
  __property TConsoleWinConfiguration ConsoleWin = { read = FConsoleWin, write = SetConsoleWin };
  __property TLoginDialogConfiguration LoginDialog = { read = FLoginDialog, write = SetLoginDialog };
  __property bool ConfirmExitOnCompletion  = { read=FConfirmExitOnCompletion, write=SetConfirmExitOnCompletion };
  __property bool SynchronizeSummary  = { read = FSynchronizeSummary, write = SetSynchronizeSummary };
  __property UnicodeString SessionColors  = { read=FSessionColors, write=FSessionColors };
  __property UnicodeString FontColors  = { read=FFontColors, write=FFontColors };
  __property bool CopyShortCutHintShown  = { read=FCopyShortCutHintShown, write=FCopyShortCutHintShown };
  __property bool UseMasterPassword = { read = GetUseMasterPassword };
  __property bool HttpForWebDAV = { read = FHttpForWebDAV, write = FHttpForWebDAV };
  __property TNotifyEvent OnMasterPasswordRecrypt = { read = FOnMasterPasswordRecrypt, write = FOnMasterPasswordRecrypt };
  __property UnicodeString DefaultFixedWidthFontName = { read = GetDefaultFixedWidthFontName };
  __property int DefaultFixedWidthFontSize = { read = GetDefaultFixedWidthFontSize };
};
//---------------------------------------------------------------------------
extern TCustomWinConfiguration * CustomWinConfiguration;
//---------------------------------------------------------------------------
#endif
