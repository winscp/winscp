//---------------------------------------------------------------------------
#ifndef CustomWinConfigurationH
#define CustomWinConfigurationH
//---------------------------------------------------------------------------
#include "GUIConfiguration.h"
#define WM_WINSCP_USER   (WM_USER + 0x2000)
#define WM_LOCALE_CHANGE (WM_WINSCP_USER + 1)
// WM_USER_STOP = WM_WINSCP_USER + 2 (in forms/Synchronize.cpp)
// WM_INTERUPT_IDLE = WM_WINSCP_USER + 3 (in windows/ConsoleRunner.cpp)
// WM_COMPONENT_HIDE = WM_WINSCP_USER + 4 (forms/CustomScpExplorer.cpp)
//---------------------------------------------------------------------------
#define C(Property) (Property != rhc.Property) ||
struct TSynchronizeChecklistConfiguration
{
  AnsiString WindowParams;
  AnsiString ListLayout;
  bool __fastcall operator !=(TSynchronizeChecklistConfiguration & rhc)
    { return C(WindowParams) C(ListLayout) 0; };
};
//---------------------------------------------------------------------------
struct TConsoleWinConfiguration
{
  AnsiString WindowSize;
  bool __fastcall operator !=(TConsoleWinConfiguration & rhc)
    { return C(WindowSize) 0; };
};
//---------------------------------------------------------------------------
class TCustomWinConfiguration : public TGUIConfiguration
{
static const int MaxHistoryCount = 50;
private:
  TLogView FLogView;
  TInterface FInterface;
  bool FShowAdvancedLoginOptions;
  TStringList * FHistory;
  TStrings * FEmptyHistory;
  TSynchronizeChecklistConfiguration FSynchronizeChecklist;
  TConsoleWinConfiguration FConsoleWin;

  void __fastcall SetInterface(TInterface value);
  void __fastcall SetLogView(TLogView value);
  void __fastcall SetShowAdvancedLoginOptions(bool value);
  void __fastcall SetHistory(const AnsiString Index, TStrings * value);
  TStrings * __fastcall GetHistory(const AnsiString Index);
  void __fastcall SetSynchronizeChecklist(TSynchronizeChecklistConfiguration value);
  void __fastcall SetConsoleWin(TConsoleWinConfiguration value);

protected:
  virtual void __fastcall SaveSpecial(THierarchicalStorage * Storage);
  virtual void __fastcall LoadSpecial(THierarchicalStorage * Storage);
  virtual void __fastcall ModifyAll();
  void __fastcall ClearHistory();

public:
  __fastcall TCustomWinConfiguration();
  virtual __fastcall ~TCustomWinConfiguration();
  virtual void __fastcall Default();

  __property TLogView LogView = { read = FLogView, write = SetLogView };
  __property TInterface Interface = { read = FInterface, write = SetInterface };
  __property bool ShowAdvancedLoginOptions = { read = FShowAdvancedLoginOptions, write = SetShowAdvancedLoginOptions};
  __property TStrings * History[AnsiString Name] = { read = GetHistory, write = SetHistory };
  __property TSynchronizeChecklistConfiguration SynchronizeChecklist = { read = FSynchronizeChecklist, write = SetSynchronizeChecklist };
  __property TConsoleWinConfiguration ConsoleWin = { read = FConsoleWin, write = SetConsoleWin };
};
//---------------------------------------------------------------------------
#define CustomWinConfiguration \
  (dynamic_cast<TCustomWinConfiguration *>(Configuration))
//---------------------------------------------------------------------------
#endif
