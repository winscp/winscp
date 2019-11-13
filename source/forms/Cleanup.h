//----------------------------------------------------------------------------
#ifndef CleanupH
#define CleanupH
//----------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
//----------------------------------------------------------------------------
#include <Configuration.h>
#include <SessionData.h>
#include <WinInterface.h>
#include <GUITools.h>
//---------------------------------------------------------------------
enum TWinSCPData {wdConfiguration = 1, wdStoredSessions, wdCaches,
  wdConfigurationIniFile, wdRandomSeedFile, wdTemporaryFolders };
//---------------------------------------------------------------------
class TCleanupDialog : public TForm
{
__published:
  TButton *OKButton;
  TButton *CancelButton;
  TListView *DataListView;
  TLabel *Label1;
  TButton *CheckAllButton;
  TButton *HelpButton;
  void __fastcall DataListViewMouseDown(TObject *Sender,
    TMouseButton Button, TShiftState Shift, int X, int Y);
  void __fastcall DataListViewKeyUp(TObject *Sender, WORD &Key,
    TShiftState Shift);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall CheckAllButtonClick(TObject *Sender);
  void __fastcall DataListViewInfoTip(TObject *Sender,
    TListItem *Item, UnicodeString &InfoTip);
  void __fastcall HelpButtonClick(TObject *Sender);
private:
  TStoredSessionList *FSessionList;
  TConfiguration * FConfiguration;
  void __fastcall InitControls();
  void __fastcall UpdateControls();
  bool __fastcall GetCleanupData(TWinSCPData Data);

  INTERFACE_HOOK;

public:
  virtual __fastcall TCleanupDialog(TComponent* AOwner);
  __property TStoredSessionList *SessionList  = { read=FSessionList, write=FSessionList };
  __property TConfiguration * Configuration  = { read=FConfiguration, write=FConfiguration };
  __property Boolean CleanupData[TWinSCPData Data]  = { read=GetCleanupData };
};
//----------------------------------------------------------------------------
#endif
