//----------------------------------------------------------------------------
#ifndef CleanupH
#define CleanupH
//----------------------------------------------------------------------------
#include <vcl\System.hpp>
#include <vcl\Windows.hpp>
#include <vcl\SysUtils.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Graphics.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Buttons.hpp>
#include <vcl\ExtCtrls.hpp>
#include <ComCtrls.hpp>
//----------------------------------------------------------------------------
#include <Configuration.h>
#include <SessionData.h>
#include <WinInterface.h>
//---------------------------------------------------------------------
enum TWinSCPData {wdConfiguration = 1, wdStoredSessions, wdHostKeys,
  wdConfigurationIniFile, wdRandomSeedFile };
//---------------------------------------------------------------------
class TCleanupDialog : public TForm
{
__published:
  TButton *OKButton;
  TButton *CancelButton;
  TListView *DataListView;
  TLabel *Label1;
  TButton *CheckAllButton;
  void __fastcall DataListViewMouseDown(TObject *Sender,
    TMouseButton Button, TShiftState Shift, int X, int Y);
  void __fastcall DataListViewKeyUp(TObject *Sender, WORD &Key,
    TShiftState Shift);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall CheckAllButtonClick(TObject *Sender);
  void __fastcall DataListViewInfoTip(TObject *Sender,
    TListItem *Item, AnsiString &InfoTip);
private:
  TStoredSessionList *FSessionList;
  TConfiguration * FConfiguration;
  void __fastcall UpdateControls();
  void __fastcall SetCleanupData(TWinSCPData Data, Boolean value);
  Boolean __fastcall GetCleanupData(TWinSCPData Data);
public:
  virtual __fastcall TCleanupDialog(TComponent* AOwner);
  __property TStoredSessionList *SessionList  = { read=FSessionList, write=FSessionList };
  __property TConfiguration * Configuration  = { read=FConfiguration, write=FConfiguration };
  __property Boolean CleanupData[TWinSCPData Data]  = { read=GetCleanupData, write=SetCleanupData };
};
//----------------------------------------------------------------------------
#endif
