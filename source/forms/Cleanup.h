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
  std::vector<UnicodeString> FCaptions;
  std::vector<UnicodeString> FLocations;
  typedef void __fastcall (__closure *TCleanupEvent)();
  std::vector<TCleanupEvent> FCleanupEvents;
  bool FAnyData;
  void __fastcall InitControls();
  void __fastcall UpdateControls();
  void __fastcall FindData();
  void __fastcall AddLocation(int CaptionId, const UnicodeString & Location, TCleanupEvent Event);
  void __fastcall AddRegistryLocation(int CaptionId, const UnicodeString & Location, TCleanupEvent Event);

  INTERFACE_HOOK

public:
  virtual __fastcall TCleanupDialog(TComponent * AOwner);
  bool __fastcall Execute();
  bool __fastcall AnyData();
};
//----------------------------------------------------------------------------
#endif
